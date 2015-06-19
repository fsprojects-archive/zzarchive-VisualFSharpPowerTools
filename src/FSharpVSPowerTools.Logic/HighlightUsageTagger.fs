namespace FSharpVSPowerTools.HighlightUsage

open System
open System.IO
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open EditorUtils

// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx

type HighlightUsageTag() = 
    inherit TextMarkerTag("MarkerFormatDefinition/HighlightedReference")

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type HighlightUsageTagger(textDocument: ITextDocument,
                          view: ITextView, 
                          vsLanguageService: VSLanguageService, 
                          serviceProvider: IServiceProvider,
                          projectFactory: ProjectFactory) as self =
    let tagsChanged = DelegateEvent<EventHandler>()
    let updateLock = obj()
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable currentWord = None
    let mutable requestedPoint = SnapshotPoint()

    let buffer = view.TextBuffer

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate(currentRequest: SnapshotPoint, newSpans: NormalizedSnapshotSpanCollection, newWord: SnapshotSpan option) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                wordSpans <- newSpans
                currentWord <- newWord
                tagsChanged.Trigger [|self; EventArgs.Empty|])

    let symbolUsesToSpans (word: SnapshotSpan) fileName (lastIdent: string) (symbolUses: FSharpSymbolUse []) =
        let filePath = Path.GetFullPathSafe(fileName)
        symbolUses
        |> Seq.choose (fun symbolUse -> 
            // We have to filter by full paths otherwise the range is invalid wrt current snapshot
            if Path.GetFullPathSafe(symbolUse.FileName) = filePath then
                fromFSharpRange word.Snapshot symbolUse.RangeAlternate
            else None)
        |> fixInvalidSymbolSpans word.Snapshot lastIdent

    let doUpdate (currentRequest: SnapshotPoint, symbol, newWord: SnapshotSpan,
                  fileName: string, projectProvider: IProjectProvider) =
        async {
            if currentRequest = requestedPoint then
                try
                    let! res = vsLanguageService.GetFSharpSymbolUse (newWord, symbol, fileName, projectProvider, AllowStaleResults.No)
                    match res with
                    | Some (_, checkResults) ->
                        let! results = vsLanguageService.FindUsagesInFile (newWord, symbol, checkResults)
                        let refSpans =
                            results |> Option.map (fun (_, lastIdent, refs) -> symbolUsesToSpans newWord fileName lastIdent refs)

                        match refSpans with
                        | Some references -> 
                            // Ignore symbols without any use
                            let word = if Seq.isEmpty references then None else Some newWord
                            synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection references, word)
                        | None ->
                            // Return empty values in order to clear up markers
                            synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
                    | None -> synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
                with e ->
                    Logging.logExceptionWithMessage e "Failed to update highlight references."
                    synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
            }

    let updateAtCaretPosition () =
        Logging.logInfo "HUT(hash = %d).updateAtCaretPosition" (self.GetHashCode())
        // If the new cursor position is still within the current word (and on the same snapshot),
        // we don't need to check it.
        match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
        | Some point, Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> ()
        | Some point, _ ->
            asyncMaybe {
                requestedPoint <- point
                let currentRequest = requestedPoint
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetCurrentDocument(textDocument.FilePath)
                let! project = projectFactory.CreateForDocument buffer doc
                match vsLanguageService.GetSymbol(currentRequest, project) with
                | Some (newWord, symbol) ->
                    // If this is the same word we currently have, we're done (e.g. caret moved within a word).
                    match currentWord with
                    | Some cw when cw = newWord -> ()
                    | _ ->
                        // If we are still up-to-date (another change hasn't happened yet), do a real update
                        return! doUpdate (currentRequest, symbol, newWord, doc.FullName, project) |> liftAsync
                | None ->
                    return synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
            } 
            |> Async.map (Option.iter id)
            |> Async.StartInThreadPoolSafe
        | _ -> ()

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, 
                                                      updateAtCaretPosition)

    let tagSpan span = TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>

    let getTags (span: SnapshotSpan): ITagSpan<HighlightUsageTag> list = 
        [
            match currentWord with
            | Some word when wordSpans.Count <> 0 -> 
                let wordSpans = 
                    if span.Snapshot = wordSpans.[0].Snapshot then
                        wordSpans
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        NormalizedSnapshotSpanCollection
                            (wordSpans |> Seq.map (fun span -> span.TranslateTo(span.Snapshot, SpanTrackingMode.EdgeExclusive)))
                
                let word = 
                    if span.Snapshot = word.Snapshot then word
                    else word.TranslateTo(span.Snapshot, SpanTrackingMode.EdgeExclusive)
                // First, yield back the word the cursor is under (if it overlaps)
                if span.OverlapsWith word then yield tagSpan word
                // Second, yield all the other words in the file
                // Note that we won't yield back the same word again in the word spans collection;
                // the duplication is not expected.
                let spanCollection = NormalizedSnapshotSpanCollection span
                for span in NormalizedSnapshotSpanCollection.Overlap(spanCollection, wordSpans) do
                    if span <> word then yield tagSpan span
            | _ -> ()
        ]

    interface IBasicTaggerSource<HighlightUsageTag> with
        [<CLIEvent>]
        member __.Changed = tagsChanged.Publish
        member __.GetTags span = protectOrDefault (fun _ -> getTags span) [] |> Seq.toReadOnlyCollection
        member __.TextSnapshot: ITextSnapshot = textDocument.TextBuffer.CurrentSnapshot
            
    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose() 