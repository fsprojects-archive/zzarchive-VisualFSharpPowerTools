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
    let tagsChanged = Event<_, _>()
    let updateLock = obj()
    let mutable wordSpans = []
    let mutable currentWord: SnapshotSpan option = None
    let mutable requestedPoint = SnapshotPoint()

    let buffer = view.TextBuffer

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate (currentRequest, newSpans, newWord) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                wordSpans <- newSpans
                currentWord <- newWord
                let span = buffer.CurrentSnapshot.FullSpan
                tagsChanged.Trigger(self, SnapshotSpanEventArgs span))

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
                  fileName: string, projectProvider: IProjectProvider, CallInUIContext callInUIContext) =
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
                            let word = if List.isEmpty references then None else Some newWord
                            do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, references, word)
                        | None ->
                            // Return empty values in order to clear up markers
                            do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, [], None)
                    | None -> 
                        do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, [], None)
                with e ->
                    Logging.logExceptionWithContext(e, "Failed to update highlight references.")
                    do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, [], None)
        }

    let updateAtCaretPosition ((CallInUIContext callInUIContext) as ciuc) =
        asyncMaybe {
            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
            | Some point, Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> ()
            | Some point, _ ->
                requestedPoint <- point
                let currentRequest = requestedPoint
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetCurrentDocument(textDocument.FilePath)
                let! project = projectFactory.CreateForDocument buffer doc
                return!
                    match vsLanguageService.GetSymbol(currentRequest, doc.FullName, project) with
                    | Some (newWord, symbol) ->
                        // If this is the same word we currently have, we're done (e.g. caret moved within a word).
                        match currentWord with
                        | Some cw when cw = newWord -> async.Return None
                        | _ ->
                            // If we are still up-to-date (another change hasn't happened yet), do a real update
                            doUpdate (currentRequest, symbol, newWord, doc.FullName, project, ciuc) |> liftAsync
                    | None ->
                        callInUIContext <| fun _ -> 
                            synchronousUpdate (currentRequest, [], None)
                        |> liftAsync
            | None, _ -> ()
        } 
        |> Async.Ignore

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, 
                                                      updateAtCaretPosition)

    let tagSpan span = TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>

    let getTags (spans: NormalizedSnapshotSpanCollection): ITagSpan<HighlightUsageTag> list = 
        [
            match currentWord, wordSpans with
            | Some word, firstWordSpan :: _ when spans.Count > 0 -> 
                let currentSnapshot = spans.[0].Snapshot
                let wordSpans = 
                    if currentSnapshot = firstWordSpan.Snapshot then
                        NormalizedSnapshotSpanCollection wordSpans
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        NormalizedSnapshotSpanCollection
                            (wordSpans |> Seq.map (fun span -> span.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)))
                
                let word = 
                    if currentSnapshot = word.Snapshot then word
                    else word.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)
                // First, yield back the word the cursor is under (if it overlaps)
                if spans.OverlapsWith(NormalizedSnapshotSpanCollection word) then yield tagSpan word
                // Second, yield all the other words in the file
                // Note that we won't yield back the same word again in the word spans collection;
                // the duplication is not expected.
                for span in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpans) do
                    if span <> word then yield tagSpan span
            | _ -> ()
        ]

    interface ITagger<HighlightUsageTag> with
        member __.GetTags spans =
            upcast (protectOrDefault (fun _ -> getTags spans) [])
        
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = dispose docEventListener
