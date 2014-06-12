namespace FSharpVSPowerTools.HighlightUsage

open System
open System.IO
open System.Diagnostics
open System.Windows.Threading
open System.Collections.Generic
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
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
type HighlightUsageTagger(view: ITextView, buffer: ITextBuffer,
                          vsLanguageService: VSLanguageService, 
                          serviceProvider: IServiceProvider,
                          projectFactory: ProjectFactory) as self =
    let tagsChanged = Event<_, _>()
    let updateLock = obj()
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable currentWord = None
    let mutable requestedPoint = SnapshotPoint()

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate(currentRequest: SnapshotPoint, newSpans: NormalizedSnapshotSpanCollection, newWord: SnapshotSpan option) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                wordSpans <- newSpans
                currentWord <- newWord
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))

    let symbolUsesToSpans fileName (lastIdent: string) (symbolUses: FSharpSymbolUse []) =
        let filePath = Path.GetFullPathSafe(fileName)
        symbolUses
        |> Seq.choose (fun symbolUse -> 
            // We have to filter by full paths otherwise the range is invalid wrt current snapshot
            if Path.GetFullPathSafe(symbolUse.FileName) = filePath then
                fromFSharpRange view.TextSnapshot symbolUse.RangeAlternate
            else None)
        |> Seq.choose (fun span -> 
            // Sometimes F.C.S returns a composite identifier which should be truncated
            let index = span.GetText().LastIndexOf (lastIdent)
            if index > 0 then 
                Some (SnapshotSpan(view.TextSnapshot, span.Start.Position + index, span.Length - index))
            elif index = 0 then Some span
            else None)
        |> Seq.toList

    let doUpdate (currentRequest: SnapshotPoint, symbol, newWord: SnapshotSpan,
                  fileName: string, projectProvider: IProjectProvider) =
        async {
            if currentRequest = requestedPoint then
                try
                    let! res = vsLanguageService.GetFSharpSymbolUse (newWord, symbol, fileName, projectProvider, AllowStaleResults.MatchingSource)
                    match res with
                    | Some (_, checkResults) ->
                        let! results = vsLanguageService.FindUsagesInFile (newWord, symbol, checkResults)
                        let refSpans =
                            results |> Option.map (fun (_, lastIdent, refs) -> symbolUsesToSpans fileName lastIdent refs)

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
                    logException e
                    synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
            }

    let updateAtCaretPosition () =
        // If the new cursor position is still within the current word (and on the same snapshot),
        // we don't need to check it.
        match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
        | Some point, Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> ()
        | Some point, _ ->
            asyncMaybe {
                requestedPoint <- point
                let currentRequest = requestedPoint
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument() |> liftMaybe
                let! project = projectFactory.CreateForDocument buffer doc |> liftMaybe
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

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, 
                                    updateAtCaretPosition)

    let tagSpan span = TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>

    let getTags (spans: NormalizedSnapshotSpanCollection): ITagSpan<HighlightUsageTag> list = 
        [
            match currentWord with
            | Some word when spans.Count <> 0 && wordSpans.Count <> 0 -> 
                let currentSnapshot = spans.[0].Snapshot
                let wordSpans = 
                    if currentSnapshot = wordSpans.[0].Snapshot then
                        wordSpans
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        NormalizedSnapshotSpanCollection
                            (wordSpans |> Seq.map (fun span -> span.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)))
                
                let word = 
                    if currentSnapshot = word.Snapshot then word
                    else word.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)
                // First, yield back the word the cursor is under (if it overlaps)
                // Note that we'll yield back the same word again in the wordspans collection;
                // the duplication here is expected.
                if spans.OverlapsWith(NormalizedSnapshotSpanCollection word) then yield tagSpan word
                // Second, yield all the other words in the file
                for span in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpans) -> tagSpan span
            | _ -> ()
        ]

    interface ITagger<HighlightUsageTag> with
        member x.GetTags spans =
            upcast (
                try getTags spans
                with e -> 
                    Logging.logException e
                    [])
        
        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish
