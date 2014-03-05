namespace FSharpVSPowerTools.HighlightUsage

open System
open System.IO
open System.Collections.Generic
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx

type HighlightUsageTag() = 
    inherit TextMarkerTag("MarkerFormatDefinition/HighlightedReference")

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type HighlightUsageTagger(view: ITextView, sourceBuffer: ITextBuffer, textSearchService: ITextSearchService) as self =
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
                let span = SnapshotSpan(sourceBuffer.CurrentSnapshot, 0, sourceBuffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))

    let doUpdate (currentRequest: SnapshotPoint, newWord: SnapshotSpan, newWordSpans: SnapshotSpan seq, 
                  fileName: string, projectProvider: IProjectProvider) =
        async {
            if currentRequest = requestedPoint then
                try
                    let! res = 
                        VSLanguageService.findUsages newWord fileName projectProvider
                    let results =
                        res
                        |> Option.map (fun (_, lastIdent, _, refs) -> 
                            refs 
                            |> Seq.choose (fun symbolUse -> 
                                // We have to filter by full paths otherwise the range is invalid wrt current snapshot
                                if Path.GetFullPath(symbolUse.FileName) = Path.GetFullPath(fileName) then
                                    // Range01 type consists of zero-based values, which is a bit confusing
                                    Some (fromVSPos view.TextSnapshot symbolUse.Range)
                                else None)
                            |> Seq.map (fun span -> 
                                // Sometimes F.C.S returns a composite identifier which should be truncated
                                let index = span.GetText().LastIndexOf (lastIdent)
                                if index > 0 then 
                                    SnapshotSpan(view.TextSnapshot, span.Start.Position + index, span.Length - index)
                                else span)
                            |> Seq.toList)
                    return 
                        match results with
                        | Some references -> 
                            let possibleSpans = HashSet(newWordSpans)
                            let references = references |> List.filter possibleSpans.Contains
                            // Ignore symbols without any use
                            let word = if Seq.isEmpty references then None else Some newWord
                            synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection references, word)
                        | None ->
                            // Return empty values in order to clear up markers
                            synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
                with e ->
                debug "[Highlight Usage] %O exception occurs while updating." e
                return synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
            } |> Async.Start

    let updateWordAdornments() =
        let currentRequest = requestedPoint
        // Find all words in the buffer like the one the caret is on
        
        maybe {
            let! doc = Dte.getActiveDocument()
            let! project = ProjectCache.getProject doc
            let! newWord,_ = VSLanguageService.getSymbol currentRequest project
            // If this is the same word we currently have, we're done (e.g. caret moved within a word).
            match currentWord with
            | Some cw when cw = newWord -> ()
            | _ ->
                // Find the new spans
                let newSpans = 
                    FindData(newWord.GetText(), newWord.Snapshot, FindOptions = FindOptions.MatchCase)
                    |> textSearchService.FindAll
                // If we are still up-to-date (another change hasn't happened yet), do a real update
                doUpdate (currentRequest, newWord, newSpans, doc.FullName, project) }
        |> function
           | None -> synchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection(), None)
           | _ -> ()

    let updateAtCaretPosition(caretPosition: CaretPosition) =
        match sourceBuffer.GetSnapshotPoint caretPosition with
        | Some point ->
            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            match currentWord with
            | Some cw when cw.Snapshot = view.TextSnapshot && 
                           point.CompareTo cw.Start >= 0 &&
                           point.CompareTo cw.End <= 0 -> ()
            | _ ->
                requestedPoint <- point
                updateWordAdornments()
        | _ -> ()

    let viewLayoutChanged = 
        EventHandler<_>(fun _ (e: TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(view.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e: CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    do
        view.LayoutChanged.AddHandler(viewLayoutChanged)
        view.Caret.PositionChanged.AddHandler(caretPositionChanged)

    interface ITagger<HighlightUsageTag> with
        member x.GetTags (spans: NormalizedSnapshotSpanCollection): ITagSpan<HighlightUsageTag> seq =
            seq {
                match currentWord with
                | Some word when spans.Count <> 0 && wordSpans.Count <> 0 ->
                    let wordSpans, word =
                        match spans.[0].Snapshot = wordSpans.[0].Snapshot with
                        | true -> wordSpans, word
                        | false ->
                            // If the requested snapshot isn't the same as the one our words are on, translate our spans
                            // to the expected snapshot
                            NormalizedSnapshotSpanCollection (wordSpans |> Seq.map (fun span -> 
                                span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive))),
                            word.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)

                    // First, yield back the word the cursor is under (if it overlaps)
                    // Note that we'll yield back the same word again in the wordspans collection;
                    // the duplication here is expected.
                    if spans.OverlapsWith(NormalizedSnapshotSpanCollection word) then
                        yield TagSpan<HighlightUsageTag>(word, HighlightUsageTag()) :> ITagSpan<_>

                    // Second, yield all the other words in the file
                    for span in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpans) ->
                        TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>
                | _ -> ()
            }
        member x.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member x.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)
         
    interface IDisposable with
        member x.Dispose() = 
            view.LayoutChanged.RemoveHandler(viewLayoutChanged)
            view.Caret.PositionChanged.RemoveHandler(caretPositionChanged)
