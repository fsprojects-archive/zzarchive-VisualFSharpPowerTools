namespace FSharpVSPowerTools.HighlightUsage

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Windows.Media
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open EnvDTE
open VSLangProj
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharp.CompilerBinding

// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx

type HighlightUsageTag() = 
    inherit TextMarkerTag("MarkerFormatDefinition/HighlightedReference")

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type HighlightUsageTagger(v : ITextView, sb : ITextBuffer, ts : ITextSearchService, tn : ITextStructureNavigator) as self =
    let tagsChanged = Event<_, _>()
    let updateLock = obj()

    let mutable view = v
    let mutable sourceBuffer = sb
    let mutable textSearchService = ts
    let mutable _textStructureNavigator = tn
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable currentWord = None
    let mutable requestedPoint = SnapshotPoint()

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate(currentRequest : SnapshotPoint, newSpans : NormalizedSnapshotSpanCollection,
                          newWord : SnapshotSpan option) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                wordSpans <- newSpans
                currentWord <- newWord
                let span = SnapshotSpan(sourceBuffer.CurrentSnapshot, 0, sourceBuffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))

    let doUpdate(currentRequest : SnapshotPoint, newWord : SnapshotSpan, newWordSpans : SnapshotSpan seq, fileName : string, 
                 projectProvider : ProjectProvider) =
        async {
           if currentRequest = requestedPoint then
              try
                let! results = VSLanguageService.findUsages newWord fileName projectProvider view.TextSnapshot

                return 
                    match results with
                    | Some references -> 
                        let possibleSpans = HashSet(newWordSpans)
                        let references = references |> List.filter possibleSpans.Contains
                        // Ignore symbols without any use
                        let word = if Seq.isEmpty references then None else Some newWord
                        synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection references, word)
                    | None ->
                        // Return empty values in order to clear up markers
                        synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), None)
              with e ->
                Debug.WriteLine(sprintf "[Highlight Usage] %O exception occurs while updating." e)
                return synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), None)
        } |> Async.Start

    let updateWordAdornments() =
        let currentRequest = requestedPoint
        // Find all words in the buffer like the one the caret is on
        let doc = Dte.getActiveDocument()

        maybe {
            let! project = doc.Project
            let! newWord = VSLanguageService.getSymbol currentRequest project
            // If this is the same word we currently have, we're done (e.g. caret moved within a word).
            match currentWord with
            | Some cw when cw = newWord -> ()
            | _ ->
                // Find the new spans
                let findData = FindData(newWord.GetText(), newWord.Snapshot, FindOptions = FindOptions.MatchCase)
                let newSpans = textSearchService.FindAll(findData)
                // If we are still up-to-date (another change hasn't happened yet), do a real update
                doUpdate(currentRequest, newWord, newSpans, doc.FullName, project) }
        |> function
           | None -> synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), None)
           | _ -> ()

    let updateAtCaretPosition(caretPosition : CaretPosition) =
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
        EventHandler<_>(fun _ (e : TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(view.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e : CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    do
      view.LayoutChanged.AddHandler(viewLayoutChanged)
      view.Caret.PositionChanged.AddHandler(caretPositionChanged)

    interface ITagger<HighlightUsageTag> with
        member __.GetTags (spans : NormalizedSnapshotSpanCollection) : ITagSpan<HighlightUsageTag> seq =
            seq {
                match currentWord with
                | None -> ()
                | Some word ->
                    // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
                    // collection throughout
                    let newWord = ref word
                    let newWordSpans = ref wordSpans
                    if spans.Count = 0 || (!newWordSpans).Count = 0 then ()
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        if spans.[0].Snapshot <> (!newWordSpans).[0].Snapshot then
                            newWordSpans := 
                                NormalizedSnapshotSpanCollection(!newWordSpans |> Seq.map (fun span -> 
                                    span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)))
                            newWord := (!newWord).TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)
                        // First, yield back the word the cursor is under (if it overlaps)
                        // Note that we'll yield back the same word again in the wordspans collection;
                        // the duplication here is expected.
                        if spans.OverlapsWith(NormalizedSnapshotSpanCollection(!newWord)) then
                            yield TagSpan<HighlightUsageTag>(!newWord, HighlightUsageTag()) :> ITagSpan<_>

                        // Second, yield all the other words in the file
                        for span in NormalizedSnapshotSpanCollection.Overlap(spans, !newWordSpans) ->
                            TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>
            }
        member __.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member __.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)
         
    interface IDisposable with
        member __.Dispose() = 
            view.LayoutChanged.RemoveHandler(viewLayoutChanged)
            view.Caret.PositionChanged.RemoveHandler(caretPositionChanged)
