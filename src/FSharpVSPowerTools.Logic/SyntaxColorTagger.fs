namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.Collections.Generic
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem


// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx
type TypeColorTag() = 
    inherit TextMarkerTag("SymbolDefinitionClassificationFormat")

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx
type SyntaxColorTagger(view: ITextView,
                       sourceBuffer: ITextBuffer,
                       textSearchService: ITextSearchService) as self =
    let tagsChanged = Event<_, _>()

    let lockObject = new Object()
    let mutable wordSpans = NormalizedSnapshotSpanCollection()

    let synchronousUpdate (newSpans: NormalizedSnapshotSpanCollection) =
        lock
            lockObject
            (fun () ->
                wordSpans <- newSpans

                tagsChanged.Trigger(self, 
                                    new SnapshotSpanEventArgs(
                                        new SnapshotSpan(
                                            sourceBuffer.CurrentSnapshot,
                                            0,
                                            sourceBuffer.CurrentSnapshot.Length))))

    let caretPositionChangedHandler =
        let handler e f =
            let snapshot = sourceBuffer.CurrentSnapshot
//            let text = snapshot.GetText()
            //Find the new spans
            let mutable findData = new FindData("int", snapshot)
            findData.FindOptions <- FindOptions.WholeWord ||| FindOptions.MatchCase

            let wordSpans = new NormalizedSnapshotSpanCollection(textSearchService.FindAll(findData))
            synchronousUpdate wordSpans

        EventHandler<_>(handler)

    do
        view.Caret.PositionChanged.AddHandler(caretPositionChangedHandler)

    interface ITagger<TypeColorTag> with
        member x.GetTags (spans: NormalizedSnapshotSpanCollection): seq<ITagSpan<TypeColorTag>> =
            seq {
                // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
                // collection throughout
                if (spans.Count > 0 && wordSpans.Count > 0) then
                    let wordSpansSnapshot =
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans to the expected snapshot 
                        if spans.[0].Snapshot <> wordSpans.[0].Snapshot then
                            new NormalizedSnapshotSpanCollection(
                                wordSpans |> Seq.map (fun span -> span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)))
                        else
                            wordSpans

                    // Second, yield all the other words in the file 
                    for (span: SnapshotSpan) in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpansSnapshot) do
                        yield (new TagSpan<_>(span, new TypeColorTag()) :> ITagSpan<_>)
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish