namespace FSharpVSPowerTools

open System
open System.Collections.Generic
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Utilities
open FSharpVSPowerTools.ProjectSystem

type UnusedDeclarationTag() =
    interface IGlyphTag

type UnusedDeclarationTagger(buffer: ITextBuffer, classifier: IClassifier) as self =     
    let unusedClassificationTag = "FSharp.Unused"
    let tagsChanged = Event<_,_>()

    let update () =
        async {
            // Wait for classification to finish
            do! Async.Sleep(200)
            let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
            return tagsChanged.Trigger(self, SnapshotSpanEventArgs(span))
        } 
        |> Async.StartInThreadPoolSafe        

    let docEventListener = new DocumentEventListener ([ViewChange.bufferChangedEvent buffer], 200us, update)

    interface ITagger<UnusedDeclarationTag> with
        member x.GetTags(spans: NormalizedSnapshotSpanCollection): ITagSpan<UnusedDeclarationTag> seq = 
            [|
                for span in spans do 
                    for classification in classifier.GetClassificationSpans(span) do
                        if classification.ClassificationType.Classification.Contains(unusedClassificationTag) then
                            yield TagSpan<_>(span, UnusedDeclarationTag()) :> ITagSpan<_>
            |] :> _
        
        [<CLIEvent>]
        member x.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>, _> = 
            tagsChanged.Publish
        
    interface IDisposable with
        member x.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
        