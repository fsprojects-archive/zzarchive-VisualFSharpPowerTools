namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.Collections.ObjectModel
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.Outlining.Extensions



type IBasicTaggerSource<'Tag when 'Tag :> ITag> =
    abstract GetTags : span:SnapshotSpan -> ReadOnlyCollection<ITagSpan<'Tag>>
    abstract Changed : IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>


type BasicTagger<'Tag when 'Tag :> ITag> (basicTaggerSource:IBasicTaggerSource<'Tag>) as self =

    let _subscriptions = List<IDisposable>()

    let _basicTaggerSource = basicTaggerSource
    let mutable _cachedRequestSpan : SnapshotSpan option = None
    let _tagsChanged = Event<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>()

    do 
        _basicTaggerSource.Changed.Subscribe(self.OnBasicTaggerSourceChanged)  |> _subscriptions.Add

    member __.TagsChanged = _tagsChanged.Publish


    member __.OnBasicTaggerSourceChanged _  =
        //let tags
        _tagsChanged.Trigger(self, new SnapshotSpanEventArgs(_cachedRequestSpan.Value))


    member __.AdjustRequestSpan(col:NormalizedSnapshotSpanCollection ) =
        if (col.Count > 0) then
            let requestSpan = col.GetOverarchingSpan()
            _cachedRequestSpan <- Some <| TaggerUtil.AdjustRequestedSpan _cachedRequestSpan requestSpan 


    member __.GetTags ( col:NormalizedSnapshotSpanCollection) =
        self.AdjustRequestSpan(col)
        if col.Count = 0 then Enumerable.Empty<ITagSpan<'Tag>>() else

        // Even though it's easier don't do a GetTags request for the overarching SnapshotSpan
        // of the request.  It's possible for the overarching SnapshotSpan to have an order
        // magnitudes more lines than the items in the collection.  This is very possible when
        // large folded regions or on screen.  Instead just request the individual ones
        if col.Count = 1 then _basicTaggerSource.GetTags(col.[0]) :> IEnumerable<ITagSpan<'Tag>>
        
        else  
            col |> IEnumerable.collect 
                    ( fun x -> (_basicTaggerSource.GetTags x) :> IEnumerable<ITagSpan<'Tag>>)

    member __.Dispose() =
        _subscriptions.ForEach ( fun x -> x.Dispose() ) 
        _subscriptions.Clear()
        
//                _basicTaggerSource.Changed -= OnBasicTaggerSourceChanged;
//            var disposable = _basicTaggerSource as IDisposable;
//            if (disposable != null)
//            {
//                disposable.Dispose();
//            }

    interface ITagger<'Tag> with

        member __.GetTags(spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<'Tag>> = 
            self.GetTags(spans)
        
        [<CLIEvent>]
        member __.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
            self.TagsChanged
        
    interface IDisposable with
        member __.Dispose(): unit = 
            self.Dispose()
        




  