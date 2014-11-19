namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools

//module StarOutlining =
//
//    let startCom = "(*"
//    let endCom = "*)"
//    let ellipsis = "..."
//    let hoverText = "hover text"
//
//
//    type PartialRegion (parent:PartialRegion option, startline:int , startOffset:int, level:int) =
//        member __.Parent = parent
//        member __.StartLine = startline
//        member __.StartOffset = startOffset
//        member __.Level = level
//        static member Empty = PartialRegion (None,-1,-1,-1)
//
//    type Region (parent:PartialRegion option, startline:int , startOffset:int, level:int, endline:int) =
//        inherit PartialRegion(parent, startline, startOffset,level)
//        member __.EndLine = endline
//
//
//    let asSnapshotSpan (region:Region) (snapshot:ITextSnapshot) =
//        let startLine   = snapshot.GetLineFromLineNumber(region.StartLine)
//        let endLine     = if region.StartLine= region.EndLine 
//                            then startLine 
//                            else snapshot.GetLineFromLineNumber(region.EndLine)
//        SnapshotSpan(startLine.Start, endLine.End)
//
//
//
//    type StarTagger(buffer:ITextBuffer) as self =
//        
//        let mutable _snapshot = buffer.CurrentSnapshot
//        let _tagsChanged   = Event<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>() 
//        let mutable _regions: Region list = []
//       
//        do  ()
//            //self.Reparse()
//           
//            debug "Outlining Tagger was created"
//            buffer.Changed.Add(self.BufferChanged) 
//            ()
//            
//
//
//
//        member __.Buffer  with get() = buffer
//
//        member __.TagsChanged = _tagsChanged.Publish
//
//        member __.Snapshot 
//            with get() = _snapshot
//            and  set v = _snapshot <- v
//
//        member __.Regions 
//            with get() = _regions 
//            and  set v = _regions <- v
//
//        member __.BufferChanged( args:TextContentChangedEventArgs  ) =
//            if args.After <> self.Snapshot then () else
//            self.Reparse()
//
//
//        member __.Reparse() =
//            let newSnapshot  = buffer.CurrentSnapshot
//
//            let newRegions = new List<Region>()
//            let mutable currentRegion:PartialRegion = PartialRegion.Empty 
//
//            for line in newSnapshot.Lines do
//                let mutable regionStart = -1
//                let text = line.GetText()
//                regionStart <- text.IndexOf(startCom,StringComparison.Ordinal) 
//                if regionStart <> -1 then
//                    let currentLevel = if currentRegion <> PartialRegion.Empty then currentRegion.Level else 1
//                    let newLel
//                    
//
//
//            let oldSpans = 
//                self.Regions |> List.map ( fun r -> 
//                    ( asSnapshotSpan r self.Snapshot).TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive).Span)
//
//            let newSpans = 
//                newRegions |> List.map ( fun r -> 
//                    ( asSnapshotSpan r newSnapshot).Span)
//
//            let oldSpanCollection = NormalizedSpanCollection oldSpans
//            let newSpanCollection = NormalizedSpanCollection newSpans
//            let removed = NormalizedSpanCollection.Difference( oldSpanCollection, newSpanCollection )
//
//            let changeStart = if removed.Count  > 0 then removed.[0].Start               else Int32.MaxValue
//            let changeEnd   = if removed.Count  > 0 then removed.[removed.Count-1].End   else -1
//
//            let changeStart'= if newSpans.Count() > 0 then 
//                                Math.Min(changeStart, newSpans.ElementAt(0).Start ) else changeStart
//            let changeEnd'  = if newSpans.Count() > 0 then 
//                                Math.Max(changeEnd  , newSpans.ElementAt(newSpans.Count()-1).End ) else changeEnd
//
//            self.Snapshot <- newSnapshot
//            self.Regions  <- newRegions
//            let eventSpan = SnapshotSpan( self.Snapshot, Span.FromBounds(changeStart',changeEnd'))
//
//            if changeStart' <= changeEnd' then
//                _tagsChanged.Trigger(self, SnapshotSpanEventArgs eventSpan )
//
//
//
//
//
//        //interface ITagger<IOutliningRegionTag> with
//        interface ITagger<IOutliningRegionTag> with
//
//            member __.GetTags ( spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<IOutliningRegionTag>> = 
//                //_outliner.GetTags(spans)
//                //getTags spans self.Regions self.Snapshot
//
//    
//                seq {
//                        debug "trying to yield tags"
//                        if spans.Count = 0 then
//                            yield TagSpan(SnapshotSpan(),OutliningRegionTag() :> IOutliningRegionTag) :> ITagSpan<IOutliningRegionTag>
//
//                        let currentRegions, currentSnapshot = self.Regions, self.Snapshot
//                        let snapEntire = SnapshotSpan(spans.[0].Start, spans.[spans.Count-1].End).TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive )
//                        let startLineNum = snapEntire.Start.GetContainingLine().LineNumber
//                        let endLineNum   = snapEntire.End.GetContainingLine().LineNumber
//
//                        for rg in currentRegions do 
//                            if rg.StartLine <= endLineNum && rg.EndLine >= startLineNum then
//                                let startLine = currentSnapshot.GetLineFromLineNumber(rg.StartLine)
//                                let endLine = currentSnapshot.GetLineFromLineNumber(rg.EndLine)
//
//                                yield TagSpan<IOutliningRegionTag>
//                                            (   SnapshotSpan(startLine.Start , endLine.End) ,
//                                                OutliningRegionTag(false, false, collapsed, tooltip)) :> ITagSpan<IOutliningRegionTag>
//                    }
//
//
//            [<CLIEvent>]
//            member __.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
//                _tagsChanged.Publish
//  
//// #endregion
//        
//    [<Export(typeof<ITaggerProvider>)>]
//    [<TagType(typeof<IOutliningRegionTag>)>]
//    [<ContentType("F#")>]
//    type StarTaggerProvider() =
//        interface ITaggerProvider with
//
//            //member x.CreateTagger<'T when 'T :> ITag>(buffer: ITextBuffer): ITagger<'T> = 
//            member __.CreateTagger<'T when 'T :> ITag>(buffer: ITextBuffer): ITagger<'T>  = 
//                debug "trying to create star tagger"
//                let sc = Func<ITagger<'T>>( fun () -> new StarTagger(buffer) :> obj :?> ITagger<_> )
//                buffer.Properties.GetOrCreateSingletonProperty<ITagger<_>>(sc)
// 
//
//
