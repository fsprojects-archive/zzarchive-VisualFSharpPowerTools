namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools
open System.Threading
open System.Collections.ObjectModel
open FSharpVSPowerTools.Outlining.Extensions

type CompleteReason =
    | Finished
    | Cancelled
    | Error



[<Struct>][<NoComparison>]
/// <summary>
/// This holds the set of data which is currently known from the background thread.  Data in 
/// this collection should be considered final for the given Snapshot.  It will only change
/// if the AsyncTaggerSource itself raises a Changed event (in which case we discard all 
/// background data).  
/// </summary>
type BackgroundCacheData<'Tag when 'Tag :> ITag> =
    val Snapshot : ITextSnapshot
    /// <summary>
    /// Set of line ranges for which tags are known
    /// </summary>
    val VisitedCollection : NormalizedLineRangeCollection
    /// <summary>
    /// Set of known tags
    /// </summary>
    val TagList : ReadOnlyCollection<ITagSpan<'Tag>> 

    new ( snapshot, visitedCollection, tagList ) =
        {   Snapshot            = snapshot
            VisitedCollection   = visitedCollection
            TagList             = tagList           }

    new ( lineRange:SnapshotLineRange, tagList ) =
        let snapshot = lineRange.Snapshot
        let visitedCollection = NormalizedLineRangeCollection()
        visitedCollection.Add lineRange.LineRange
        BackgroundCacheData<'Tag>(snapshot, visitedCollection, tagList )


type ROC<'T> = ReadOnlyCollection<'T>

[<Struct>][<NoComparison>] 
type TrackingCacheData<'Tag when 'Tag :> ITag> =
        
        val  TrackingSpan : ITrackingSpan  
        val  TrackingList : ReadOnlyCollection<Tuple<ITrackingSpan,'Tag>> 

        new( trackingSpan,  trackingList ) =
            {   TrackingSpan = trackingSpan
                TrackingList = trackingList  }

        member self.Merge( snapshot:ITextSnapshot,  trackingCacheData:TrackingCacheData<'Tag> ) =
            let left  = self.TrackingSpan.GetSpanSafe(snapshot)
            let right = trackingCacheData.TrackingSpan.GetSpanSafe(snapshot)
       
            let span = 
                match left, right with
                | Some l, Some r -> SnapshotSpan.CreateOverarching l r
                | Some l, None   -> l
                | None  , Some r -> r
                | None  , None   -> SnapshotSpan(snapshot, 0, 0)

            let trackingSpan = 
                snapshot.CreateTrackingSpan(span.Span, SpanTrackingMode.EdgeInclusive )
            
            let equalityTool = 
                EqualityUtility.Create<Tuple<ITrackingSpan, 'Tag>>
                    ( fun (x:Tuple<ITrackingSpan, 'Tag>) 
                          (y:Tuple<ITrackingSpan, 'Tag>)   -> 
                                x.Item1.GetSpanSafe(snapshot) = y.Item1.GetSpanSafe(snapshot))
                    ( fun tuple -> tuple.Item1.GetSpanSafe(snapshot).GetHashCode())
                :> IEqualityComparer<Tuple<ITrackingSpan, 'Tag>>                     

            let tagList =
                self.TrackingList 
                |> ROC.concat<Tuple<ITrackingSpan, 'Tag>>   ( trackingCacheData.TrackingList )
                |> IEnumerable.distinct<Tuple<ITrackingSpan, 'Tag>>  ( equalityTool )
                |> Seq.toReadOnlyCollection

            TrackingCacheData( trackingSpan, tagList )
            /// <summary>
            /// Does this tracking information contain tags over the given span in it's 
            /// ITextSnapshot
            /// </summary>
        member self.ContainsCachedTags(span:SnapshotSpan ) : bool =
                let snapshot     = span.Snapshot;
                let trackingSpan = self.TrackingSpan.GetSpanSafe(snapshot);
                trackingSpan.IsSome

            /// <summary>
            /// Get the cached tags on the given ITextSnapshot
            ///
            /// If this SnapshotSpan is coming from a different snapshot which is ahead of 
            /// our current one we need to take special steps.  If we simply return nothing
            /// and go to the background the tags will flicker on screen.  
            ///
            /// To work around this we try to map the tags to the requested ITextSnapshot. If
            /// it succeeds then we use the mapped values and simultaneously kick off a background
            /// request for the correct ones
            /// </summary>
//            internal ReadOnlyCollection<ITagSpan<TTag>> GetCachedTags(ITextSnapshot snapshot)
//            {
                // Mapping gave us at least partial information.  Will work for the transition
                // period
//                return
//                    TrackingList
//                    .Select(
//                        tuple =>
//                        {
//                            var itemSpan = tuple.Item1.GetSpanSafe(snapshot);
//                            return itemSpan.HasValue
//                                ? (ITagSpan<TTag>)new TagSpan<TTag>(itemSpan.Value, tuple.Item2)
//                                : null;
//                        })
//                    .Where(tagSpan => tagSpan != null)
//                    .ToReadOnlyCollection();
//            }
//        }





//type TagCacheState =
//    | None
//    | Partial
//    | Complete
//
//[<Struct; NoComparison>]
//type TagCache =
//    val BackgroundCacheData : BackgroundCacheData option
//    val TrackingCacheData   : TrackingCacheData   option
//
//    new ( backgroundCacheData, trackingCacheData ) =
//        {   BackgroundCacheData = backgroundCacheData 
//            TrackingCacheData   = trackingCacheData     }
//
//    member x.IsEmpty 
//        with get() =
//            x.BackgroundCacheData = None  && 
//            x.TrackingCacheData   = None
//
//    static member Empty = TagCache(None,None)
