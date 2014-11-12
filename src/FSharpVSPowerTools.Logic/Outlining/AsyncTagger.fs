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
                |> ReadOnlyCollection.concat<Tuple<ITrackingSpan, 'Tag>>   
                                                   ( trackingCacheData.TrackingList )
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
        member self.GetCachedTags (snapshot:ITextSnapshot) =
            self.TrackingList
            |> ReadOnlyCollection.select
                ( fun tuple ->  
                    let itemSpan = tuple.Item1.GetSpanSafe(snapshot)
                    if itemSpan.IsSome 
                    then Some <| TagSpan<'Tag>( itemSpan.Value, tuple.Item2 ) 
                    else None )
            |> IEnumerable.where ( fun tagSpan -> tagSpan <> None )
            |> IEnumerable.toReadOnlyCollection


type TagCacheState =  Empty | Partial | Complete

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

    member self.Span 
        with get() =
            let range = self.VisitedCollection.OverarchingLineRange
            if range.IsNone then SnapshotSpan( self.Snapshot, 0, 0 ) else
            let lineRange = SnapshotLineRange( self.Snapshot                , 
                                               range.Value.StartLineNumber  , 
                                               range.Value.Count            )
            lineRange.ExtentIncludingLineBreak

    /// <summary>
    /// Determine tag cache state we have for the given SnapshotSpan
    /// </summary>
    member self.GetTagCacheState (span:SnapshotSpan) =
        // If the requested span doesn't even intersect with the overarching SnapshotSpan
        // of the cached data in the background then a more exhaustive search isn't needed
        // at this time
        let cachedSpan = self.Span
        if   not <| cachedSpan.IntersectsWith span 
        then TagCacheState.Empty 
        else 
            let lineRange = SnapshotLineRange.CreateForSpan span
            let unvisited = self.VisitedCollection.GetUnvisited lineRange.LineRange
            if   unvisited.IsSome 
            then TagCacheState.Partial 
            else TagCacheState.Complete
      
        
    /// <summary>
    /// Create a TrackingCacheData instance from this BackgroundCacheData
    /// </summary>
    member self.CreateTrackingCacheData() =
        let trackingList:ReadOnlyCollection<Tuple<ITrackingSpan, 'Tag>> = 
            self.TagList.Select ( fun tagSpan ->
                let snapshot     = tagSpan.Span.Snapshot
                let trackingSpan = snapshot.CreateTrackingSpan( tagSpan.Span.Span, SpanTrackingMode.EdgeExclusive )
                Tuple<ITrackingSpan, 'Tag>(trackingSpan, tagSpan.Tag) )
                |> IEnumerable.toReadOnlyCollection
        TrackingCacheData( self.Snapshot.CreateTrackingSpan( self.Span.Span , SpanTrackingMode.EdgeInclusive), trackingList )



[<Struct; NoComparison>]
type TagCache<'Tag when 'Tag :> ITag> =
    val BackgroundCacheData : BackgroundCacheData<'Tag> option
    val TrackingCacheData   : TrackingCacheData<'Tag>   option

    new ( backgroundCacheData, trackingCacheData ) =
        {   BackgroundCacheData = backgroundCacheData 
            TrackingCacheData   = trackingCacheData     }

    member x.IsEmpty 
        with get() =
            x.BackgroundCacheData.IsNone && x.TrackingCacheData.IsNone

    static member Empty = TagCache(None,None)



/// <summary>
/// Need another type here because SnapshotLineRange is a struct and we need atomic assignment
/// guarantees to use Interlocked.Exchange
/// </summary>
type TextViewLineRange (lineRange:SnapshotLineRange) =
    
    member __.LineRange with get() = lineRange

    static member Empty = TextViewLineRange(SnapshotLineRange())



/// <summary>
/// This type is used to support the one way transfer of SnapshotLineRange values between
/// the foreground thread of the tagger and the background processing thread.  It understands
/// the priority placed on the visible UI lines and will transfer those lines at a higher
/// priority than normal requests
/// </summary>
type Channel() as self =

    /// <summary>
    /// This is the normal request stack from the main thread.  More recently requested items
    /// are given higher priority than older items
    /// </summary>
    let mutable _stack = ReadOnlyStack<SnapshotLineRange>.Empty

    /// <summary>
    /// When set this is represents the visible line range of the text view.  It has the highest
    /// priority for the background thread
    /// </summary>
    let mutable _textViewLineRange = TextViewLineRange.Empty

    /// <summary>
    /// Version number tracks the number of writes to the channel
    /// </summary>
    let mutable _version = 0

    /// <summary>
    /// The current state of the request stack
    /// </summary>
    member __.CurrentStack with get() = _stack

    /// <summary>
    /// This number is incremented after every write to the channel.  It is a hueristic only and 
    /// not an absolute indicator.  It is not set atomically with every write but instead occurs
    /// some time after the write.  
    /// </summary>
    member __.CurrentVersion with get() = _version


    member __.WriteVisibleLines (lineRange:SnapshotLineRange) =
        let textViewLineRange = TextViewLineRange(lineRange)
        _textViewLineRange <- Interlocked.Exchange(ref _textViewLineRange, textViewLineRange)
        // TODO unsure if this is correct
        //Interlocked.Exchange(_textViewLineRange, textViewLineRange) |> ignore

        _version <- Interlocked.Increment(ref _version)
        // TODO unsure whether to reassign result or to ignore it
        // Interlocked.Increment(_version) |> ignore


    member __.WriteNormal (lineRange:SnapshotLineRange) =
        let compareStacks() =
            let oldStack = _stack
            let newStack = _stack.Push(lineRange)
            oldStack = Interlocked.CompareExchange(ref _stack, newStack, oldStack)

        let rec loop success =
            if   success  = true then () else
            let  success' = compareStacks()
            loop success'

        loop <| compareStacks()
        _version <- Interlocked.Increment(ref _version)

    member __.ReadVisibleLines() =
        let rec readLoop() =
            let oldTextViewLineRange = _textViewLineRange
            if  oldTextViewLineRange = TextViewLineRange.Empty then None else
            let success = 
                oldTextViewLineRange = Interlocked.CompareExchange( ref _textViewLineRange  , 
                                                                    TextViewLineRange.Empty , 
                                                                    oldTextViewLineRange    )
            if success = true then Some oldTextViewLineRange.LineRange
            else readLoop()
        readLoop()  


    member __.ReadNormal() =
        let rec readNormalLoop() =
            let oldStack = _stack
            if  oldStack =  ReadOnlyStack<SnapshotLineRange>.Empty then None else
            let newStack = _stack.Pop()
            let success  =
                oldStack = Interlocked.CompareExchange( ref _stack, newStack, oldStack )
            if success = true then Some oldStack.Value
            else readNormalLoop()
        readNormalLoop()



    member __.Read() =
        let lineRange = self.ReadVisibleLines()
        if   lineRange.IsSome then lineRange                
        else self.ReadNormal()


[<Struct>]
type AsyncBackgroundRequest =
    val Snapshot    : ITextSnapshot
    val Channel     : Channel


type CompleteReason = Finished | Cancelled | Error

