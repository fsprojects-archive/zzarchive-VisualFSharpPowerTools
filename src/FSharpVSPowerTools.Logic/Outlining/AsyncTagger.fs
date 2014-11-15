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
open System.Threading.Tasks
open System.Collections.ObjectModel
open FSharpVSPowerTools.Outlining.Extensions

  

[<Struct>][<NoComparison>] 
type TrackingCacheData<'Tag when 'Tag :> ITag> =
        
        val  TrackingSpan : ITrackingSpan  
        val  TrackingList : ReadOnlyCollection<Tuple<ITrackingSpan,'Tag>> 

        new ( trackingSpan,  trackingList ) =
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
        member self.GetCachedTags (snapshot:ITextSnapshot) : ReadOnlyCollection<ITagSpan<'Tag>> =
            self.TrackingList
            |> ReadOnlyCollection.map
                ( fun tuple ->  
                    let itemSpan = tuple.Item1.GetSpanSafe(snapshot)
                    if itemSpan.IsSome 
                    then Some <| TagSpan<'Tag>( itemSpan.Value, tuple.Item2 ) 
                    else None )
            |> IEnumerable.filter ( fun tagSpan -> tagSpan <> None )
            |> IEnumerable.map    ( fun tagSpan -> tagSpan.Value :> ITagSpan<'Tag>  )
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

    static member Empty = TagCache<'Tag>(None,None)



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


[<Struct>][<NoComparison>]
type AsyncBackgroundRequest =
    val Snapshot                : ITextSnapshot
    val Channel                 : Channel
    val Task                    : Task
    val CancellationTokenSource : CancellationTokenSource

    new ( snapshot, channel, task, cancellationTokenSource ) =
        {   Snapshot                = snapshot
            Channel                 = channel
            Task                    = task
            CancellationTokenSource = cancellationTokenSource   }        



type CompleteReason = Finished | Cancelled | Error


type AsyncTagger<'Data,'Tag when 'Tag :> ITag>
    ( asyncTaggerSource:IAsyncTaggerSource<'Data, 'Tag>) as self =

    let _subscriptions     = List<IDisposable>()
    let _asyncTaggerSource = asyncTaggerSource


    do  _asyncTaggerSource.Changed.Subscribe (self.OnAsyncTaggerSourceChanged) |> _subscriptions.Add

        // If there is an ITextView associated with the IAsyncTaggerSource then we want to 
        // listen to LayoutChanges.  If the layout changes while we are getting tags we want
        // to prioritize the visible lines
        if _asyncTaggerSource.TextViewOptional.IsSome then
            _asyncTaggerSource.TextViewOptional.Value.LayoutChanged.Subscribe(self.OnLayoutChanged)
                |> _subscriptions.Add




    /// <summary>
    /// <para> This number was chosen virtually at random.  In extremely large files it's legal    </para>
    /// <para> to ask for the tags for the entire file (and sadly very often done).  When this     </para>
    /// <para> happens even an async tagger breaks down a bit.  It won't cause the UI to hang      </para>
    /// <para> but it will appear the tagger is broken because it's not giving back any data. So   </para>
    /// <para> we break the non-visible sections into chunks and process the chunks one at a time  </para>
    /// <para>&#160;</para>
    /// <para> Note: Even though a section is not visible we must still provide tags.  Gutter </para>
    /// <para> margins and such still need to see tags for non-visible portions of the buffer </para>
    /// </summary>
    let DefaultChunkCount = 500


    /// <summary>
    /// Cached empty tag list
    /// </summary>
    static let EmptyTagList = ReadOnlyCollection<ITagSpan<'Tag>>(List<ITagSpan<'Tag>>())

    //private readonly IAsyncTaggerSource<TData, TTag> _asyncTaggerSource;
    let _tagsChanged = Event<SnapshotSpanEventArgs>()

    /// <summary>
    /// The one and only active AsyncBackgroundRequest instance.  There can be several
    /// in flight at once.  But we will cancel the earlier ones should a new one be 
    /// requested
    /// </summary>
    let mutable _asyncBackgroundRequest:AsyncBackgroundRequest option = None

    /// <summary>
    /// The current cache of tags we've provided to our consumer
    /// </summary>
    let mutable _tagCache = TagCache<'Tag>.Empty

    /// <summary>
    /// <para> The Overarching snapshot span for which we've received GetTags request.  At a glance </para>
    /// <para> it would appear that using a NormalizedLineRangeCollection would be more efficient   </para>
    /// <para> here as it more accurately tracks the ranges.  That is true, but we only use this    </para>
    /// <para> for the cases where IAsyncTaggerSource declares that it changes.  It's not incorrect </para>
    /// <para> to say the overarching SnapshotSpan has changed in this case.  And it's cheaper      </para>
    /// <para> to use the overaching span in the case of odd requests like time travel ones         </para>
    /// </summary>
    let mutable _cachedOverarchingRequestSpan = SnapshotSpan()

    let mutable _chunkCount = DefaultChunkCount;

    /// <summary>
    /// The SnapshotSpan for which we have given out tags
    /// </summary>
    member __.CachedOverarchingRequestSpan
        with get() = _cachedOverarchingRequestSpan
        and  set v = _cachedOverarchingRequestSpan <- v
        
    /// <summary>
    /// The cache of ITag values
    /// </summary>
    member __.TagCacheData
        with get() = _tagCache
        and  set v = _tagCache <- v

    /// <summary>
    /// If there is a background request active this holds the information about it 
    /// </summary>
    member __.AsyncBackgroundRequest
        with get() = _asyncBackgroundRequest
        and  set v = _asyncBackgroundRequest <- v

    member __.ChunkCount
        with get() = _chunkCount
        and  set v = _chunkCount <- v


    member __.RaiseTagsChanged(span:SnapshotSpan) =
        //let lineRange = SnapshotLineRange.CreateForSpan(span);
        //EditorUtilsTrace.TraceInfo("AsyncTagger::RaiseTagsChanged {0} - {1}", lineRange.StartLineNumber, lineRange.LastLineNumber);

        _tagsChanged.Trigger( SnapshotSpanEventArgs(span))
        

    /// <summary>
    /// Given a new tag list determine if the results differ from what we would've been 
    /// returning from our TrackingCacheData over the same SnapshotSpan.  Often times the 
    /// new data is the same as the old hence we don't need to produce any changed information
    /// to the buffer
    /// </summary>
    member  __.DidTagsChange (span:SnapshotSpan) (tagList:ReadOnlyCollection<ITagSpan<'Tag>> ) =
        if  _tagCache.TrackingCacheData.IsNone || 
            not <| _tagCache.TrackingCacheData.Value.ContainsCachedTags(span) then
            // Nothing in the tracking cache so it changed if there is anything in the new
            // collection.  If the new collection has anything then it changed
            tagList.Count > 0
        else
            let trackingCacheData = _tagCache.TrackingCacheData.Value
            let trackingTagList   = trackingCacheData.GetCachedTags(span.Snapshot)
            if  trackingTagList.Count <> tagList.Count then true else

            let trackingSet = 
                trackingTagList
                |> ReadOnlyCollection.map (fun tagSpan -> tagSpan.Span )
                |> IEnumerable.toHashSet();

            tagList.Any( fun x -> not <| trackingSet.Contains(x.Span))

    /// <summary>
    /// If the Layout changes while we are in the middle of getting tags we want to 
    /// prioritize the new set of visible lines.
    /// </summary>
    member __.OnLayoutChanged _ =
        if _asyncBackgroundRequest.IsNone || _asyncTaggerSource.TextViewOptional.IsNone then () else

        let visibleLineRange = _asyncTaggerSource.TextViewOptional.Value.GetVisibleSnapshotLineRange()
        let  asyncBackgroundRequest = _asyncBackgroundRequest.Value;
        if  visibleLineRange.IsSome && 
            visibleLineRange.Value.Snapshot = asyncBackgroundRequest.Snapshot then
            asyncBackgroundRequest.Channel.WriteVisibleLines(visibleLineRange.Value)





    /// <summary>
    /// Cancel the pending AsyncBackgoundRequest if one is currently running
    /// </summary>
    member __.CancelAsyncBackgroundRequest() =
        if _asyncBackgroundRequest.IsSome then
            // Use a try / with to protect the Cancel from throwing and taking down the process
            try let asyncBackgroundRequest = _asyncBackgroundRequest.Value
                if asyncBackgroundRequest.CancellationTokenSource.IsCancellationRequested = false then
                    asyncBackgroundRequest.CancellationTokenSource.Cancel()
            with
            |  exn -> ()
            _asyncBackgroundRequest <- None


    /// <summary>
    /// Called when the IAsyncTaggerSource raises a Changed event.  Clear out the 
    /// cache, pass on the event to the ITagger and wait for the next request
    /// </summary>
    member __.OnAsyncTaggerSourceChanged _ =
        // Clear out the cache.  It's no longer valid.
        _tagCache <- TagCache<_>.Empty
        self.CancelAsyncBackgroundRequest()

        // Now if we've previously had a SnapshotSpan requested via GetTags go ahead
        // and tell the consumers that it's changed.  Use the entire cached request
        // span here.  We're pessimistic when we have a Changed call because we have
        // no information on what could've changed
        if _cachedOverarchingRequestSpan.IsEmpty = false then
            self.RaiseTagsChanged(_cachedOverarchingRequestSpan);


    /// <summary>
    /// Is the async operation with the specified CancellationTokenSource the active 
    /// background request
    /// </summary>
    member __.IsActiveBackgroundRequest(cancellationTokenSource:CancellationTokenSource ) =
        _asyncBackgroundRequest.IsSome && 
        _asyncBackgroundRequest.Value.CancellationTokenSource = cancellationTokenSource


    [<UsedInBackgroundThread>]
    static member GetTagsInBackgroundCore
            ( asyncTaggerSource  : IAsyncTaggerSource<_, _>             )
            ( data               : 'Data                                       )
            ( chunkCount         : int                                         )
            ( channel            : Channel                                     )
            ( visited            : NormalizedLineRangeCollection               )
            ( cancellationToken  : CancellationToken                           )
            ( onComplete         : CompleteReason -> unit                      )
            ( onProgress         : SnapshotLineRange -> 
                                    ReadOnlyCollection<ITagSpan<'Tag>> -> unit ) =

        // TODO There's a good change this whole section is wrong, trying to adpat the do-while after getTags
        

        // Keep track of the LineRange values which we've already provided tags for.  Don't 
        // duplicate the work
        let toProcess = new Queue<SnapshotLineRange>()

        // *** This value can be wrong *** 
        // This is the version number we expect the Channel to have.  It's used
        // as a hueristic to determine if we should prioritize a value off of the stack or our
        // local stack.  If it's wrong it means we prioritize the wrong value.  Not a bug it
        // just changes the order in which values will appear
        let versionNumber = channel.CurrentVersion

        // Take one value off of the threadedLineRangeStack value.  If the value is bigger than
        // our chunking increment then we will add the value in chunks to the toProcess queue
        let popOne() =
            let value = channel.Read()
            if value.IsNone then () else

            let lineRange = value.Value

            if  lineRange.Count <= chunkCount 
            then toProcess.Enqueue(lineRange) else
                
            let snapshot = lineRange.Snapshot
            let startLineNumber = lineRange.StartLineNumber

            let rec enqueueLines startnum =
                if startLineNumber <= lineRange.LastLineNumber then
                    let startLine = snapshot.GetLineFromLineNumber(startLineNumber)
                    let localRange = SnapshotLineRange.CreateForLineAndMaxCount startLine chunkCount
                    toProcess.Enqueue(localRange)
                    enqueueLines (startnum + chunkCount)

            enqueueLines startLineNumber       

        // Get the tags for the specified SnapshotLineRange and return the results.  No chunking is done here,
        // the data is just directly processed
        let getTags (tagLineRange:SnapshotLineRange) =
            let unvisited = visited.GetUnvisited(tagLineRange.LineRange)
                
            if unvisited.IsSome then
                let tagList = 
                    try let tagLineRange' = 
                            (SnapshotLineRange.CreateForLineNumberRange tagLineRange.Snapshot 
                                                                        unvisited.Value.StartLineNumber 
                                                                        unvisited.Value.LastLineNumber  ).Value

                        asyncTaggerSource.GetTagsInBackground data tagLineRange'.ExtentIncludingLineBreak cancellationToken
                            :?> ReadOnlyCollection<ITagSpan<'Tag>>
                    with
                    |  exn -> ReadOnlyCollection( List<ITagSpan<'Tag>>() ) 
                               // :> IReadOnlyCollection<ITagSpan<'Tag>>
                        // Ignore exceptions that are thrown by IAsyncTaggerSource.  If the tagger threw then we consider
                        // the tags to be nothing for this span.  
                        //
                        // It's important that we register some value here.  If we register nothing then the foreground will
                        // never see this slot as fulfilled and later requests for this span will eventually queue up another
                        // background request
//                            EditorUtilsTrace.TraceInfo("AsyncTagger source exception in background processing {0}", e);
                visited.Add(tagLineRange.LineRange)
                onProgress tagLineRange tagList

        let rec getTagsLoop () =
            if   toProcess.Count = 0 then () 

            // If at any point the threadLineRangeStack value changes we consider the new values to have 
            // priority over the old ones
            elif versionNumber <> channel.CurrentVersion then () 
            else 
                cancellationToken.ThrowIfCancellationRequested()
                let lineRange = toProcess.Dequeue()
                getTags(lineRange)
                getTagsLoop()

        let rec loopQueue () =
            //let versionNumber = channel.CurrentVersion
            popOne()

            // We've drained both of the sources of input hence we are done
            if toProcess.Count = 0 then () else
            getTagsLoop()
            if cancellationToken.IsCancellationRequested = false then
                loopQueue()

        let completeReason =
            try loopQueue()
                match cancellationToken.IsCancellationRequested with
                | true  -> CompleteReason.Cancelled
                | false -> CompleteReason.Finished
            with
            | :? OperationCanceledException ->
                // Don't report cancellation exceptions.  These are thrown during cancellation for fast
                // break from the operation.  It's really a control flow mechanism
                CompleteReason.Cancelled
            | _ ->
                // Handle cancellation exceptions and everything else.  Don't want an errant 
                // exception thrown by the IAsyncTaggerSource to crash the process
//                EditorUtilsTrace.TraceInfo("AsyncTagger Exception in background processing {0}", e);
                CompleteReason.Error

        onComplete(completeReason)



    /// <summary>
    /// Called on the main thread when the request for tags has processed at least a small 
    /// section of the file.  This funtion may be called many times for a single background 
    /// request
    ///
    /// Called on the main thread
    /// </summary>
    member __.OnGetTagsInBackgroundProgress ( cancellationTokenSource:CancellationTokenSource)
                                            ( lineRange: SnapshotLineRange ) 
                                         //   ( tagList: ReadOnlyCollection<ITagSpan<ITag>>) =
                                            ( tagList: ReadOnlyCollection<ITagSpan<'Tag>> ) =
                                           // ( tagList: ReadOnlyCollection<ITagSpan<'Tag>> when 'Tag :> ITag) =
                                           //( tagList: ReadOnlyCollection<_> ) =
        if self.IsActiveBackgroundRequest(cancellationTokenSource) = false then ()

        // Merge the existing background data if it's present and on the same ITextSnapshot
        let newData  =
            if  _tagCache.BackgroundCacheData.IsSome && 
                _tagCache.BackgroundCacheData.Value.Snapshot = lineRange.Snapshot then
                let oldData = _tagCache.BackgroundCacheData.Value
                let tags    = oldData.TagList.Concat(tagList).ToReadOnlyCollection()
                oldData.VisitedCollection.Add(lineRange.LineRange)
                BackgroundCacheData(lineRange.Snapshot, oldData.VisitedCollection, tags)
            else
                BackgroundCacheData(lineRange, tagList)

        _tagCache <- TagCache(Some newData, _tagCache.TrackingCacheData)

        // Determine if the tags changed on the given Span.  In an edit it's very possible and likely
        // that the ITagSpan we returned by simply mapping the SnapshotSpan forward was correct.  If 
        // so then for a given SnapshotSpan we've already returned a result which was correct.  Raising
        // TagsChanged again for that SnapshotSpan will cause needless work to ocur (and potentially
        // more layouts
        let span = lineRange.ExtentIncludingLineBreak
        if self.DidTagsChange span (tagList) then self.RaiseTagsChanged(span)


    /// <summary>
    /// Called when the background request is completed
    ///
    /// Called on the main thread
    /// </summary>
    member __.OnGetTagsInBackgroundComplete ( reason  : CompleteReason  ) 
                                            ( channel : Channel         )
                                            ( cancellationTokenSource : CancellationTokenSource ) =

        if self.IsActiveBackgroundRequest(cancellationTokenSource) = false then () else

        // The request is complete.  Reset the active request information
        self.CancelAsyncBackgroundRequest()

        // Update the tag cache to indicate we are no longer doing any tracking edits
        _tagCache <- TagCache( _tagCache.BackgroundCacheData, None )

        // There is one race condition we must deal with here.  It is possible to get requests in the following
        // order 
        //
        //  - F GetTags span1
        //  - B Process span1 
        //  - B Complete span1
        //  - F GetTags span2 (adds to existing queue)
        //  - F Get notified that background complete
        //
        // The good news is any data that is missed will still be in threadedLineRangeStack.  So we just need to
        // drain this value and re-request the data 
        //
        // We own the stack at this point so just access it directly
        let stack = channel.CurrentStack
        if stack.IsEmpty = false && reason = CompleteReason.Finished then
            let list = new List<SnapshotSpan>()

            let rec stackLoop ( stack : ReadOnlyStack<SnapshotLineRange> ) =
                if stack.IsEmpty then () else
                self.GetTagsInBackground stack.Value.ExtentIncludingLineBreak
                stackLoop <| stack.Pop()
            
            stackLoop stack
                        


    /// <summary>
    /// The background processing is now focussed on the given ITextSnapshot.  If anything is 
    /// focused on the old ITextSnapshot move it to the specified one.
    /// </summary>
    member __.AdjustToSnapshot(snapshot:ITextSnapshot ) =
        // First check and see if we need to move the existing background data to tracking data
        if  _tagCache.BackgroundCacheData.IsSome && 
            _tagCache.BackgroundCacheData.Value.Snapshot <> snapshot then
            let backgroundCacheData = _tagCache.BackgroundCacheData.Value
            let trackingCacheData = 
                let createdTrackingCacheData = backgroundCacheData.CreateTrackingCacheData()
                if _tagCache.TrackingCacheData.IsSome then
                    createdTrackingCacheData.Merge(snapshot, _tagCache.TrackingCacheData.Value)
                else 
                    createdTrackingCacheData
            _tagCache <- TagCache(None, Some trackingCacheData);

        // Next cancel any existing background request if it's not focused on this ITextSnapshot
        if  _asyncBackgroundRequest.IsSome && 
            _asyncBackgroundRequest.Value.Snapshot <> snapshot then
            self.CancelAsyncBackgroundRequest()


    member __.GetTagsInBackground( span:SnapshotSpan ) =
        let synchronizationContext = SynchronizationContext.Current
        if  synchronizationContext = null then () else

        // The background processing should now be focussed on the specified ITextSnapshot 
        let snapshot = span.Snapshot

        // In the majority case GetTags(NormalizedSnapshotCollection) drives this function and 
        // AdjustToSnapshot is already called.  There are other code paths though within AsyncTagger
        // which call this method.  We need to guard against them here.  
        self.AdjustToSnapshot(snapshot)

        // Our caching and partitioning of data is all done on a line range
        // basis.  Just expand the requested SnapshotSpan to the encompassing
        // SnaphotlineRange
        let lineRange = SnapshotLineRange.CreateForSpan(span)
        let span = lineRange.ExtentIncludingLineBreak

        // If we already have a background task running for this ITextSnapshot then just enqueue this 
        // request onto that existing one.  By this point if the request exists it must be tuned to 
        // this ITextSnapshot
        if _asyncBackgroundRequest.IsSome then
            let asyncBackgroundRequest = _asyncBackgroundRequest.Value
            if asyncBackgroundRequest.Snapshot = snapshot then
                
                //Contract.Requires(asyncBackgroundRequest.Snapshot == snapshot);
                //    EditorUtilsTrace.TraceInfo("AsyncTagger Background Existing {0} - {1}", lineRange.StartLineNumber, lineRange.LastLineNumber);
                asyncBackgroundRequest.Channel.WriteNormal(lineRange)
            else  self.CancelAsyncBackgroundRequest()

//            Contract.Assert(!_asyncBackgroundRequest.HasValue);
//            EditorUtilsTrace.TraceInfo("AsyncTagger Background New {0} - {1}", lineRange.StartLineNumber, lineRange.LastLineNumber);

            // Create the data which is needed by the background request
        let data = _asyncTaggerSource.GetDataForSnapshot(snapshot)
        let cancellationTokenSource = new CancellationTokenSource()
        let cancellationToken = cancellationTokenSource.Token
        let channel = new Channel()
        channel.WriteNormal(lineRange)

        // If there is an ITextView then make sure it is requested as well.  If the source provides an 
        // ITextView then it is always prioritized on requests for a new snapshot
        if _asyncTaggerSource.TextViewOptional.IsSome then
            let visibleLineRange = _asyncTaggerSource.TextViewOptional.Value.GetVisibleSnapshotLineRange()
            if visibleLineRange.IsSome then
                channel.WriteVisibleLines(visibleLineRange.Value)

        // The background thread needs to know the set of values we have already queried 
        // for.  Send a copy since this data will be mutated from a background thread
        let localVisited : NormalizedLineRangeCollection =
            if _tagCache.BackgroundCacheData.IsSome then
                let backgroundCacheData = _tagCache.BackgroundCacheData.Value
                // TODO Investigate the necessity of this Contract
                //Contract.Requires(backgroundCacheData.Snapshot == snapshot);
                backgroundCacheData.VisitedCollection.Copy()
            else
                NormalizedLineRangeCollection()

        // Function which finally gets the tags.  This is run on a background thread and can
        // throw as the implementor is encouraged to use CancellationToken::ThrowIfCancelled
        let localAsyncTaggerSource = _asyncTaggerSource
        let localChunkCount = _chunkCount

        let onComplete (completeReason:CompleteReason) =
            synchronizationContext.Post 
                ( ( fun _ -> self.OnGetTagsInBackgroundComplete 
                                completeReason   channel    cancellationTokenSource ) , None )

        let onProgress (processedLineRane:SnapshotLineRange) (tagList:ReadOnlyCollection<ITagSpan<'Tag>>) =
             synchronizationContext.Post 
                ( ( fun _ -> self.OnGetTagsInBackgroundProgress
                                cancellationTokenSource processedLineRane tagList) , None )
            
        let getTags() =
            AsyncTagger.GetTagsInBackgroundCore localAsyncTaggerSource  data
                                                localChunkCount         channel
                                                localVisited            cancellationToken
                                                onComplete              onProgress
        

                    

        // Create the Task which will handle the actual gathering of data.  If there is a delay
        // specified use it
        let localDelay = _asyncTaggerSource.Delay

        let taskAction() =
                if localDelay.IsSome then Thread.Sleep(localDelay.Value)
                getTags()

        let task = new Task(taskAction, cancellationToken)
        _asyncBackgroundRequest <- 
            Some <| AsyncBackgroundRequest( span.Snapshot           ,
                                            channel                 ,
                                            task                    ,
                                            cancellationTokenSource )

        task.Start()


    /// <summary>
    /// Try and get the tags promptly from the IAsyncTaggerSource
    /// </summary>
    member __.TryGetTagsPrompt (span:SnapshotSpan)( tagList:IEnumerable<ITagSpan<'Tag>> ref ) =
        _asyncTaggerSource.TryGetTagsPrompt span tagList

    member __.TryGetTagsFromBackgroundDataCache (span:SnapshotSpan) (tagList:IEnumerable<ITagSpan<'Tag>> ref ) =
        if _tagCache.BackgroundCacheData.IsNone || 
            _tagCache.BackgroundCacheData.Value.Snapshot <> span.Snapshot then
            tagList := EmptyTagList :> IEnumerable<ITagSpan<'Tag>>
            false
        else
            let backgroundCacheData = _tagCache.BackgroundCacheData.Value
            match backgroundCacheData.GetTagCacheState(span) with
            | TagCacheState.Complete -> tagList := backgroundCacheData.TagList :> IEnumerable<ITagSpan<'Tag>>
                                        true
            | TagCacheState.Partial  -> tagList := backgroundCacheData.TagList :> IEnumerable<ITagSpan<'Tag>>
                                        false
            | TagCacheState.Empty    -> tagList := EmptyTagList :> IEnumerable<ITagSpan<'Tag>>
                                        false

    member __.GetTagsFromTrackingDataCache( snapshot:ITextSnapshot ) =
        if _tagCache.TrackingCacheData.IsNone then
            EmptyTagList
        else
            let trackingCacheData = _tagCache.TrackingCacheData.Value
            trackingCacheData.GetCachedTags(snapshot)





                
    member __.AdjustRequestSpan( col:NormalizedSnapshotSpanCollection) =
        if col.Count > 0 then
            // Note that we only use the overarching span to track what data we are responsible 
            // for in a
            let requestSpan = col.GetOverarchingSpan()
            _cachedOverarchingRequestSpan<- TaggerUtil.AdjustRequestedSpan( Some _cachedOverarchingRequestSpan)  
                                                                            requestSpan




    member __.GetTags(span:SnapshotSpan) =
       // let lineRange = SnapshotLineRange.CreateForSpan(span);
        //EditorUtilsTrace.TraceInfo("AsyncTagger::GetTags {0} - {1}", lineRange.StartLineNumber, lineRange.LastLineNumber);

        // First try and see if the tagger can provide prompt data.  We want to avoid 
        // creating Task<T> instances if possible.  
        
        // ref to store the result from TryGetTagsPromt
        let cacheList:ReadOnlyCollection<ITagSpan<'Tag>> ref = ref EmptyTagList 
        // TODO - this seems like some very risky mutation business 
        let tryPrompt, cacheList = self.TryGetTagsPrompt span  (!cacheList :> IEnumerable<ITagSpan<'Tag>> |> ref)
        let tryBackground = self.TryGetTagsFromBackgroundDataCache span cacheList
        let tagList = 
            if  tryPrompt = true || tryBackground = true then 
                !cacheList |> IEnumerable.toReadOnlyCollection
            else
                // The request couldn't be fully satisfied from the cache or prompt data so
                // we will request the data in the background thread
                self.GetTagsInBackground(span)

                // Since the request couldn't be fully fulfilled by the cache we augment the returned data
                // with our tracking data
                let trackingTagList = self.GetTagsFromTrackingDataCache span.Snapshot
                trackingTagList 

        // Now filter the set of returned ITagSpan values to those which are part of the 
        // requested NormalizedSnapshotSpanCollection.  The cache lookups don't dig down and 
        // instead return all available tags.  We filter down the collection here to what's 
        // necessary.
        tagList |> IEnumerable<ITagSpan<'Tag>>.filter (fun tagSpan -> tagSpan.Span.IntersectsWith(span))
                |> IEnumerable.toReadOnlyCollection



    /// <summary>
    /// Get the tags for the specified NormalizedSnapshotSpanCollection.  Use the cache if 
    /// possible and possibly go to the background if necessary
    /// </summary>
    member __.GetTags( col:NormalizedSnapshotSpanCollection) =
        // The editor itself will never send an empty collection to GetTags.  But this is an 
        // API and other components are free to call it with whatever values they like
        if col.Count = 0 then EmptyTagList else

        self.AdjustRequestSpan(col)
        self.AdjustToSnapshot (col.[0].Snapshot)

        if col.Count = 1 then self.GetTags(col.[0]) else

        //EditorUtilsTrace.TraceInfo("AsyncTagger::GetTags Count {0}", col.Count);
        let  allTags:seq<ITagSpan<'Tag>> = Seq.empty

        let allTags' = col.Aggregate( allTags, (fun (acc:seq<ITagSpan<'Tag>>) x -> acc.Concat(self.GetTags(x))))
        allTags'.ToReadOnlyCollection()


    member __.Dispose() =
        _subscriptions.ForEach( fun (x:IDisposable) -> x.Dispose()) 
        _subscriptions.Clear()

    interface IDisposable with
        member __.Dispose() = self.Dispose()
