namespace FSharpDepthColorizer

[<AutoOpen>]
module Utils =
    open System
    open System.Threading

    let synchronize f = 
        let ctx = SynchronizationContext.Current 
        let thread = 
            match ctx with
            | null -> null // saving a thread-local access
            | _ -> Thread.CurrentThread 

        f (fun g arg ->
            let nctx = SynchronizationContext.Current 
            match ctx, nctx with
            | null, _ -> g arg
            | _, _ when Object.Equals(ctx, nctx) && thread.Equals(Thread.CurrentThread) -> g arg
            | _ -> ctx.Post((fun _ -> g(arg)), null)  )

    type Microsoft.FSharp.Control.Async with 
        static member EitherEvent(ev1:IObservable<'a>, ev2:IObservable<'b>) = 
            synchronize (fun f ->
                Async.FromContinuations((fun (cont,econt,ccont) -> 
                    let rec callback1 = (fun value ->
                        remover1.Dispose()
                        remover2.Dispose()
                        f cont (Choice1Of2(value)) )
                    and callback2 = (fun value ->
                        remover1.Dispose()
                        remover2.Dispose()
                        f cont (Choice2Of2(value)) )
                    and remover1 : IDisposable  = ev1.Subscribe(callback1) 
                    and remover2 : IDisposable  = ev2.Subscribe(callback2) 
                    () )))

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Windows.Threading

// The tag that carries metadata about F# color-regions.
type FSharpRegionTag(info : int*int*int*int) =
    interface ITag
    // why are (line,startColumn,endColumn,depth) here, and not just depth?  
    // because we might have range info for indent coloring on a blank line, and there are no chars to tag there, so we put a tag in column 0 and carry all this info as metadata
    member this.Info = info

type FSharpTagger(sourceBuffer : ITextBuffer, filename : string) as this =
    // computed periodically on a background thread
    let mutable results : _[] = null
    let resultsLock = obj() // lock for reading/writing "results"

    // only updated on the UI thread in the GetTags method
    let mutable prevTags : ITagSpan<FSharpRegionTag>[] = null
    let mutable prevSnapshot = null
    let mutable prevResults = null

    // for the event
    let tagsChangedEvent = new Event<EventHandler<SnapshotSpanEventArgs>, SnapshotSpanEventArgs>()

    // was once useful for debugging
    let trace(s) = 
        //let ticks = System.DateTime.Now.Ticks 
        //System.Diagnostics.Debug.WriteLine("{0}:{1}", ticks, s)
        ()

    let RefreshFileImpl(doSync) =
        async {
            try
                let syncContext = System.Threading.SynchronizationContext.Current
                let ss = sourceBuffer.CurrentSnapshot   // this is the possibly-out-of-date snapshot everyone here works with
                if not doSync then
                    do! Async.SwitchToThreadPool()
                do
                    let sourceCodeOfTheFile = ss.GetText()
                    let ranges = MyParsing.GetNonoverlappingDepthRanges(sourceCodeOfTheFile, filename)
                    let tempResults = new ResizeArray<_>()
                    for (line,sc,ec,d) as info in ranges do
                        try
                            System.Diagnostics.Debug.WriteLine("{0},{1},{2},{3}", line, sc, ec, d)
                            // -1 because F# reports 1-based line nums, whereas VS wants 0-based
                            let startLine = ss.GetLineFromLineNumber(Math.Min(line - 1, ss.LineCount - 1))
                            let start = startLine.Start.Add(Math.Min(sc, startLine.Length))
                            let endLine = ss.GetLineFromLineNumber(Math.Min(line - 1, ss.LineCount - 1))
                            let end_ = endLine.Start.Add(Math.Min(ec, endLine.Length))
                            let span = ss.CreateTrackingSpan((new SnapshotSpan(start, end_)).Span, SpanTrackingMode.EdgeExclusive)
                            tempResults.Add(Tuple.Create(span, info))
                        with e ->
                            System.Diagnostics.Debug.WriteLine(e)
                            if (System.Diagnostics.Debugger.IsAttached) then
                                System.Diagnostics.Debugger.Break()
                    lock (resultsLock) (fun() ->
                        results <- Array.create tempResults.Count (null,(0,0,0,0))
                        tempResults.CopyTo(results)
                    )
                if not doSync then
                    do! Async.SwitchToContext(syncContext)
                trace("firing tagschanged")
                tagsChangedEvent.Trigger(this, new SnapshotSpanEventArgs(new SnapshotSpan(ss, 0, ss.Length)))
            with e ->
                System.Diagnostics.Debug.WriteLine(e)
                if (System.Diagnostics.Debugger.IsAttached) then
                    System.Diagnostics.Debugger.Break()
        } |> Async.StartImmediate 

    do
        // start an async loop on the UI thread that will re-parse the file and compute tags after idle time after a source change
        sourceBuffer.Changed.Add (fun _ -> trace("source changed event"))
        let timeSpan = TimeSpan.FromMilliseconds(500.)
        let startNewTimer() = let t = new DispatcherTimer(DispatcherPriority.ApplicationIdle, Interval=timeSpan) in t.Start(); t
        let rec awaitPauseAfterChange(timer:DispatcherTimer) = async {
            let! e = Async.EitherEvent(sourceBuffer.Changed, timer.Tick)
            match e with
            | Choice1Of2 _ -> timer.Stop()
                              do! awaitPauseAfterChange(startNewTimer())
            | _ -> ()
        }
        async {
            while true do
                trace("about to subscribe")
                let! _ = Async.AwaitEvent sourceBuffer.Changed
                do! awaitPauseAfterChange(startNewTimer())
                trace("about to refresh")
                RefreshFileImpl(false)
        } |> Async.StartImmediate 
        // go ahead and synchronously get the first bit of info for the original rendering
        RefreshFileImpl(true)

    interface ITagger<FSharpRegionTag> with
        member this.GetTags(spans) =
            let ss = spans.[0].Snapshot
            // note: accessing 'results' on line below outside the lock.  in theory we could have stale result cached and neglect to update,
            // but in practice this is extremely unlikely to happen, and is easily recoverable (e.g. by the user typing a new char into the buffer).
            if not(obj.ReferenceEquals(ss, prevSnapshot)) || not(obj.ReferenceEquals(prevResults, results)) then
                trace("computing fresh results")
                prevSnapshot <- ss
                lock (resultsLock) (fun() ->
                    prevResults <- results 
                )
                prevTags <- [|
                    for span,depth in prevResults do
                        yield upcast new TagSpan<FSharpRegionTag>(span.GetSpan(ss), new FSharpRegionTag(depth))
                    |]
            else
                trace("using cached results")
            upcast prevTags
        [<CLIEvent>]
        member this.TagsChanged = tagsChangedEvent.Publish 