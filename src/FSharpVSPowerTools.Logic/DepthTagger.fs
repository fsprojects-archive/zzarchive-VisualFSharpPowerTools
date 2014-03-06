namespace FSharpVSPowerTools.DepthColorizer
   
open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Windows.Threading
open FSharpVSPowerTools.Core
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

// The tag that carries metadata about F# color-regions.
type DepthRegionTag(info: int * int * int * int) = 
    interface ITag
    // why are (line,startColumn,endColumn,depth) here, and not just depth?  
    // because we might have range info for indent coloring on a blank line, and there are no chars to tag there, so we put a tag in column 0 and carry all this info as metadata
    member x.Info = info

type DepthTagger(sourceBuffer: ITextBuffer, filename: string) as self = 
    // computed periodically on a background thread
    let mutable results: _ [] = null
    let resultsLock = obj() // lock for reading/writing "results"
    // only updated on the UI thread in the GetTags method
    let mutable prevTags: ITagSpan<DepthRegionTag> [] = null
    let mutable prevSnapshot = null
    let mutable prevResults = null
    // for the event
    let tagsChangedEvent = new Event<EventHandler<SnapshotSpanEventArgs>, SnapshotSpanEventArgs>()
    
    // was once useful for debugging
    let trace s = 
        let ticks = System.DateTime.Now.Ticks
        System.Diagnostics.Debug.WriteLine("{0}:{1}", ticks, s)
        ()
    
    let refreshFileImpl (doSync) = 
        async { 
            try 
                let syncContext = System.Threading.SynchronizationContext.Current
                let ss = sourceBuffer.CurrentSnapshot // this is the possibly-out-of-date snapshot everyone here works with
                if not doSync then do! Async.SwitchToThreadPool()
                do let sourceCodeOfTheFile = ss.GetText()
                   // Reuse the instance of InteractiveChecker
                   let ranges = 
                       DepthParser.GetNonoverlappingDepthRanges
                           (sourceCodeOfTheFile, filename, VSLanguageService.instance.Checker)
                   let tempResults = new ResizeArray<_>()
                   for (line, sc, ec, d) as info in ranges do
                       try 
                           System.Diagnostics.Debug.WriteLine("{0},{1},{2},{3}", line, sc, ec, d)
                           // -1 because F# reports 1-based line nums, whereas VS wants 0-based
                           let startLine = ss.GetLineFromLineNumber(Math.Min(line - 1, ss.LineCount - 1))
                           let start = startLine.Start.Add(Math.Min(sc, startLine.Length))
                           let endLine = ss.GetLineFromLineNumber(Math.Min(line - 1, ss.LineCount - 1))
                           let end_ = endLine.Start.Add(Math.Min(ec, endLine.Length))
                           let span = 
                               ss.CreateTrackingSpan
                                   ((new SnapshotSpan(start, end_)).Span, SpanTrackingMode.EdgeExclusive)
                           tempResults.Add(Tuple.Create(span, info))
                       with e -> 
                           System.Diagnostics.Debug.WriteLine(e)
                           if (System.Diagnostics.Debugger.IsAttached) then System.Diagnostics.Debugger.Break()
                   lock (resultsLock) (fun () -> 
                       results <- Array.create tempResults.Count (null, (0, 0, 0, 0))
                       tempResults.CopyTo(results))
                if not doSync then do! Async.SwitchToContext(syncContext)
                trace ("firing tagschanged")
                tagsChangedEvent.Trigger(self, new SnapshotSpanEventArgs(new SnapshotSpan(ss, 0, ss.Length)))
            with e -> 
                System.Diagnostics.Debug.WriteLine(e)
                if (System.Diagnostics.Debugger.IsAttached) then System.Diagnostics.Debugger.Break()
        }
        |> Async.StartImmediate
    
    do 
        // start an async loop on the UI thread that will re-parse the file and compute tags after idle time after a source change
        sourceBuffer.Changed.Add(fun _ -> trace ("source changed event"))
        let timeSpan = TimeSpan.FromMilliseconds(500.)
        
        let startNewTimer() = 
            let t = new DispatcherTimer(DispatcherPriority.ApplicationIdle, Interval = timeSpan)
            t.Start()
            t
        
        let rec awaitPauseAfterChange (timer: DispatcherTimer) = 
            async { 
                let! e = Async.EitherEvent(sourceBuffer.Changed, timer.Tick)
                match e with
                | Choice1Of2 _ -> 
                    timer.Stop()
                    do! awaitPauseAfterChange (startNewTimer())
                | _ -> ()
            }
        
        async { 
            while true do
                trace ("about to subscribe")
                let! _ = Async.AwaitEvent sourceBuffer.Changed
                do! awaitPauseAfterChange (startNewTimer())
                trace ("about to refresh")
                refreshFileImpl (false)
        }
        |> Async.StartImmediate
        // go ahead and synchronously get the first bit of info for the original rendering
        refreshFileImpl (true)
    
    interface ITagger<DepthRegionTag> with
        
        member x.GetTags(spans) = 
            let ss = spans.[0].Snapshot
            // note: accessing 'results' on line below outside the lock.  in theory we could have stale result cached and neglect to update,
            // but in practice this is extremely unlikely to happen, and is easily recoverable (e.g. by the user typing a new char into the buffer).
            if not (obj.ReferenceEquals(ss, prevSnapshot)) || not (obj.ReferenceEquals(prevResults, results)) then 
                trace ("computing fresh results")
                prevSnapshot <- ss
                lock (resultsLock) (fun () -> prevResults <- results)
                prevTags <- [| for span, depth in prevResults do
                                   yield upcast new TagSpan<DepthRegionTag>(span.GetSpan(ss), new DepthRegionTag(depth)) |]
            else trace ("using cached results")
            upcast prevTags
        
        [<CLIEvent>]
        member x.TagsChanged = tagsChangedEvent.Publish
