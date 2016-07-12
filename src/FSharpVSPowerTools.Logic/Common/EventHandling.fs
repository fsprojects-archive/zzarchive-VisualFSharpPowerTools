namespace FSharpVSPowerTools.ProjectSystem

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.Threading
open System.Windows.Threading
open FSharpVSPowerTools
open System.Diagnostics
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module ViewChange =    
    open Microsoft.VisualStudio.Text.Tagging

    let layoutEvent (view: ITextView) = 
        view.LayoutChanged |> Event.choose (fun e -> if e.NewSnapshot <> e.OldSnapshot then Some() else None)
    
    let viewportHeightEvent (view: ITextView) = view.ViewportHeightChanged |> Event.map ignore
    let caretEvent (view: ITextView) = view.Caret.PositionChanged |> Event.map ignore
    let bufferEvent (buffer: ITextBuffer) = buffer.ChangedLowPriority |> Event.map ignore
    let tagsEvent (tagAggregator: ITagAggregator<_>) = tagAggregator.TagsChanged |> Event.map ignore

[<NoComparison; NoEquality>]
type CallInUIContext = 
    | CallInUIContext of ((unit -> unit) -> Async<unit>)
    static member FromCurrentThread() = 
        let uiContext = SynchronizationContext.Current
        CallInUIContext (fun f ->
            async {
                let ctx = SynchronizationContext.Current
                do! Async.SwitchToContext uiContext
                protect f
                do! Async.SwitchToContext ctx
            })

type AsyncManualResetEvent() = 
    [<VolatileField>]
    let mutable _tcs = new TaskCompletionSource<bool>()
    
    member __.WaitAsync() = _tcs.Task |> Async.AwaitTask |> Async.Ignore
    
    member __.Reset() =
        let rec loop() =
            let tcs = _tcs
            if tcs.Task.IsCompleted &&
               Interlocked.CompareExchange(&_tcs, new TaskCompletionSource<bool>(), tcs) <> tcs then 
                loop()
        loop()

    member __.Set() =
        let tcs = _tcs
        Task.Factory.StartNew(
            (fun s -> (box s :?> TaskCompletionSource<bool>).TrySetResult true),
            tcs, 
            CancellationToken.None, 
            TaskCreationOptions.PreferFairness, 
            TaskScheduler.Default) |> ignore
        tcs.Task.Wait()

type DocumentEventListener (events: IEvent<unit> list, delayMillis: uint16, update: CallInUIContext -> Async<unit>) =
    // Start an async loop on the UI thread that will execute the update action after the delay
    do if List.isEmpty events then invalidArg "events" "Events must be a non-empty list"
    let events = events |> List.reduce Event.merge
    let triggered = AsyncManualResetEvent()
    do events.Add (fun _ -> triggered.Set())
    let timer = DispatcherTimer(DispatcherPriority.ApplicationIdle, Interval = TimeSpan.FromMilliseconds (float delayMillis))
    let tokenSource = new CancellationTokenSource()
    let mutable disposed = false

    // This is a none or for-all option for unit testing purpose only
    static let mutable skipTimerDelay = false

    let startNewTimer() = 
        timer.Stop()
        timer.Start()
        
    let rec awaitPauseAfterChange() =
        async { 
            let! e = Async.EitherEvent(events, timer.Tick)
            match e with
            | Choice1Of2 _ -> 
                startNewTimer()
                do! awaitPauseAfterChange()
            | _ -> ()
        }
        
    do 
       let callUIContext = CallInUIContext.FromCurrentThread()
       let startUpdate (cts: CancellationTokenSource) = Async.StartInThreadPoolSafe (update callUIContext, cts.Token)

       let computation =
           async { 
               let mutable cts = new CancellationTokenSource()
               startUpdate cts

               while true do
                   //let e = events
                   do! triggered.WaitAsync()
                   triggered.Reset()
                   //Logging.logInfo <| fun _ -> "!!!!!!!!! awaited"
                   if not skipTimerDelay then
                       startNewTimer()
                       do! awaitPauseAfterChange()
                   cts.Cancel()
                   cts.Dispose()
                   cts <- new CancellationTokenSource()
                   startUpdate cts 
           }
       
       Async.StartInThreadPoolSafe (computation, tokenSource.Token)

    /// Skip all timer events in order to test events instantaneously
    static member internal SkipTimerDelay 
        with get () = skipTimerDelay
        and set v = skipTimerDelay <- v

    interface IDisposable with
        member __.Dispose() =
            if not disposed then
                tokenSource.Cancel()
                tokenSource.Dispose()
                timer.Stop()
                disposed <- true
   