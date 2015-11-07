namespace FSharpVSPowerTools.ProjectSystem

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.Threading
open System.Windows.Threading
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
module ViewChange =    
    open Microsoft.VisualStudio.Text.Tagging

    let layoutEvent (view: ITextView) = 
        view.LayoutChanged |> Event.choose (fun e -> if e.NewSnapshot <> e.OldSnapshot then Some() else None)
    
    let viewportHeightEvent (view: ITextView) = 
        view.ViewportHeightChanged |> Event.map (fun _ -> ())

    let caretEvent (view: ITextView) = 
        view.Caret.PositionChanged |> Event.map (fun _ -> ())

    let bufferEvent (buffer: ITextBuffer) = 
        buffer.ChangedLowPriority |> Event.map (fun _ -> ())

    let tagsEvent (tagAggregator: ITagAggregator<_>) = 
        tagAggregator.TagsChanged |> Event.map (fun _ -> ())

type DocumentEventListener (events: IEvent<unit> list, delayMillis: uint16, update: unit -> unit) =
    // Start an async loop on the UI thread that will execute the update action after the delay
    do if List.isEmpty events then invalidArg "events" "Events must be a non-empty list"
    let events = events |> List.reduce Event.merge
    let timer = DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                Interval = TimeSpan.FromMilliseconds (float delayMillis))
    let tokenSource = new CancellationTokenSource()
    let mutable disposed = false

    // This is a none or for-all option for unit testing purpose only
    static let mutable skipTimerDelay = false

    // Protect against exceptions and log error messages
    let protectUpdate() = protect update

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
       let computation =
           async { 
            while true do
                do! Async.AwaitEvent events
                if not skipTimerDelay then
                    startNewTimer()
                    do! awaitPauseAfterChange()
                protectUpdate() }
       // Go ahead and synchronously get the first bit of info for the original rendering
       protectUpdate()
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
   