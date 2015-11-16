namespace FSharpVSPowerTools.ProjectSystem

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Shell.Interop
open System.ComponentModel.Composition
open Microsoft.VisualStudio.ComponentModelHost
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

[<NoComparison; NoEquality>]
type CallInUIContext = CallInUIContext of ((unit -> unit) -> Async<unit>)
    with static member FromCurrentThread() = 
                         let uiContext = SynchronizationContext.Current
                         CallInUIContext (fun f ->
                             async {
                                 let ctx = SynchronizationContext.Current
                                 do! Async.SwitchToContext uiContext
                                 protect f
                                 do! Async.SwitchToContext ctx
                             })

type DocumentEventListener (events: IEvent<unit> list, delayMillis: uint16, update: CallInUIContext -> Async<unit>) =
    // Start an async loop on the UI thread that will execute the update action after the delay
    do if List.isEmpty events then invalidArg "events" "Events must be a non-empty list"
    let events = events |> List.reduce Event.merge
    let timer = DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                Interval = TimeSpan.FromMilliseconds (float delayMillis))
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
               let cts = ref (new CancellationTokenSource())
               startUpdate !cts

               while true do
                   do! Async.AwaitEvent events
                   if not skipTimerDelay then
                       startNewTimer()
                       do! awaitPauseAfterChange()
                   (!cts).Cancel()
                   (!cts).Dispose()
                   cts := new CancellationTokenSource()
                   startUpdate !cts }
       
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

[<Export>]
type EventChannel [<ImportingConstructor>]   
    ([<Import(typeof<SVsFileChangeEx)>] fileChangeService : IVsFileChangeEx,
     [<Import(typeof<SVsShell)>] shellService : IVsShell ) =

    let cookie = ref 0u
                            fileChangeService.AdviseFileChange(file, trackedChange, fileChangeMonitor, cookie) |> ignore
                            fileChangeCookies.[file] <- !cookie)