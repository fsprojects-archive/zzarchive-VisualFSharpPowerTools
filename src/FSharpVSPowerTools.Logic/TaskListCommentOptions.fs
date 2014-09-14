namespace FSharpVSPowerTools.TaskList

open System
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading

type internal OptionsReader(serviceProvider: IServiceProvider) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    
    member x.GetOptions() =
        let taskListOptions = dte.get_Properties("Environment", "TaskList")
        let mutable ct = None
        for o in taskListOptions do
            if o.Name = "CommentTokens" then
                ct <- Some((o.Value :?> obj[]) |> Array.map (fun o -> o :?> string))

        let vfptOptions = dte.get_Properties("F# Power Tools", "General")
        let mutable tlce = false
        for o in vfptOptions do
            if o.Name = "TaskListCommentsEnabled" then
                tlce <- o.Value :?> bool
        
        if ct.IsSome then
            ct.Value |> Array.map (fun s -> let parts = s.Split(':') in
                                                { Comment = parts.[0]; Priority = Int32.Parse(parts.[1]) })
        else
            [| CommentOption.Default |]
    

type internal OptionsChangedEventArgs(oldOptions: CommentOption[], newOptions: CommentOption[]) =
    inherit EventArgs()

    member x.OldOptions = oldOptions
    member x.NewOptions = newOptions


type internal OptionsMonitor(serviceProvider: IServiceProvider) =
    let optionsReader = new OptionsReader(serviceProvider)

    let mutable currentOptions = optionsReader.GetOptions()
    let haveOptionsChanged newOptions =
        let sortByText = fun o -> o.Comment

        (newOptions |> Array.sortBy sortByText)
        <>
        (currentOptions |> Array.sortBy sortByText)

    let optionsChanged = new Event<OptionsChangedEventArgs>()
    let onElapsed =
        new EventHandler(fun _ _ ->
                            let newOptions = optionsReader.GetOptions()
                            if haveOptionsChanged newOptions then
                                optionsChanged.Trigger(new OptionsChangedEventArgs(currentOptions, newOptions))
                            currentOptions <- newOptions
        )

    let timer = new DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                    Interval = TimeSpan.FromMilliseconds(3000.0))
    
    [<CLIEvent>]
    member x.OptionsChanged = optionsChanged.Publish

    member x.GetOptions() =
        currentOptions

    /// Starts listening for option changes
    member x.Start() =
        if timer.IsEnabled then invalidOp "Already listening for option changes"
        else
            timer.Tick.AddHandler(onElapsed)
            timer.Start()

    /// Stops listening for option changes
    member x.Stop() =
        if not timer.IsEnabled then invalidOp "Not currently listening for option changes"
        else
            timer.Tick.RemoveHandler(onElapsed)
            timer.Stop()

    /// Gets whether this monitor is currently listening for option changes
    member x.IsListening = timer.IsEnabled