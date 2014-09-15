namespace FSharpVSPowerTools.TaskList

open System
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading

type internal OptionsReader(serviceProvider: IServiceProvider) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    
    member __.GetOptions() =
        dte.get_Properties("Environment", "TaskList")
        |> Seq.cast
        |> Seq.tryPick (fun (prop: Property) ->
            if prop.Name = "CommentTokens" then
                Some (prop.Value :?> obj[] |> Array.map (fun o -> o :?> string))
            else None)
        |> function
           | Some ct ->
                ct |> Array.choose (fun s -> 
                    match s.Split(':') with
                    | [| comment; priority |] -> Some { Comment = comment; Priority = Int32.Parse priority } 
                    | _ -> None)
           | None -> [| CommentOption.Default |]


type internal OptionsChangedEventArgs(newOptions: CommentOption[]) =
    inherit EventArgs()

    member __.NewOptions = newOptions


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
                                optionsChanged.Trigger(new OptionsChangedEventArgs(newOptions))
                            currentOptions <- newOptions
        )

    let timer = new DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                    Interval = TimeSpan.FromMilliseconds(3000.0))
    
    [<CLIEvent>]
    member __.OptionsChanged = optionsChanged.Publish

    /// Starts listening for option changes
    member __.Start() =
        if timer.IsEnabled then invalidOp "Already listening for option changes"
        else
            timer.Tick.AddHandler(onElapsed)
            timer.Start()

    /// Stops listening for option changes
    member __.Stop() =
        if not timer.IsEnabled then invalidOp "Not currently listening for option changes"
        else
            timer.Tick.RemoveHandler(onElapsed)
            timer.Stop()
