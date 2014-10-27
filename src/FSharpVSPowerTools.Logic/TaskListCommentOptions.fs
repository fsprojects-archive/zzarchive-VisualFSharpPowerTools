namespace FSharpVSPowerTools.TaskList

open System
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools

type internal OptionsReader(serviceProvider: IServiceProvider) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    
    member __.GetOptions() =
        dte.TryGetProperty("Environment", "TaskList", "CommentTokens")
        |> function
           | Some ct ->
                match ct with
                | :? (string []) as ss ->
                    ss
                    |> Array.choose (fun s -> 
                        match s.Split(':') with
                        | [| comment; priority |] -> 
                            match Int32.TryParse priority with
                            | true, priorityVal ->
                                Some { Comment = comment; Priority = priorityVal } 
                            | _ -> None
                        | _ -> None)
                | _ -> 
                    [| CommentOption.Default |]
           | None -> 
                [| CommentOption.Default |]


type internal OptionsChangedEventArgs(newOptions: CommentOption[]) =
    inherit EventArgs()

    member __.NewOptions = newOptions


type internal OptionsMonitor(serviceProvider: IServiceProvider) =
    let optionsReader = OptionsReader(serviceProvider)

    let mutable currentOptions = optionsReader.GetOptions()
    let haveOptionsChanged newOptions =
        let sortByText = fun o -> o.Comment
        let sortedNewOptions = newOptions |> Array.sortBy sortByText
        let sortedCurrentOptions = currentOptions |> Array.sortBy sortByText
        sortedNewOptions <> sortedCurrentOptions

    let optionsChanged = Event<OptionsChangedEventArgs>()
    let onElapsed =
        EventHandler(fun _ _ ->
            protect <| fun _ ->
                            let newOptions = optionsReader.GetOptions()
                            if haveOptionsChanged newOptions then
                                optionsChanged.Trigger(new OptionsChangedEventArgs(newOptions))
                            currentOptions <- newOptions
        )

    let timer = DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                Interval = TimeSpan.FromMilliseconds(3000.0))
    
    [<CLIEvent>]
    member __.OptionsChanged = optionsChanged.Publish

    /// Starts listening for option changes
    member __.Start() =
        if timer.IsEnabled then 
            debug "Already listening for option changes"
        else
            timer.Tick.AddHandler(onElapsed)
            timer.Start()

    /// Stops listening for option changes
    member __.Stop() =
        if not timer.IsEnabled then 
            debug "Not currently listening for option changes"
        else
            timer.Tick.RemoveHandler(onElapsed)
            timer.Stop()
