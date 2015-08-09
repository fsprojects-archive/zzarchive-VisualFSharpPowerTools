namespace FSharpVSPowerTools.Linting

open System
open System.ComponentModel
open System.Collections.ObjectModel
open FSharpLint.Framework.Configuration
open FSharp.ViewModule

type BoolViewModel(name, isChecked) =
    let mutable name = name
    let mutable isChecked = isChecked

    member this.Name
        with set(value) = name <- value
        and get() = name

    member this.IsChecked
        with get() = isChecked
        and set(v) = isChecked <- v

type IntViewModel(name, initialValue) =
    let mutable name = name
    let mutable value = initialValue

    member this.Name
        with set(value) = name <- value
        and get() = name

    member this.Value
        with get() = value
        and set(v) = value <- v

type HintsViewModel(config:Configuration option) =
    inherit ViewModelBase()

    let mutable selectedHintIndex = 0

    let mutable newHint = ""

    let propertyChanged = new Event<_, _>()

    let hintSettings =
        match config with
        | Some(config) -> 
            config.Analysers 
                |> Seq.tryFind (fun x -> x.Key = "Hints")
                |> (function 
                    | Some(x) -> x.Value.Settings |> Seq.map (fun x -> Some(x.Value)) 
                    | None -> Seq.empty)
        | None -> Seq.empty

    let hints =
        hintSettings 
            |> Seq.tryPick (function | Some(Hints(hints)) -> Some(hints) | _ -> None)
            |> (function | Some(x) -> ObservableCollection<_>(x) | None -> ObservableCollection<_>())

    let mutable isEnabled =
        hintSettings 
            |> Seq.tryPick (function | Some(Enabled(enabled)) -> Some(enabled) | _ -> None)
            |> (function | Some(enabled) -> enabled | None -> false)

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member private this.OnPropertyChanged(propertyName:string) =
        propertyChanged.Trigger(this, PropertyChangedEventArgs(propertyName))

    member this.Hints with get() = hints

    member this.IsEnabled
        with get() = isEnabled
        and set(v) = isEnabled <- v

    member this.AddHintCommand = 
        this.Factory.CommandSync(fun _ -> 
            if String.IsNullOrEmpty this.NewHint |> not then
                hints.Add(this.NewHint)
                this.NewHint <- ""
                this.SelectedHintIndex <- hints.Count - 1)

    member this.RemoveHintCommand = 
        this.Factory.CommandSyncParam(fun (selectedItem:obj) ->
            if selectedItem <> null && selectedItem :? string then
                hints.Remove(selectedItem :?> string) |> ignore)

    member this.SelectedHintIndex
        with get() = selectedHintIndex
        and set(v) = 
            selectedHintIndex <- v
            this.OnPropertyChanged("SelectedHintIndex")

    member this.NewHint
        with get() = newHint
        and set(v) = 
            newHint <- v
            this.OnPropertyChanged("NewHint")

type AccessViewModel(name, initialValue) =
    let mutable name = name
    let mutable value = initialValue

    member this.Value
        with get() = value
        and set(v) = value <- v

    member this.Name
        with set(value) = name <- value
        and get() = name

    member this.AccessValues
        with get() =
            System.Enum.GetValues(typeof<Access>)
                |> Seq.cast<Access>
                
module SetupViewModels =
    let getSettingsViewModelsFromRule (settings:Map<string, Setting>) =
        [
            for setting in settings do
                match setting.Value with
                | Lines(value) -> 
                    yield IntViewModel("Lines", value) :> obj
                | Depth(value) -> 
                    yield IntViewModel("Depth", value) :> obj
                | MaxItems(value) -> 
                    yield IntViewModel("MaxItems", value) :> obj
                | Length(value) -> 
                    yield IntViewModel("Length", value) :> obj
                | MaxCyclomaticComplexity(value) -> 
                    yield IntViewModel("MaxCyclomaticComplexity", value) :> obj
                | IncludeMatchStatements(value) -> 
                    yield BoolViewModel("IncludeMatchStatements", value) :> obj
                | OneSpaceAllowedAfterOperator(value) -> 
                    yield BoolViewModel("OneSpaceAllowedAfterOperator", value) :> obj
                | NumberOfSpacesAllowed(value) -> 
                    yield IntViewModel("NumberOfSpacesAllowed", value) :> obj
                | IgnoreBlankLines(value) -> 
                    yield BoolViewModel("IgnoreBlankLines", value) :> obj
                | Access(value) -> 
                    yield AccessViewModel("Access", value) :> obj
                | Hints(_)
                | Enabled(_) -> ()
        ]

    let isRuleEnabled (settings:Map<string, Setting>) =
        if settings.ContainsKey "Enabled" then
            match settings.["Enabled"] with
            | Enabled(e) -> e
            | _ -> false
        else
            false

    let ruleViewModelsFromConfig config = 
        seq { 
            for analyser in config.Analysers |> Seq.where (fun x -> x.Key <> "Hints") do 
                let rules = seq { 
                    for rule in analyser.Value.Rules do 
                        yield RuleViewModel(Name = rule.Key,
                                            Settings = getSettingsViewModelsFromRule rule.Value.Settings,
                                            IsChecked = isRuleEnabled rule.Value.Settings) 
                }

                yield RuleViewModel(Name = analyser.Key, 
                                    Rules = rules, 
                                    Settings = getSettingsViewModelsFromRule analyser.Value.Settings,
                                    IsChecked = isRuleEnabled analyser.Value.Settings) 
        }

type OptionsViewModel(?config:Configuration) =
    inherit ViewModelBase()

    let mutable selectedRule:RuleViewModel = null

    let propertyChanged = new Event<_, _>()

    let mutable (files:FileViewModel seq) = Seq.empty

    let mutable hints = HintsViewModel(config)

    let mutable newIgnoreFile = ""

    let mutable ignoreFiles =
        match config with
        | Some(config) -> 
            match config.IgnoreFiles with
            | Some(x) -> 
                x.Content.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun x -> x.Trim())
                    |> (fun x -> ObservableCollection<_>(x))
            | None -> ObservableCollection<string>()
        | None -> ObservableCollection<string>()

    let mutable selectedIgnoreFileIndex = 0

    let mutable (rules:RuleViewModel seq) = 
        match config with
        | Some(config) -> SetupViewModels.ruleViewModelsFromConfig config
        | None -> Seq.empty

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member private this.OnPropertyChanged(propertyName:string) =
        propertyChanged.Trigger(this, PropertyChangedEventArgs(propertyName))

    member this.SelectedRule 
        with get() = selectedRule
        and set (value) = 
            selectedRule <- value
            this.OnPropertyChanged("SelectedRule")
    
    member val FileNames:string seq = null with get, set
    
    member this.Files
        with get() = files
        and set(value) = files <- value
    
    member this.IgnoreFiles
        with get() = ignoreFiles
        and set(value) = ignoreFiles <- value
    
    member this.Rules
        with get() = rules
        and set(value) = rules <- value

    member this.Hints
        with get() = hints
        and set(value) = hints <- value

    member this.AddIgnoreFileCommand = 
        this.Factory.CommandSync(fun _ -> 
            if String.IsNullOrEmpty this.NewIgnoreFile |> not then
                ignoreFiles.Add(this.NewIgnoreFile)
                this.NewIgnoreFile <- ""
                this.SelectedIgnoreFileIndex <- ignoreFiles.Count - 1)

    member this.RemoveIgnoreFileCommand = 
        this.Factory.CommandSyncParam(fun (selectedItem:obj) ->
            if selectedItem <> null && selectedItem :? string then
                ignoreFiles.Remove(selectedItem :?> string) |> ignore)

    member this.SelectedIgnoreFileIndex
        with get() = selectedIgnoreFileIndex
        and set(v) = 
            selectedIgnoreFileIndex <- v
            this.OnPropertyChanged("SelectedIgnoreFileIndex")

    member this.NewIgnoreFile
        with get() = newIgnoreFile
        and set(v) = 
            newIgnoreFile <- v
            this.OnPropertyChanged("NewIgnoreFile")