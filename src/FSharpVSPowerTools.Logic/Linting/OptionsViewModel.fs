namespace FSharpVSPowerTools.Linting

open System
open System.ComponentModel
open FSharpLint.Framework.Configuration

type BoolViewModel(name, initialValue) =
    let mutable name = name
    let mutable isChecked = initialValue

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
    let hintSettings =
        match config with
        | Some(config) -> 
            config.Analysers 
                |> Seq.find (fun x -> x.Key = "Hints")
                |> (fun x -> x.Value.Settings)
                |> Seq.map (fun x -> Some(x.Value))
        | None -> Seq.empty

    let mutable hints =
        hintSettings 
            |> Seq.pick (function | Some(Hints(hints)) -> Some(hints) | _ -> None)
            |> List.toSeq

    let mutable isEnabled =
        hintSettings 
            |> Seq.find (function | Some(Enabled(enabled)) -> enabled | _ -> false)

    member this.Hints
        with get() = hints
        and set(v) = hints <- v

    member this.IsEnabled
        with get() = isEnabled
        and set(v) = isEnabled <- v

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
        seq {
            for setting in settings do
                match setting.Value with
                | Lines(value) -> yield IntViewModel("Lines", value) :> obj
                | Depth(value) -> yield IntViewModel("Depth", value) :> obj
                | MaxItems(value) -> yield IntViewModel("MaxItems", value) :> obj
                | Length(value) -> yield IntViewModel("Length", value) :> obj
                | MaxCyclomaticComplexity(value) -> yield IntViewModel("MaxCyclomaticComplexity", value) :> obj
                | IncludeMatchStatements(value) -> yield BoolViewModel("IncludeMatchStatements", value) :> obj
                | OneSpaceAllowedAfterOperator(value) -> yield BoolViewModel("OneSpaceAllowedAfterOperator", value) :> obj
                | NumberOfSpacesAllowed(value) -> yield IntViewModel("NumberOfSpacesAllowed", value) :> obj
                | IgnoreBlankLines(value) -> yield BoolViewModel("IgnoreBlankLines", value) :> obj
                | Access(value) -> yield AccessViewModel("Access", value) :> obj
                | Hints(_)
                | Enabled(_) -> ()
        }

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
    let mutable selectedRule:RuleViewModel = null

    let propertyChanged = new Event<_, _>()

    let mutable (files:FileViewModel seq) = Seq.empty

    let mutable hints = HintsViewModel(config)

    let mutable ignoreFiles =
        match config with
        | Some(config) -> 
            match config.IgnoreFiles with
            | Some(x) -> 
                x.Content.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun x -> x.Trim())
                    |> String.concat Environment.NewLine
            | None -> ""
        | None -> ""

    let mutable (rules:RuleViewModel seq) = 
        match config with
        | Some(config) -> SetupViewModels.ruleViewModelsFromConfig config
        | None -> Seq.empty

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member this.OnPropertyChanged(e:PropertyChangedEventArgs) =
        propertyChanged.Trigger(this, e)

    member this.OnPropertyChanged(propertyName:string) =
        this.OnPropertyChanged(PropertyChangedEventArgs(propertyName))

    member this.SelectedRule 
        with get() = selectedRule
        and set (value) = 
            selectedRule <- value
            this.OnPropertyChanged("SelectedRule")
    
    member val SelectedFileName:int = 0 with get, set
    
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