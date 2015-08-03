namespace FSharpVSPowerTools.Linting

open System.ComponentModel
open FSharpLint.Framework.Configuration

type BoolViewModel() =
    let mutable name = ""
    let mutable isChecked = false

    member this.Name
        with set(value) = name <- value
        and get() = name

    member this.IsChecked
        with get() = isChecked
        and set(v) = isChecked <- v

type IntViewModel() =
    let mutable name = ""
    let mutable value = 0

    member this.Name
        with set(value) = name <- value
        and get() = name

    member this.Value
        with get() = value
        and set(v) = value <- v

type HintsViewModel() =
    let mutable name = ""

    member this.Name
        with set(value) = name <- value
        and get() = name

module SetupViewModels =
    let getSettingsViewModelsFromRule (settings:Map<string, Setting>) =
        seq {
            for setting in settings do
                match setting.Value with
                | Lines(x) -> yield IntViewModel(Name = "Lines", Value = x) :> obj
                | Depth(x) -> yield IntViewModel(Name = "Depth", Value = x) :> obj
                | MaxItems(x) -> yield IntViewModel(Name = "MaxItems", Value = x) :> obj
                | Length(x) -> yield IntViewModel(Name = "Length", Value = x) :> obj
                | MaxCyclomaticComplexity(x) -> yield IntViewModel(Name = "MaxCyclomaticComplexity", Value = x) :> obj
                | IncludeMatchStatements(x) -> yield BoolViewModel(Name = "IncludeMatchStatements", IsChecked = x) :> obj
                | OneSpaceAllowedAfterOperator(x) -> yield BoolViewModel(Name = "OneSpaceAllowedAfterOperator", IsChecked = x) :> obj
                | NumberOfSpacesAllowed(x) -> yield IntViewModel(Name = "NumberOfSpacesAllowed", Value = x) :> obj
                | IgnoreBlankLines(x) -> yield BoolViewModel(Name = "IgnoreBlankLines", IsChecked = x) :> obj
                | Hints(_) -> yield HintsViewModel(Name = "Hints") :> obj
                | Access(_)
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
            for analyser in config.Analysers do 
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
    
    member this.Rules
        with get() = rules
        and set(value) = rules <- value