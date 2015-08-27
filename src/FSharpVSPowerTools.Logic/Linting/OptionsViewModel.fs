namespace FSharpVSPowerTools.Linting

open System
open System.ComponentModel
open System.Collections.ObjectModel
open FSharpLint.Framework.Configuration
open FSharp.ViewModule

type BoolViewModel(name, isChecked) as this =
    inherit ViewModelBase()

    let isChecked = this.Factory.Backing(<@ this.IsChecked @>, isChecked)

    member this.Name
        with get() = name

    member this.IsChecked
        with get() = isChecked.Value
        and set(v) = isChecked.Value <- v

type IntViewModel(name, initialValue) as this =
    inherit ViewModelBase()

    let value = this.Factory.Backing(<@ this.Value @>, initialValue)

    member this.Name
        with get() = name

    member this.Value
        with get() = value.Value
        and set(v) = value.Value <- v

type HintsViewModel(config:Configuration) as this =
    inherit ViewModelBase()

    let validateHint hint =
        [
            if hint <> "" then
                match FParsec.CharParsers.run FSharpLint.Framework.HintParser.phint hint with
                | FParsec.CharParsers.Success(_) -> ()
                | FParsec.CharParsers.Failure(message, _, _) -> yield message
        ]

    let newHint = this.Factory.Backing(<@ this.NewHint @>, "", validateHint)
    let selectedHintIndex = this.Factory.Backing(<@ this.SelectedHintIndex @>, 0)

    let hintSettings =
        config.Analysers 
            |> Seq.tryFind (fun x -> x.Key = "Hints")
            |> (function 
                | Some(x) -> x.Value.Settings |> Seq.map (fun x -> Some(x.Value)) 
                | None -> Seq.empty)

    let hints =
        hintSettings 
            |> Seq.tryPick (function | Some(Hints(hints)) -> Some(hints) | _ -> None)
            |> (function | Some(x) -> ObservableCollection<_>(x) | None -> ObservableCollection<_>())

    let isEnabled =
        hintSettings 
            |> Seq.tryPick (function | Some(Enabled(enabled)) -> Some(enabled) | _ -> None)
            |> (function | Some(enabled) -> enabled | None -> false)

    let isEnabled = this.Factory.Backing(<@ this.IsEnabled @>, isEnabled)

    member this.Hints with get() = hints

    member this.IsEnabled
        with get() = isEnabled.Value
        and set(v) = isEnabled.Value <- v

    member this.AddHintCommand = 
        this.Factory.CommandSync(fun _ -> 
            if String.IsNullOrEmpty this.NewHint |> not then
                match FParsec.CharParsers.run FSharpLint.Framework.HintParser.phint this.NewHint with
                | FParsec.CharParsers.Success(_) -> 
                    hints.Add(this.NewHint)
                    this.NewHint <- ""
                    this.SelectedHintIndex <- hints.Count - 1
                | FParsec.CharParsers.Failure(_) -> ())

    member this.RemoveHintCommand = 
        this.Factory.CommandSyncParam(fun (selectedItem:obj) ->
            if selectedItem :? string then
                hints.Remove(selectedItem :?> string) |> ignore)

    member this.SelectedHintIndex
        with get() = selectedHintIndex.Value
        and set(v) = selectedHintIndex.Value <- v

    member this.NewHint
        with get() = newHint.Value
        and set(v) = newHint.Value <- v

type AccessViewModel(name, initialValue) as this =
    inherit ViewModelBase()

    let value = this.Factory.Backing(<@ this.Value @>, initialValue)

    member this.Value
        with get() = value.Value
        and set(v) = value.Value <- v

    member this.Name
        with get() = name

    member this.AccessValues
        with get() = Enum.GetValues(typeof<Access>) |> Seq.cast<Access>
                
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
                        yield RuleViewModel(rule.Key,
                                            [],
                                            getSettingsViewModelsFromRule rule.Value.Settings,
                                            IsChecked = isRuleEnabled rule.Value.Settings) 
                }

                yield RuleViewModel(analyser.Key, 
                                    rules, 
                                    getSettingsViewModelsFromRule analyser.Value.Settings,
                                    IsChecked = isRuleEnabled analyser.Value.Settings) 
        }

type OptionsViewModel(config:Configuration, files:FileViewModel seq) as this =
    inherit ViewModelBase()

    let hints = HintsViewModel(config)
    
    let selectedRule = this.Factory.Backing(<@ this.SelectedRule @>, None)
    let newIgnoreFile = this.Factory.Backing(<@ this.NewIgnoreFile @>, "")

    let ignoreFiles =
        match config.IgnoreFiles with
        | Some(x) -> 
            x.Content.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.map (fun x -> x.Trim())
                |> (fun x -> ObservableCollection<_>(x))
        | None -> ObservableCollection<string>()

    let selectedIgnoreFileIndex = this.Factory.Backing(<@ this.SelectedIgnoreFileIndex @>, 0)

    let rules = SetupViewModels.ruleViewModelsFromConfig config

    member this.SelectedRule 
        with get() = selectedRule.Value
        and set (value) = selectedRule.Value <- value
    
    member this.Files
        with get() = files
    
    member this.IgnoreFiles
        with get() = ignoreFiles
    
    member this.Rules
        with get() = rules

    member this.Hints
        with get() = hints

    member this.AddIgnoreFileCommand = 
        this.Factory.CommandSync(fun _ -> 
            if String.IsNullOrEmpty this.NewIgnoreFile |> not then
                ignoreFiles.Add(this.NewIgnoreFile)
                this.NewIgnoreFile <- ""
                this.SelectedIgnoreFileIndex <- ignoreFiles.Count - 1)

    member this.RemoveIgnoreFileCommand = 
        this.Factory.CommandSyncParam(fun (selectedItem:obj) ->
            if selectedItem :? string then
                ignoreFiles.Remove(selectedItem :?> string) |> ignore)

    member this.SelectedIgnoreFileIndex
        with get() = selectedIgnoreFileIndex.Value
        and set(v) = selectedIgnoreFileIndex.Value <- v

    member this.NewIgnoreFile
        with get() = newIgnoreFile.Value
        and set(v) = newIgnoreFile.Value <- v