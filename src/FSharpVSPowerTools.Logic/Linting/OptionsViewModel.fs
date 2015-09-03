namespace FSharpVSPowerTools.Linting

open System
open System.ComponentModel
open System.Collections.ObjectModel
open FSharpVSPowerTools
open FSharpLint.Framework
open Configuration
open FParsec
open FSharp.ViewModule

type BoolViewModel(name, isChecked) as this =
    inherit ViewModelBase()

    let isChecked = this.Factory.Backing(<@ this.IsChecked @>, isChecked)

    member __.Name = name

    member __.IsChecked
        with get() = isChecked.Value
        and set(v) = isChecked.Value <- v

type IntViewModel(name, initialValue) as this =
    inherit ViewModelBase()

    let value = this.Factory.Backing(<@ this.Value @>, initialValue)

    member __.Name = name

    member __.Value
        with get() = value.Value
        and set(v) = value.Value <- v

type IRemovable =
    abstract RemoveManyCommand : INotifyCommand 

type HintsViewModel(config) as this =
    inherit ViewModelBase()

    let validateHint hint =
        [
            if hint <> "" then
                match CharParsers.run HintParser.phint hint with
                | CharParsers.Success(_) -> ()
                | CharParsers.Failure(message, _, _) -> yield message
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

    let isEnabled = hintSettings |> Seq.exists (fun x -> x = Some(Enabled(true)))

    let isEnabled = this.Factory.Backing(<@ this.IsEnabled @>, isEnabled)

    member __.Hints = hints

    member __.IsEnabled
        with get() = isEnabled.Value
        and set(v) = isEnabled.Value <- v

    member __.AddHintCommand = 
        this.Factory.CommandSync(fun _ -> 
            if String.IsNullOrEmpty this.NewHint |> not then
                match CharParsers.run HintParser.phint this.NewHint with
                | CharParsers.Success(_) -> 
                    hints.Add(this.NewHint)
                    this.NewHint <- ""
                    this.SelectedHintIndex <- hints.Count - 1
                | CharParsers.Failure(_) -> ())
                
    interface IRemovable with
        member __.RemoveManyCommand = 
            this.Factory.CommandSyncParam(Seq.iter (hints.Remove >> ignore))
                
    member __.RemoveCommand = 
        this.Factory.CommandSyncParam(hints.Remove >> ignore)

    member __.SelectedHintIndex
        with get() = selectedHintIndex.Value
        and set(v) = selectedHintIndex.Value <- v

    member __.NewHint
        with get() = newHint.Value
        and set(v) = newHint.Value <- v

type AccessViewModel(name, initialValue) as this =
    inherit ViewModelBase()

    let value = this.Factory.Backing(<@ this.Value @>, initialValue)

    member __.Value
        with get() = value.Value
        and set(v) = value.Value <- v

    member __.Name = name

    member __.AccessValues = Enum.GetValues(typeof<Access>) |> Seq.cast<Access>
                
module SetupViewModels =
    let getSettingsViewModelsFromRule (settings:Map<_, _>) =
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

    let isRuleEnabled settings = Map.tryFind "Enabled" settings = Some(Enabled(true))

    let ruleViewModelsFromConfig config = 
        seq { 
            for analyser in config.Analysers |> Seq.filter (fun x -> x.Key <> "Hints") do 
                let rules = seq { 
                    for rule in analyser.Value.Rules do 
                        yield RuleViewModel(rule.Key,
                                            [],
                                            getSettingsViewModelsFromRule rule.Value.Settings,
                                            isRuleEnabled rule.Value.Settings) 
                }

                yield RuleViewModel(analyser.Key, 
                                    rules, 
                                    getSettingsViewModelsFromRule analyser.Value.Settings,
                                    isRuleEnabled analyser.Value.Settings) 
        }

type IgnoreFilesModel(config) as this =
    inherit ViewModelBase()

    let ignoreFiles =
        ObservableCollection(
            match config.IgnoreFiles with
            | Some x -> 
                x.Content 
                |> String.split StringSplitOptions.RemoveEmptyEntries [| Environment.NewLine |]
                |> Array.map String.trim
            | None -> [||])

    let selectedIgnoreFileIndex = this.Factory.Backing(<@ this.SelectedIgnoreFileIndex @>, 0)
    
    let newIgnoreFile = this.Factory.Backing(<@ this.NewIgnoreFile @>, "")
    
    member __.IgnoreFiles = ignoreFiles

    member __.AddIgnoreFileCommand = 
        this.Factory.CommandSync(fun _ -> 
            if (String.IsNullOrEmpty >> not) this.NewIgnoreFile then
                ignoreFiles.Add(this.NewIgnoreFile)
                this.NewIgnoreFile <- ""
                this.SelectedIgnoreFileIndex <- ignoreFiles.Count - 1)
                
    interface IRemovable with
        member __.RemoveManyCommand = 
            this.Factory.CommandSyncParam(Seq.iter (ignoreFiles.Remove >> ignore))
                
    member __.RemoveCommand = 
        this.Factory.CommandSyncParam(ignoreFiles.Remove >> ignore)

    member __.SelectedIgnoreFileIndex
        with get() = selectedIgnoreFileIndex.Value
        and set(v) = selectedIgnoreFileIndex.Value <- v

    member __.NewIgnoreFile
        with get() = newIgnoreFile.Value
        and set(v) = newIgnoreFile.Value <- v

type OptionsViewModel(config, files, fileNames:string seq, selectedFile:string) as this =
    inherit ViewModelBase()

    let hints = HintsViewModel(config)
    let ignoreFiles = IgnoreFilesModel(config)
    let rules = SetupViewModels.ruleViewModelsFromConfig config
    
    let selectedRule = this.Factory.Backing(<@ this.SelectedRule @>, None)

    let selectedFilename = this.Factory.Backing(<@ this.SelectedFileName @>, selectedFile)

    member __.SelectedRule 
        with get() = selectedRule.Value
        and set (value) = selectedRule.Value <- value

    member __.SelectedRuleChanged = 
        this.Factory.CommandSyncParam(fun p -> this.SelectedRule <- Some(p))

    member __.SelectedFileName = selectedFilename.Value
    
    member __.Files = files

    member __.FileNames = fileNames
    
    member __.Rules = rules

    member __.Hints = hints

    member __.IgnoreFiles = ignoreFiles