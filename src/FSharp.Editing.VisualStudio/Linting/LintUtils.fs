namespace FSharp.Editing.VisualStudio.Linting

open System
open System.Diagnostics
open System.IO
open FSharpLint.Framework.Configuration
open IgnoreFiles
open Management
open FSharp.Editing

module LintUtils =
    open FSharp.Editing.VisualStudio

    let getProjectPaths (dte: EnvDTE.DTE) =
        listFSharpProjectsInSolution dte
        |> List.choose (fun project -> 
                let projectFilePath = project.FullName
                if not (String.IsNullOrEmpty projectFilePath) then
                    let projectDirectoryPath = Path.GetDirectoryName projectFilePath
                    Some (normalisePath projectDirectoryPath)
                else
                    None)

    let getSolutionPath (dte: EnvDTE.DTE) =
        let solutionFilePath = dte.Solution.FullName
        if not (String.IsNullOrEmpty solutionFilePath) then 
            let solutionDirectoryPath = Path.GetDirectoryName solutionFilePath
            Some (normalisePath solutionDirectoryPath)
        else
            None

    let tryLoadConfig path = 
        let filename = 
            path 
            |> String.concat (Path.DirectorySeparatorChar.ToString())
            |> fun x -> x </> SettingsFileName

        if File.Exists(filename) then
            try
                File.ReadAllText filename |> configuration |> Some
            with
                | ConfigurationException(message) ->
                    Logging.logWarning (fun _ -> sprintf "Failed to load config file %s: %s" filename message)
                    None
                | e ->
                    Logging.logWarning (fun _ -> sprintf "Failed to load config file %s: %s" filename e.Message)
                    None
        else
            None

    let updateLoadedConfigs dte loadedConfigs =
        updatePaths tryLoadConfig loadedConfigs (getProjectPaths dte)

    let getInitialLoadedConfigs dte =
        updateLoadedConfigs dte LoadedConfigs.Empty

    let private directorySeparator = Path.DirectorySeparatorChar.ToString()

    let rec private listStartsWith = function
        | (_, []) -> true
        | (x::list, y::startsWithList) when x = y ->
            listStartsWith (list, startsWithList)
        | _ -> false

    let getFileHierarchy loadedConfigs =
        let files = Map.toSeq loadedConfigs.LoadedConfigs

        let rec createFileViewModel currentPath =
            [ for (path, config) in files do
                if List.length path = List.length currentPath + 1 && listStartsWith (path, currentPath) then
                    let lastSegment = Seq.last path + directorySeparator
                    let hasConfig = Option.isSome config
                    yield FileViewModel(lastSegment, path, createFileViewModel path, hasConfig)]

        [ for (path, config) in files do
            match path with
            | [root] -> 
                let hasConfig = Option.isSome config
                yield FileViewModel(root + directorySeparator, 
                                    path,
                                    createFileViewModel path, 
                                    hasConfig)
            | _ -> () ]
                    
    let getInitialPath dte loadedConfigs =
        getSolutionPath dte
        |> Option.bind (fun solutionPath ->
            commonPath loadedConfigs solutionPath
            |> Option.orElse (Some solutionPath))
        |> Option.orTry (fun _ ->
            getProjectPaths dte |> List.tryHead)

    let getConfigForDirectory loadedConfigs directory =
        normalisePath directory
        |> getConfig loadedConfigs
        |> Option.getOrElse defaultConfiguration

    let private settingFromObject (settingObj:obj) = 
        match settingObj with
        | :? BoolViewModel as x -> 
            match x.Name with
            | "IncludeMatchStatements" -> Some(x.Name, IncludeMatchStatements(x.IsChecked))
            | "OneSpaceAllowedAfterOperator" -> Some(x.Name, OneSpaceAllowedAfterOperator(x.IsChecked))
            | "IgnoreBlankLines" -> Some(x.Name, IgnoreBlankLines(x.IsChecked))
            | _ -> None
        | :? IntViewModel as x -> 
            match x.Name with
            | "Lines" -> Some(x.Name, Lines(x.Value))
            | "Depth" -> Some(x.Name, Depth(x.Value))
            | "MaxItems" -> Some(x.Name, MaxItems(x.Value))
            | "Length" -> Some(x.Name, Length(x.Value))
            | "MaxCyclomaticComplexity" -> Some(x.Name, MaxCyclomaticComplexity(x.Value))
            | "NumberOfSpacesAllowed" -> Some(x.Name, NumberOfSpacesAllowed(x.Value))
            | _ -> None
        | :? AccessViewModel as x -> Some(x.Name, Access(x.Value))
        | _ -> None

    let private settingsFromRuleViewModel (viewModel:RuleViewModel) =
        let settings = 
            [ for settingObj in viewModel.Settings do 
                match settingFromObject settingObj with 
                | Some(setting) -> yield setting 
                | None -> Debug.Assert(false, "Unknown lint setting.") ]

        ("Enabled", Enabled(viewModel.IsChecked))::settings |> Map.ofList

    let private ruleViewModelToRule (viewModel:RuleViewModel) =
        (viewModel.Name, { Rule.Settings = settingsFromRuleViewModel viewModel })

    let private ruleViewModelToAnalyser (viewModel:RuleViewModel) =
        let analyser =
            { Settings = settingsFromRuleViewModel viewModel
              Rules = viewModel.Rules |> Seq.map ruleViewModelToRule |> Map.ofSeq }

        (viewModel.Name, analyser)

    let private ruleViewModelToHintAnalyser (viewModel:HintsViewModel) =
        let analyser =
            { Settings = 
                Map.ofList 
                    [("Enabled", Enabled(viewModel.IsEnabled))
                     ("Hints", Hints(viewModel.Hints |> Seq.toList))]
              Rules = Map.ofList [] }
        
        ("Hints", analyser)

    let viewModelToConfig (viewModel:OptionsViewModel) =
        { UseTypeChecker = None
          IgnoreFiles = 
            { Update = IgnoreFilesUpdate.Overwrite
              Files = []
              Content = viewModel.IgnoreFiles.IgnoreFiles 
                |> String.concat Environment.NewLine } |> Some
          Analysers = 
            viewModel.Rules
            |> Seq.map ruleViewModelToAnalyser
            |> Seq.toList
            |> (fun x -> (ruleViewModelToHintAnalyser viewModel.Hints)::x)
            |> Map.ofList }

    let saveViewModelToLoadedConfigs loadedConfigs (viewModel:OptionsViewModel) =
        let config = viewModelToConfig viewModel
        let directory = viewModel.CurrentFilePath
        let normalisedDir = normalisePath directory

        let existing = getConfigForDirectory loadedConfigs directory

        let existingPartial = 
            getPartialConfig loadedConfigs normalisedDir
            |> Option.getOrTry (fun _ ->
                { UseTypeChecker = None
                  IgnoreFiles = None
                  Analysers = Map.ofList [] })

        let updatedPartial = updateConfigMap config existing existingPartial

        updateConfig loadedConfigs normalisedDir (Some updatedPartial)

    type SaveConfigResult =
    | Success
    | Failure of reason:string

    let saveViewModel loadedConfigs (viewModel: OptionsViewModel) =
        let directory = viewModel.CurrentFilePath
        let filepath = directory </> SettingsFileName
    
        match getPartialConfig loadedConfigs (normalisePath directory) with
        | Some(config) -> 
            try
                config.ToXmlDocument().Save(filepath)
                Success
            with e -> 
                Logging.logExceptionWithContext(e, sprintf "Failed to save config file to %s" filepath)
                Failure e.Message
        | None -> Success