namespace FSharpVSPowerTools.Linting

open System
open System.IO
open FSharpLint.Framework.Configuration
open Management
open FSharpVSPowerTools
open Microsoft.VisualStudio.Shell

module LintUtils =

    let private dte = lazy (Package.GetGlobalService(typeof<EnvDTE.DTE>) :?> EnvDTE.DTE)

    let getProjectPaths () =
        [ for project in dte.Value.Solution.Projects do 
            let projectFilePath = project.FullName
            if (String.IsNullOrEmpty >> not) projectFilePath then 
                let projectDirectoryPath = Path.GetDirectoryName projectFilePath
                yield normalisePath projectDirectoryPath ]

    let getSolutionPath () =
        let solutionFilePath = dte.Value.Solution.FullName
        if (String.IsNullOrEmpty >> not) solutionFilePath then 
            let solutionDirectoryPath = Path.GetDirectoryName solutionFilePath
            Some(normalisePath solutionDirectoryPath)
        else
            None

    let tryLoadConfig path = 
        let filename = 
            path 
                |> String.concat (Path.DirectorySeparatorChar.ToString())
                |> fun x -> Path.Combine(x, SettingsFileName)

        if File.Exists(filename) then
            try
                File.ReadAllText filename |> configuration |> Some
            with
                | ConfigurationException(message) ->
                    Logging.logWarning "Failed to load config file %s: %s" filename message
                    None
                | e ->
                    Logging.logWarning "Failed to load config file %s: %s" filename e.Message
                    None
        else
            None

    let updateLoadedConfigs loadedConfigs =
        updatePaths tryLoadConfig loadedConfigs (getProjectPaths())

    let getInitialLoadedConfigs () =
        updateLoadedConfigs LoadedConfigs.Empty

    let private directorySeparator = Path.DirectorySeparatorChar.ToString()

    let fromNormalisedPath = String.concat directorySeparator

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
                    yield FileViewModel(lastSegment, createFileViewModel path, hasConfig)]

        [ for (path, config) in files do
            match path with
            | [root] -> 
                let hasConfig = Option.isSome config
                yield FileViewModel(root + directorySeparator, 
                                    createFileViewModel path, 
                                    hasConfig)
            | _ -> () ]
                    
    let getInitialPath loadedConfigs =
        match getSolutionPath() with
        | Some(solutionPath) ->
            let normalisedSolutionPath = fromNormalisedPath solutionPath
            let commonToAllProjects = commonPath loadedConfigs solutionPath

            match commonToAllProjects with
            | Some(x) -> Some(fromNormalisedPath x)
            | None -> Some(normalisedSolutionPath)
        | None ->
            match getProjectPaths() with
            | firstProject::_ ->
                Some(fromNormalisedPath firstProject)
            | [] -> None