namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharpVSPowerTools
open VSLangProj
open Microsoft.VisualStudio.Text
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal ProjectProvider(project: Project, getProjectProvider: Project -> IProjectProvider, 
                              onChanged: Project -> unit, fixProjectLoadTime: ProjectOptions -> ProjectOptions) =
    static let mutable getField = None
    do Debug.Assert(project <> null, "Input project should be well-formed.")
    let refAdded = _dispReferencesEvents_ReferenceAddedEventHandler (fun _ -> onChanged project)
    let refChanged = _dispReferencesEvents_ReferenceChangedEventHandler (fun _ -> onChanged project)
    let refRemoved = _dispReferencesEvents_ReferenceRemovedEventHandler (fun _ -> onChanged project)
        
    do match project.VSProject with
        | Some p -> 
            p.Events.ReferencesEvents.add_ReferenceAdded refAdded
            p.Events.ReferencesEvents.add_ReferenceChanged refChanged
            p.Events.ReferencesEvents.add_ReferenceRemoved refRemoved
        | _ -> debug "[ProjectProvider] Cannot subsribe for ReferencesEvents"
    
    let getProperty (tag: string) =
        try 
            let prop = project.Properties.[tag]
            prop.Value.ToString() 
        with e -> 
            debug "[ProjectProvider] Cannot get property '%s' due to %O" tag e
            null

    let currentDir = getProperty "FullPath"
    
    let projectFileName = lazy (
        let fileName = getProperty "FileName"
        Debug.Assert(fileName <> null && currentDir <> null, "Should have a file name for the project.")
        Path.Combine(currentDir, fileName))
    
    let getSourcesAndFlags = 
        // Warning! This place is very brittle because of assumption it makes about the underlying structure of F# DTE project
        // Code below will work in VS2012/VS2013 but compatibility with further versions are not guaranteed
        let underlyingProjectField = project.GetType().GetField("project", Reflection.instanceNonPublic)
        let underlyingProject = underlyingProjectField.GetValue(project)

        let getter =
            match getField with
            | None ->
                let sourcesAndFlagsField = underlyingProject.GetType().GetField("sourcesAndFlags", Reflection.instanceNonPublic)
                let getter = Reflection.precompileFieldGet<option<string[] * string[]>> sourcesAndFlagsField
                getField <- Some getter
                getter
            | Some getter -> getter
        fun() -> getter underlyingProject 

    let compilerOptions = lazy (
        match getSourcesAndFlags() with
        | Some(_, flags) -> flags
        | _ -> [||])

    let getActiveConfigProperty (tag: string) =
        try 
            let prop = project.ConfigurationManager.ActiveConfiguration.Properties.[tag]
            prop.Value.ToString()
        with e -> 
            debug "[ProjectProvider] Cannot get active config '%s' due to %O" tag e
            null

    let targetFramework = lazy (
        match getProperty "TargetFrameworkMoniker" with
        | null -> FSharpTargetFramework.NET_4_5
        | x -> 
            try
                let frameworkName = new Runtime.Versioning.FrameworkName(x)
                match frameworkName.Version.Major, frameworkName.Version.Minor with
                | 4, 5 -> FSharpTargetFramework.NET_4_5
                | 4, 0 -> FSharpTargetFramework.NET_4_0
                | 3, 5 -> FSharpTargetFramework.NET_3_5
                | 3, 0 -> FSharpTargetFramework.NET_3_0
                | 2, 0 -> FSharpTargetFramework.NET_2_0
                | _ -> invalidArg "prop" "Unsupported .NET framework version" 
            with :? ArgumentException -> FSharpTargetFramework.NET_4_5)

    let sourceFiles = lazy (
        match getSourcesAndFlags() with
        | Some(sources, _) -> sources
        | _ -> [||])

    let fullOutputPath = lazy (
        Path.Combine (getProperty "FullPath", getActiveConfigProperty "OutputPath", getProperty "OutputFileName")
        |> Path.GetFullPathSafe)

    let referencedProjects = lazy (project.GetReferencedFSharpProjects() |> List.map getProjectProvider)
    let allReferencedProjects = lazy (
        project.GetReferencedProjects()
        |> List.map (fun p -> p.FileName)
        |> List.choose (fun file -> 
                if String.IsNullOrWhiteSpace file then None
                else Some(Path.GetFileNameSafe file)))

    let cache = ref None

    let getProjectCheckerOptions languageService =
        async {
            match !cache with
            | Some x -> return x
            | None ->
                let refs = referencedProjects.Value
                let! referencedProjects =
                    refs 
                    |> List.toArray
                    |> Async.Array.map (fun p -> async {
                        let! opts = p.GetProjectCheckerOptions languageService 
                        return p.FullOutputFilePath, opts })
                
                // Fix project load time to ensure each project provider instance has its correct project load time.    
                let opts = 
                    languageService.GetProjectCheckerOptions (
                        projectFileName.Value, sourceFiles.Value, compilerOptions.Value, referencedProjects)
                    |> fixProjectLoadTime 

                let orphanedProjects = lazy (
                    let refProjectsOutPaths = 
                        opts.ReferencedProjects 
                        |> Array.map fst
                        |> Set.ofArray

                    opts.ProjectOptions 
                    |> Seq.choose (fun x -> if x.StartsWith("-r:") then Some (x.[3..].Trim()) else None)
                    |> Set.ofSeq
                    |> Set.difference refProjectsOutPaths)

                Debug.Assert (Set.isEmpty orphanedProjects.Value, 
                    sprintf "Not all referenced projects are in the compiler options: %A" orphanedProjects.Value)

                //debug "[ProjectProvider] Options for %s: %A" projectFileName opts
                cache := Some opts
                return opts
        }

    interface IProjectProvider with
        member __.IsForStandaloneScript = false
        member __.ProjectFileName = projectFileName.Value
        member __.TargetFramework = targetFramework.Value
        member __.CompilerOptions = compilerOptions.Value
        member __.SourceFiles = sourceFiles.Value
        member __.FullOutputFilePath = fullOutputPath.Value
        member __.GetReferencedProjects() = referencedProjects.Value
        member __.GetAllReferencedProjectFileNames() = allReferencedProjects.Value
        member __.GetProjectCheckerOptions languageService = getProjectCheckerOptions languageService

    interface IDisposable with
        member __.Dispose() =
            project.VSProject
            |> Option.iter (fun p ->
                 p.Events.ReferencesEvents.remove_ReferenceAdded refAdded
                 p.Events.ReferencesEvents.remove_ReferenceChanged refChanged
                 p.Events.ReferencesEvents.remove_ReferenceRemoved refRemoved)
                            
type internal VirtualProjectProvider (buffer: ITextBuffer, filePath: string) = 
    do Debug.Assert (filePath <> null && buffer <> null, "FilePath and Buffer should not be null.")
    let source = buffer.CurrentSnapshot.GetText()
    let targetFramework = FSharpTargetFramework.NET_4_5
    let projectFileName = filePath + ".fsproj"

    interface IProjectProvider with
        member __.IsForStandaloneScript = true
        member __.ProjectFileName = projectFileName
        member __.TargetFramework = targetFramework
        member __.CompilerOptions = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]
        member __.SourceFiles = [| filePath |]
        member __.FullOutputFilePath = Path.ChangeExtension(filePath, ".dll")
        member __.GetReferencedProjects() = []
        member __.GetAllReferencedProjectFileNames() = []
        member __.GetProjectCheckerOptions languageService =
            languageService.GetScriptCheckerOptions (filePath, projectFileName, source, targetFramework)

        