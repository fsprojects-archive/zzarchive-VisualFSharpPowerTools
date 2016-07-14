namespace FSharp.Editing.VisualStudio.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharp.Editing.Infrastructure
open FSharp.Editing
open FSharp.Editing.VisualStudio
open VSLangProj
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal ProjectProvider(project: Project, 
                              getProjectProvider: Project -> IProjectProvider option, 
                              onChanged: Project -> unit) =
    static let mutable getField = None
    do Debug.Assert(isNotNull project, "Input project should be well-formed.")
    let refAdded = _dispReferencesEvents_ReferenceAddedEventHandler (fun _ -> onChanged project)
    let refChanged = _dispReferencesEvents_ReferenceChangedEventHandler (fun _ -> onChanged project)
    let refRemoved = _dispReferencesEvents_ReferenceRemovedEventHandler (fun _ -> onChanged project)
        
    do match project.VSProject with
        | Some p -> 
            p.Events.ReferencesEvents.add_ReferenceAdded refAdded
            p.Events.ReferencesEvents.add_ReferenceChanged refChanged
            p.Events.ReferencesEvents.add_ReferenceRemoved refRemoved
        | _ -> debug "[ProjectProvider] Cannot subscribe for ReferencesEvents"
    
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
        Debug.Assert(isNotNull fileName && isNotNull currentDir, "Should have a file name for the project.")
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
        | null -> FSharpTargetFramework.NET_4_6
        | x -> 
            try
                let frameworkName = new Runtime.Versioning.FrameworkName(x)
                match frameworkName.Version.Major, frameworkName.Version.Minor with
                | 4, 6 -> FSharpTargetFramework.NET_4_6
                | 4, 5 -> FSharpTargetFramework.NET_4_5
                | 4, 0 -> FSharpTargetFramework.NET_4_0
                | 3, 5 -> FSharpTargetFramework.NET_3_5
                | 3, 0 -> FSharpTargetFramework.NET_3_0
                | 2, 0 -> FSharpTargetFramework.NET_2_0
                | _ -> invalidArg "prop" "Unsupported .NET framework version" 
            with :? ArgumentException -> 
                FSharpTargetFramework.NET_4_6)

    let sourceFiles = lazy (
        match getSourcesAndFlags() with
        | Some(sources, _) -> sources
        | _ -> [||])

    let fullOutputPath = lazy (
        maybe {
            let! fullPath = Option.ofNull (getProperty "FullPath")
            let! outputPath = Option.ofNull (getActiveConfigProperty "OutputPath")
            let! outputFileName = Option.ofNull (getProperty "OutputFileName")
            return Path.Combine (fullPath, outputPath, outputFileName) |> Path.GetFullPathSafe
        })

    let referencedProjects = lazy (project.GetReferencedFSharpProjects() |> List.choose getProjectProvider)
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
                
                let referencedProjects = 
                    referencedProjects 
                    |> Array.choose (fun (fullPath, opts) -> 
                        fullPath |> Option.map (fun x -> x, opts))
                
                let opts = 
                    languageService.GetProjectCheckerOptions (
                        projectFileName.Value, sourceFiles.Value, compilerOptions.Value, referencedProjects)
  
                (*
                // Disable due to a bug in Visual F# Tools' project system (https://visualfsharp.codeplex.com/workitem/164)
                let orphanedProjects = lazy (
                    let refProjectsOutPaths = 
                        opts.ReferencedProjects 
                        |> Array.map fst
                        |> Set.ofArray

                    opts.OtherOptions 
                    |> Seq.choose (fun x -> if x.StartsWith("-r:") then Some (x.[3..].Trim()) else None)
                    |> Set.ofSeq
                    |> Set.difference refProjectsOutPaths)

                Debug.Assert (Set.isEmpty orphanedProjects.Value, 
                    sprintf "Not all referenced projects are in the compiler options: %A" orphanedProjects.Value)
                *)

                cache := Some opts
                return opts
        }

    interface IProjectProvider with
        member __.IsForStandaloneScript = false
        member __.ProjectFileName = projectFileName.Value
        member __.TargetFramework = targetFramework.Value
        member __.CompilerVersion =
            // This option is only relevant in scripts
            None
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
     
/// A standalone project provider in order to represent script files
type internal VirtualProjectProvider (source: string, filePath: string, vsVersion) = 
    do Debug.Assert (isNotNull filePath && isNotNull source, "FilePath and source should not be null.")
    let compilerVersion = 
        match vsVersion with
        | VisualStudioVersion.VS2012 -> FSharpCompilerVersion.FSharp_3_0
        | VisualStudioVersion.VS2013 -> FSharpCompilerVersion.FSharp_3_1 
        | _ -> FSharpCompilerVersion.FSharp_4_0
    let projectFileName = filePath + ".fsproj"
    let flags = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]

    interface IProjectProvider with
        member __.IsForStandaloneScript = true
        member __.ProjectFileName = projectFileName
        member __.TargetFramework = FSharpTargetFramework.NET_4_5
        member __.CompilerVersion = Some compilerVersion
        member __.CompilerOptions = flags
        member __.SourceFiles = [| filePath |]
        member __.FullOutputFilePath = Some (Path.ChangeExtension(projectFileName, ".dll"))
        member __.GetReferencedProjects() = []
        member __.GetAllReferencedProjectFileNames() = []
        member __.GetProjectCheckerOptions languageService =
            languageService.GetScriptCheckerOptions (filePath, projectFileName, source, compilerVersion)

/// An ad-hoc project provider in order to integrate generated signatures into the project system
type internal SignatureProjectProvider (filePath: string, attachedProject: IProjectProvider) = 
    let projectFileName = filePath + ".fsproj"
    let sourceFiles = [| filePath |]
    let flags = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]

    interface IProjectProvider with
        // Although we inherit from another project, symbol-based features only work in the scope of current file.
        member __.IsForStandaloneScript = true
        member __.ProjectFileName = projectFileName
        member __.TargetFramework = attachedProject.TargetFramework
        member __.CompilerVersion = attachedProject.CompilerVersion
        member __.CompilerOptions = flags
        member __.SourceFiles = sourceFiles
        member __.FullOutputFilePath = Some (Path.ChangeExtension(projectFileName, ".dll"))
        member __.GetReferencedProjects() = []
        member __.GetAllReferencedProjectFileNames() = []
        member __.GetProjectCheckerOptions languageService =
            async {
                let! opts = attachedProject.GetProjectCheckerOptions languageService
                let refProjectsOutPaths = 
                    opts.ReferencedProjects 
                    |> Array.map fst
                    |> Set.ofArray
                let references = 
                    opts.OtherOptions
                    |> Array.choose (fun arg -> 
                            // Filter out project references, which aren't necessary for the scenario
                            if arg.StartsWith("-r:") && not (Set.contains (arg.[3..].Trim()) refProjectsOutPaths) then 
                                Some arg 
                            else None)
                return languageService.GetProjectCheckerOptions (projectFileName, sourceFiles, Array.append flags references, [||])
            }
