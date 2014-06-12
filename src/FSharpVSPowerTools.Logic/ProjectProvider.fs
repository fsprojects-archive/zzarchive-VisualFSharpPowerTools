namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharpVSPowerTools
open VSLangProj
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp
open Microsoft.VisualStudio.Shell.Interop
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text

[<NoComparison; NoEquality>]
type private CacheMessage<'k, 'v> =
    | Get of 'k * (unit -> 'v) * AsyncReplyChannel<'v>
    | TryGet of 'k * AsyncReplyChannel<'v option>
    | Remove of 'k
    | Clear

type private Cache<'k, 'v when 'k: comparison>() =
    let disposeValue value =
        match box value with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop (cache: Map<'k, 'v>) = 
            async {
                let! msg = inbox.Receive()
                return! loop (
                    match msg with
                    | Get (key, creator, r) ->
                        match cache |> Map.tryFind key with
                        | Some value ->
                            debug "[Project cache] Return from cache for %A" key
                            r.Reply value
                            cache
                        | None ->
                            let value = creator()
                            debug "[Project cache] Creating new value for %A" key
                            r.Reply value
                            cache |> Map.add key value
                    | TryGet (key, r) ->
                        r.Reply (cache |> Map.tryFind key)
                        cache
                    | Remove key -> 
                        match cache |> Map.tryFind key with
                        | Some value -> 
                            disposeValue value
                            cache |> Map.remove key
                        | _ -> cache
                    | Clear -> 
                        cache |> Map.toSeq |> Seq.iter (fun (_, value) -> disposeValue value)
                        Map.empty) 
            }
        loop Map.empty)
    do agent.Error.Add (fail "%O")
    member x.Get key creator = agent.PostAndReply (fun r -> Get (key, creator, r))
    member x.TryGet key = agent.PostAndReply (fun r -> TryGet (key, r))
    member x.Remove key = agent.Post (Remove key)
    member x.Clear() = agent.Post Clear

[<AutoOpen>]
module FSharpSymbolExtensions =
    type FSharpSymbol with
        member x.TryGetLocation() = Option.orElse x.ImplementationLocation x.DeclarationLocation

type internal ProjectProvider(project: Project, getProjectProvider: Project -> IProjectProvider, onChanged: Project -> unit) =
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
        let prop = try project.Properties.[tag] with _ -> null
        match prop with
        | null -> null
        | _ -> try prop.Value.ToString() with _ -> null

    let currentDir = getProperty "FullPath"
    
    let projectFileName = lazy (
        let fileName = getProperty "FileName"
        Debug.Assert(fileName <> null && currentDir <> null, "Should have a file name for the project.")
        Path.Combine(currentDir, fileName))
    
    let getSourcesAndFlags = 
        // Warning! this place is very brittle because of assumption it makes about the underlying structure of F# DTE project
        // code below will work in VS2012\VS2013 but compatibility with further versions are not guaranteed
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
        let prop = try project.ConfigurationManager.ActiveConfiguration.Properties.[tag] with _ -> null
        match prop with
        | null -> null
        | _ -> try prop.Value.ToString() with _ -> null

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
                    
                let opts = languageService.GetProjectCheckerOptions (
                                projectFileName.Value, sourceFiles.Value, compilerOptions.Value, referencedProjects) 

                let orphanedProjects = lazy (
                    let refProjectsOutPaths = 
                        opts.ReferencedProjects 
                        |> Array.map fst
                        |> Set.ofArray

                    opts.ProjectOptions 
                    |> Seq.filter (fun x -> x.StartsWith("-r:"))
                    |> Seq.map (fun x -> x.Substring(3).Trim())
                    |> Set.ofSeq
                    |> Set.difference refProjectsOutPaths)

                Debug.Assert (Set.isEmpty orphanedProjects.Value, 
                    sprintf "Not all referenced projects are in the compiler options: %A" orphanedProjects.Value)

                //debug "[ProjectProvider] Options for %s: %A" projectFileName opts
                cache := Some opts
                return opts
        }

    interface IProjectProvider with
        member x.IsForStandaloneScript = false
        member x.ProjectFileName = projectFileName.Value
        member x.TargetFramework = targetFramework.Value
        member x.CompilerOptions = compilerOptions.Value
        member x.SourceFiles = sourceFiles.Value
        member x.FullOutputFilePath = fullOutputPath.Value
        member x.GetReferencedProjects() = referencedProjects.Value
        member x.GetAllReferencedProjectFileNames() = allReferencedProjects.Value
        member x.GetProjectCheckerOptions languageService = getProjectCheckerOptions languageService

    interface IDisposable with
        member x.Dispose() =
            project.VSProject
            |> Option.iter (fun p ->
                 p.Events.ReferencesEvents.remove_ReferenceAdded refAdded
                 p.Events.ReferencesEvents.remove_ReferenceChanged refChanged
                 p.Events.ReferencesEvents.remove_ReferenceRemoved refRemoved)
                            
type internal VirtualProjectProvider (buffer: ITextBuffer, filePath: string) = 
    do Debug.Assert (filePath <> null && buffer <> null, "FilePath and Buffer should not be null.")
    let source = buffer.CurrentSnapshot.GetText()
    let targetFramework = FSharpTargetFramework.NET_4_5

    interface IProjectProvider with
        member x.IsForStandaloneScript = true
        member x.ProjectFileName = null
        member x.TargetFramework = targetFramework
        member x.CompilerOptions = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]
        member x.SourceFiles = [| filePath |]
        member x.FullOutputFilePath = Path.ChangeExtension(filePath, ".dll")
        member x.GetReferencedProjects() = []
        member x.GetAllReferencedProjectFileNames() = []
        member x.GetProjectCheckerOptions languageService =
            languageService.GetScriptCheckerOptions (filePath, null, source, targetFramework)

type private ProjectUniqueName = string

[<Export>]
type ProjectFactory
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
     [<Import(typeof<VSLanguageService>)>] vsLanguageService: VSLanguageService) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let cache = Cache<ProjectUniqueName, ProjectProvider>()
    
    let onProjectChanged (project: Project) = 
        debug "[ProjectFactory] %s changed." project.Name
        cache.TryGet project.UniqueName
        |> Option.iter vsLanguageService.InvalidateProject
        cache.Remove project.UniqueName

    let onProjectItemChanged (projectItem: ProjectItem) =
        projectItem.VSProject |> Option.iter (fun item -> onProjectChanged item.Project)

    do match events with
        | Some events ->
            events.SolutionEvents.add_AfterClosing (fun _ -> cache.Clear())
            events.ProjectItemsEvents.add_ItemRenamed (fun p _ -> onProjectItemChanged p)
            events.ProjectItemsEvents.add_ItemRemoved (fun p -> onProjectItemChanged p)
            events.ProjectItemsEvents.add_ItemAdded (fun p -> onProjectItemChanged p)
            events.ProjectsEvents.add_ItemRemoved (fun p -> onProjectChanged p)
            events.ProjectsEvents.add_ItemRenamed (fun p _ -> onProjectChanged p)
            events.SolutionEvents.add_ProjectRemoved (fun p -> onProjectChanged p)
            events.SolutionEvents.add_ProjectRenamed (fun p _ -> onProjectChanged p) 
            debug "[ProjectFactory] Subscribed for ProjectItemsEvents"
        | _ -> fail "[ProjectFactory] Cannot subscribe for ProjectItemsEvents"

    member x.CreateForProject (project: Project): IProjectProvider = 
        cache.Get project.UniqueName (fun _ ->
            new ProjectProvider (project, x.CreateForProject, onProjectChanged)) :> _

    member x.CreateForFileInProject (buffer: ITextBuffer) (filePath: string) project: IProjectProvider option =
        if not (project === null) && not (filePath === null) && isFSharpProject project then
            let projectProvider = x.CreateForProject project
            // If current file doesn't have 'BuildAction = Compile', it doesn't appear in the list of source files 
            // Consequently, we should interprete it as a script
            if Array.exists ((=) filePath) projectProvider.SourceFiles then
                Some projectProvider
            else
                Some (VirtualProjectProvider(buffer, filePath) :> _)
        elif not (filePath === null) then
            let ext = Path.GetExtension filePath
            if String.Equals(ext, ".fsx", StringComparison.OrdinalIgnoreCase) || 
                String.Equals(ext, ".fsscript", StringComparison.OrdinalIgnoreCase) ||
                String.Equals(ext, ".fs", StringComparison.OrdinalIgnoreCase) then
                Some (VirtualProjectProvider(buffer, filePath) :> _)
            else
                None
        else 
            None

    member x.CreateForDocument buffer (doc: Document) =
        x.CreateForFileInProject buffer doc.FullName doc.ProjectItem.ContainingProject

    member x.GetSymbolUsageScope isStandalone (symbol: FSharpSymbol) (dte: DTE) (currentFile: FilePath): SymbolDeclarationLocation option =
        let isPrivateToFile = 
            match symbol with 
            | :? FSharpMemberFunctionOrValue as m -> not m.IsModuleValueOrMember
            | :? FSharpEntity as m -> m.Accessibility.IsPrivate
            | :? FSharpGenericParameter -> true
            | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
            | :? FSharpField as m -> m.Accessibility.IsPrivate
            | _ -> false
        if isPrivateToFile then 
            Some SymbolDeclarationLocation.File 
        else 
            match symbol.TryGetLocation() with
            | Some loc ->
                let filePath = Path.GetFullPath loc.FileName
                if isStandalone && filePath = currentFile then 
                    Some SymbolDeclarationLocation.File
                elif isStandalone then
                    None
                else
                    let allProjects = dte.ListFSharpProjectsInSolution() |> List.map x.CreateForProject
                    match allProjects |> List.filter (fun p -> p.SourceFiles |> Array.exists ((=) filePath)) with
                    | [] -> None
                    | projects -> Some (SymbolDeclarationLocation.Projects projects)
            | None -> None

    member x.GetDependentProjects (dte: DTE) (projects: IProjectProvider list) =
        let projectFileNames = projects |> List.map (fun p -> p.ProjectFileName.ToLower()) |> set
        dte.ListFSharpProjectsInSolution()
        |> Seq.map x.CreateForProject
        |> Seq.filter (fun p -> 
            p.GetReferencedProjects() 
            |> List.exists (fun p -> 
                projectFileNames |> Set.contains (p.ProjectFileName.ToLower())))
        |> Seq.append projects
        |> Seq.distinctBy (fun p -> p.ProjectFileName.ToLower())
        |> Seq.toList