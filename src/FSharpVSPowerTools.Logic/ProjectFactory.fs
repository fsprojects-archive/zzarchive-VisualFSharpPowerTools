namespace FSharpVSPowerTools.ProjectSystem

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.Shell.Interop
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open EnvDTE
open FSharpVSPowerTools
open Microsoft.VisualStudio.Text
open System.IO
open System.Diagnostics

[<NoComparison; NoEquality>]
type private CacheMessage<'K, 'V> =
    | Get of 'K * (unit -> 'V) * AsyncReplyChannel<'V>
    | TryGet of 'K * AsyncReplyChannel<'V option>
    | Remove of 'K
    | Clear

type private Cache<'K, 'V when 'K: comparison>() =
    let disposeValue value =
        match box value with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop (cache: Map<'K, 'V>) = 
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
    member __.Get key creator = agent.PostAndReply (fun r -> Get (key, creator, r))
    member __.TryGet key = agent.PostAndReply (fun r -> TryGet (key, r))
    member __.Remove key = agent.Post (Remove key)
    member __.Clear() = agent.Post Clear

type private ProjectUniqueName = string

[<Export>]
type ProjectFactory
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
     vsLanguageService: VSLanguageService) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let cache = Cache<ProjectUniqueName, ProjectProvider>()

    let onProjectChanged (project: Project) = 
        debug "[ProjectFactory] %s changed." project.Name
        cache.Remove project.FullName

    let onProjectItemChanged (projectItem: ProjectItem) =
        projectItem.VSProject |> Option.iter (fun item -> 
            cache.TryGet item.Project.FullName
            |> Option.iter (vsLanguageService.InvalidateProject >> Async.RunSynchronously) 
            onProjectChanged item.Project)

    let solutionBuildEventListener = new SolutionBuildEventListener(serviceProvider)
    do solutionBuildEventListener.ActiveConfigChanged.Add(fun p -> cache.Remove p.FullName)

    let fsharpProjectsCache = ref None
 
    do match events with
        | Some events ->
            events.SolutionEvents.add_AfterClosing (fun _ -> 
                vsLanguageService.ClearCaches()
                cache.Clear())
            events.ProjectItemsEvents.add_ItemRenamed (fun p _ -> onProjectItemChanged p)
            events.ProjectItemsEvents.add_ItemRemoved (fun p -> onProjectItemChanged p)
            events.ProjectItemsEvents.add_ItemAdded (fun p -> onProjectItemChanged p)
            events.ProjectsEvents.add_ItemRemoved (fun p -> onProjectChanged p)
            events.ProjectsEvents.add_ItemRenamed (fun p _ -> onProjectChanged p)
            events.SolutionEvents.add_ProjectAdded (fun _ -> fsharpProjectsCache := None)
            events.SolutionEvents.add_ProjectRemoved (fun p -> 
                fsharpProjectsCache := None
                onProjectChanged p)
            events.SolutionEvents.add_ProjectRenamed (fun p _ -> 
                fsharpProjectsCache := None
                onProjectChanged p) 
            debug "[ProjectFactory] Subscribed for ProjectItemsEvents"
        | _ -> fail "[ProjectFactory] Cannot subscribe for ProjectItemsEvents"

    abstract CreateForProject: Project -> IProjectProvider

    default x.CreateForProject (project: Project): IProjectProvider = 
        cache.Get project.FullName (fun _ ->
            new ProjectProvider (project, x.CreateForProject, onProjectChanged)) :> _

    member x.CreateForFileInProject (buffer: ITextBuffer) (filePath: string) project: IProjectProvider option =
        if not (project === null) && not (filePath === null) && isFSharpProject project then
            let projectProvider = x.CreateForProject project
            // If current file doesn't have 'BuildAction = Compile', it doesn't appear in the list of source files 
            // Consequently, we should interpret it as a script
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

    member __.ListFSharpProjectsInSolution (dte: DTE) =
        let rec handleProject (p: Project) = 
            if p === null then []
            elif isFSharpProject p then [ p ]
            elif p.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then handleProjectItems p.ProjectItems
            else []
        
        and handleProjectItems (items: ProjectItems) = 
            [ for pi in items do
                    yield! handleProject pi.SubProject ]

        match !fsharpProjectsCache with
        | Some x -> x
        | None ->
            let res = [ for p in dte.Solution.Projects do
                            yield! handleProject p ]
            fsharpProjectsCache := Some res
            res

    member x.GetSymbolDeclarationLocation (symbol: FSharpSymbol) (currentFile: FilePath) (currentProject: IProjectProvider) : SymbolDeclarationLocation option =
        Debug.Assert(currentProject.SourceFiles |> Array.exists ((=) currentFile), "Current file should be included in current project.")
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
                let filePath = Path.GetFullPathSafe loc.FileName
                if currentProject.IsForStandaloneScript && filePath = currentFile then 
                    Some SymbolDeclarationLocation.File
                elif currentProject.IsForStandaloneScript then
                    // The standalone script might include other files via '#load'
                    // These files appear in project options and the standalone file 
                    // should be treated as an individual project
                    Some (SymbolDeclarationLocation.Projects [currentProject])
                else
                    let allProjects = x.ListFSharpProjectsInSolution dte |> List.map x.CreateForProject
                    Debug.Assert(allProjects |> List.exists (fun p -> p.ProjectFileName = currentProject.ProjectFileName), "Current project should appear in the project list.")
                    match allProjects |> List.filter (fun p -> p.SourceFiles |> Array.exists ((=) filePath)) with
                    | [] -> None
                    | projects -> Some (SymbolDeclarationLocation.Projects projects)
            | None -> None

    member x.GetDependentProjects (dte: DTE) (projects: IProjectProvider list) =
        let projectFileNames = projects |> List.map (fun p -> p.ProjectFileName.ToLower()) |> set
        x.ListFSharpProjectsInSolution dte
        |> Seq.map x.CreateForProject
        |> Seq.filter (fun p -> 
            p.GetReferencedProjects() 
            |> List.exists (fun p -> 
                projectFileNames |> Set.contains (p.ProjectFileName.ToLower())))
        |> Seq.append projects
        |> Seq.distinctBy (fun p -> p.ProjectFileName.ToLower())
        |> Seq.toList

    interface IDisposable with
        member __.Dispose() = 
            (solutionBuildEventListener :> IDisposable).Dispose()


type BasicProjectFactory(events: EnvDTE80.Events2) =
    let fsharpProjectsCache = ref None
    let cache = Cache<ProjectUniqueName, ProjectProvider>()

    let onProjectChanged (project: Project) = 
        debug "[ProjectFactory] %s changed." project.Name
        cache.Remove project.FullName

    do events.SolutionEvents.add_AfterClosing (fun _ -> 
        fsharpProjectsCache := None
        cache.Clear())

    member __.ListFSharpProjectsInSolution(dte: DTE) =
        let rec handleProject (p: Project) =
            if p === null then []
            elif isFSharpProject p then [ p ]
            elif p.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then handleProjectItems p.ProjectItems
            else []
        
        and handleProjectItems (items: ProjectItems) = 
            [ for pi in items do
                    yield! handleProject pi.SubProject ]

        match !fsharpProjectsCache with
        | Some x -> x
        | None ->
            let res = [ for p in dte.Solution.Projects do
                            yield! handleProject p ]
            fsharpProjectsCache := Some res
            res

    member x.CreateForProject(project: Project): IProjectProvider = 
        cache.Get project.FullName (fun _ ->
            new ProjectProvider (project, x.CreateForProject, onProjectChanged)) :> _