namespace FSharp.Editing.VisualStudio.ProjectSystem

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open System.ComponentModel.Composition
open System.Runtime.InteropServices
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open EnvDTE
open FSharp.Editing
open FSharp.Editing.Infrastructure
open FSharp.Editing.VisualStudio

type private Cache<'K, 'V when 'K: comparison>() =
    let cache = ConcurrentDictionary<'K, 'V>()

    let disposeValue value =
        match box value with
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    member __.Get key (creator: unit -> 'V) = cache.GetOrAdd (key, valueFactory = fun _ -> creator())
    
    member __.TryGet key =
        match cache.TryGetValue key with
        | true, value -> Some value
        | _ -> None
    
    member __.Remove key = 
        match cache.TryRemove key with
        | true, value -> disposeValue value
        | _ -> ()
    
    member __.Clear() = 
        cache |> Seq.iter (fun x -> disposeValue x.Value)
        cache.Clear()

type private ProjectUniqueName = string

[<Export>]
type ProjectFactory
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
     openDocumentsTracker: IOpenDocumentsTracker,
     vsLanguageService: VSLanguageService) =
    let dte = serviceProvider.GetService<DTE, SDTE>()

    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let cache = Cache<ProjectUniqueName, ProjectProvider>()

    let runningDocumentTable = lazy(serviceProvider.GetService<IVsRunningDocumentTable, SVsRunningDocumentTable>())

    let mayReferToSameBuffer (buffer: ITextBuffer) filePath =
        match openDocumentsTracker.TryFindOpenDocument(filePath) with
        | None -> true
        | Some doc ->
            buffer = doc.Document.TextBuffer

    let onProjectChanged (project: Project) = 
        debug "[ProjectFactory] %s changed." project.Name
        cache.Remove project.FullName

    let onProjectItemChanged (projectItem: ProjectItem) =
        projectItem.VSProject |> Option.iter (fun item -> 
            cache.TryGet item.Project.FullName
            |> Option.iter (vsLanguageService.InvalidateProject >> Async.RunSynchronously) 
            onProjectChanged item.Project)

    let signatureProjectData = Dictionary()
    let fsharpProjectsCache = ref None

    let clearCaches() =
        signatureProjectData.Clear()
        fsharpProjectsCache := None
        cache.Clear()
        vsLanguageService.ClearCaches()

    let solutionBuildEventListener = new SolutionBuildEventListener(serviceProvider)
    // When active configuration changes, all project providers are stale so we clear our own caches
    do solutionBuildEventListener.ActiveConfigChanged.Add(fun _ -> clearCaches())

    do match events with
        | Some events ->
            events.SolutionEvents.add_AfterClosing (fun _ -> clearCaches())
            events.ProjectItemsEvents.add_ItemRenamed (fun p _ -> onProjectItemChanged p)
            events.ProjectItemsEvents.add_ItemRemoved (fun p -> onProjectItemChanged p)
            events.ProjectItemsEvents.add_ItemAdded (fun p -> onProjectItemChanged p)
            events.ProjectsEvents.add_ItemRemoved (fun p -> onProjectChanged p)
            events.ProjectsEvents.add_ItemRenamed (fun p _ -> onProjectChanged p)

            events.SolutionEvents.add_ProjectAdded (fun _ -> 
                fsharpProjectsCache := None)
            events.SolutionEvents.add_ProjectRemoved (fun p -> 
                fsharpProjectsCache := None
                onProjectChanged p)
            events.SolutionEvents.add_ProjectRenamed (fun p _ -> 
                fsharpProjectsCache := None
                onProjectChanged p) 
            debug "[ProjectFactory] Subscribed for ProjectItemsEvents"
        | _ -> fail "[ProjectFactory] Cannot subscribe for ProjectItemsEvents"

    member __.RegisterSignatureProjectProvider (filePath: string) (signature: string, project) =
        // We have to keep the project provider even the buffer has been close.
        // If the buffer is reopened via Navigate Backward, we still have to colorize the text.
        // The project provider will be discard once the solution is closed.
        let signatureProject = SignatureProjectProvider(filePath, project) :> IProjectProvider
        signatureProjectData.[filePath] <- (signature, signatureProject)
        signatureProject

    member __.TryGetSignatureProjectProvider(filePath: string) =
        match signatureProjectData.TryGetValue(filePath) with
        | true, project ->
            Some project
        | _ -> None

    abstract CreateForProject: Project -> IProjectProvider

    default x.CreateForProject (project: Project): IProjectProvider = 
        let createProjectProvider project = Some (x.CreateForProject project)
        cache.Get project.FullName (fun _ ->
            new ProjectProvider (project, createProjectProvider, onProjectChanged)) :> _

    member x.CreateForDocument (buffer: ITextBuffer) (filePath: FilePath) =
        maybe {
            let project = tryFindProject runningDocumentTable.Value filePath |> Option.bind Option.ofNull
            Debug.Assert(mayReferToSameBuffer buffer filePath, sprintf "Buffer '%A' doesn't refer to the current document '%s'." buffer filePath)
            
            let getText (buffer: ITextBuffer) =
                // Try to obtain cached document text; otherwise, retrieve the text from the buffer.
                openDocumentsTracker.TryGetDocumentText filePath
                |> Option.getOrTry (fun _ -> buffer.CurrentSnapshot.GetText())

            match project, filePath with
            | _, null -> 
                return! None
            | Some project, _ when isFSharpProject project ->            
                let projectProvider = x.CreateForProject project
                // If current file doesn't have 'BuildAction = Compile', it doesn't appear in the list of source files. 
                // Consequently, we should interpret it as a script.
                if Array.exists ((=) filePath) projectProvider.SourceFiles then
                    return projectProvider
                else
                    if isSourceFile filePath then
                        let vsVersion = VisualStudioVersion.fromDTEVersion dte.Version
                        return (VirtualProjectProvider(getText buffer, filePath, vsVersion) :> _)
                    else
                        return! None
            | _ ->
                if isSourceFile filePath then
                    let vsVersion = VisualStudioVersion.fromDTEVersion dte.Version
                    return (VirtualProjectProvider(getText buffer, filePath, vsVersion) :> _)
                elif isSignatureFile filePath then
                    match signatureProjectData.TryGetValue(filePath) with
                    | true, (_, project) -> return project
                    | _ -> return! None
                else return! None
        }

    member __.ListFSharpProjectsInSolution dte =
        match !fsharpProjectsCache with
        | Some x -> x
        | None ->
            let res = listFSharpProjectsInSolution dte
            fsharpProjectsCache := Some res
            res

    member x.GetSymbolDeclarationLocation (symbol: FSharpSymbol) (currentFile: FilePath) (currentProject: IProjectProvider) =
        Debug.Assert(currentProject.SourceFiles |> Array.exists ((=) currentFile), 
            sprintf "Current file '%s' should be included in current project '%A'." currentFile currentProject.SourceFiles)
        let isPrivateToFile = 
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember
            | :? FSharpEntity as m -> m.Accessibility.IsPrivate
            | :? FSharpGenericParameter -> true
            | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
            | :? FSharpField as m -> m.Accessibility.IsPrivate
            | _ -> false

        if isPrivateToFile then 
            Some SymbolDeclarationLocation.File 
        else
            let isSymbolLocalForProject = TypedAstUtils.isSymbolLocalForProject symbol 
            match Option.orElse symbol.ImplementationLocation symbol.DeclarationLocation with
            | Some loc ->
                //Logging.logInfo (fun _ -> sprintf "Trying to find symbol '%O' declared at '%O' from current file '%O'..." symbol loc.FileName currentFile)
                let filePath = Path.GetFullPathSafe loc.FileName
                if currentProject.IsForStandaloneScript && filePath = currentFile then 
                    Some SymbolDeclarationLocation.File
                elif currentProject.IsForStandaloneScript then
                    // The standalone script might include other files via '#load'
                    // These files appear in project options and the standalone file 
                    // should be treated as an individual project
                    Some (SymbolDeclarationLocation.Projects ([currentProject], isSymbolLocalForProject))
                else
                    let allProjects = x.ListFSharpProjectsInSolution dte |> List.map x.CreateForProject
                    let allProjectFileNames =
                        lazy (allProjects |> List.map (fun p -> Path.GetFullPathSafe(p.Project.ProjectFile)))
                    Debug.Assert(
                        allProjectFileNames.Value |> List.exists (fun projectFileName -> 
                                                        Path.GetFullPathSafe(currentProject.Project.ProjectFile) = projectFileName), 
                        sprintf "Current project '%s' should appear in the project list '%A'." currentProject.Project.ProjectFile allProjectFileNames.Value)
                    match allProjects |> List.filter (fun p -> p.SourceFiles |> Array.exists ((=) filePath)) with
                    | [] -> None
                    | projects -> Some (SymbolDeclarationLocation.Projects (projects, isSymbolLocalForProject))
            | None -> None

    member x.GetDependentProjects (dte: DTE) (projects: IProjectProvider list) =
        let projectFileNames = projects |> List.map (fun p -> p.Project.ProjectFile.ToLowerInvariant()) |> set
        x.ListFSharpProjectsInSolution dte
        |> Seq.map x.CreateForProject
        |> Seq.filter (fun p -> 
            p.GetReferencedProjects() 
            |> List.exists (fun p -> 
                projectFileNames |> Set.contains (p.Project.ProjectFile.ToLowerInvariant())))
        |> Seq.append projects
        |> Seq.distinctBy (fun p -> p.Project.ProjectFile.ToLowerInvariant())
        |> Seq.toList

    interface IDisposable with
        member __.Dispose() = 
            (solutionBuildEventListener :> IDisposable).Dispose()
