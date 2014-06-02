namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharpVSPowerTools
open VSLangProj
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp

type FilePath = string

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation = 
    | File
    | Projects of IProjectProvider list // source file where a symbol is declared may be included into several projects

and IProjectProvider =
    abstract ProjectFileName: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerOptions: string []
    abstract SourceFiles: string []
    abstract FullOutputFilePath: string
    abstract GetReferencedProjects: unit -> IProjectProvider list
    abstract GetAllReferencedProjectFileNames: unit -> string list
    abstract GetProjectCheckerOptions: LanguageService -> Async<ProjectOptions>

[<NoComparison; NoEquality>]
type private CacheMessage<'k, 'v> =
    | Get of 'k * (unit -> 'v) * AsyncReplyChannel<'v>
    | Remove of 'k

type private Cache<'k, 'v when 'k: comparison>() =
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
                    | Remove key -> cache |> Map.remove key) 
            }
        loop Map.empty)
    do agent.Error.Add (fail "%O")
    member x.Get key creator = agent.PostAndReply (fun r -> Get (key, creator, r))
    member x.Remove key = agent.Post (Remove key)

[<AutoOpen>]
module FSharpSymbolExtensions =
    type FSharpSymbol with
        member x.TryGetLocation() = Option.orElse x.ImplementationLocation x.DeclarationLocation

module ProjectProvider =
    open System.Reflection
    
    type private Expr = System.Linq.Expressions.Expression

    let private InstanceNonPublic = BindingFlags.Instance ||| BindingFlags.NonPublic
    
    let private precompileFieldGet<'R>(f : FieldInfo) =
        let p = Expr.Parameter(typeof<obj>)
        let lambda = Expr.Lambda<Func<obj, 'R>>(Expr.Field(Expr.Convert(p, f.DeclaringType) :> Expr, f) :> Expr, p)
        lambda.Compile().Invoke

    type internal ProjectProvider(project: Project, getProjectProvider: Project -> IProjectProvider) =
        static let mutable getField = None
        do Debug.Assert(project <> null, "Input project should be well-formed.")
    
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
            let underlyingProjectField = project.GetType().GetField("project", InstanceNonPublic)
            let underlyingProject = underlyingProjectField.GetValue(project)

            let getter =
                match getField with
                | None ->
                    let sourcesAndFlagsField = underlyingProject.GetType().GetField("sourcesAndFlags", InstanceNonPublic)
                    let getter = precompileFieldGet<option<string[] * string[]>> sourcesAndFlagsField
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

        let getProjectCheckerOptions languageService =
            let cache = ref None
            async {
                match !cache with
                | Some x -> return x
                | _ ->
                    let refs = referencedProjects.Value
                    let! referencedProjects =
                        refs 
                        |> List.map (fun p -> async {
                            let! opts = p.GetProjectCheckerOptions languageService 
                            return p.FullOutputFilePath, opts })
                        |> Async.Parallel
                    
                    let opts = languageService.GetProjectCheckerOptions (
                                   projectFileName.Value, sourceFiles.Value, compilerOptions.Value, referencedProjects) 

                    let refProjectsOutPaths = 
                        opts.ReferencedProjects 
                        |> Array.map fst
                        |> Set.ofArray

                    let orphanedProjects =
                        opts.ProjectOptions 
                        |> Seq.filter (fun x -> x.StartsWith("-r:"))
                        |> Seq.map (fun x -> x.Substring(3).Trim())
                        |> Set.ofSeq
                        |> Set.difference refProjectsOutPaths

                    Debug.Assert (
                        Set.isEmpty orphanedProjects, 
                        sprintf "Not all referenced projects are in the compiler options: %A" orphanedProjects)

                    //debug "[ProjectProvider] Options for %s: %A" projectFileName opts
                    cache := Some opts
                    return opts
            }

        interface IProjectProvider with
            member x.ProjectFileName = projectFileName.Value
            member x.TargetFramework = targetFramework.Value
            member x.CompilerOptions = compilerOptions.Value
            member x.SourceFiles = sourceFiles.Value
            member x.FullOutputFilePath = fullOutputPath.Value
            member x.GetReferencedProjects() = referencedProjects.Value
            member x.GetAllReferencedProjectFileNames() = allReferencedProjects.Value
            member x.GetProjectCheckerOptions languageService = getProjectCheckerOptions languageService
                            
    type internal VirtualProjectProvider (filePath: string) = 
        do Debug.Assert (filePath <> null, "FilePath should not be null.")
        let source = File.ReadAllText filePath
        let targetFramework = FSharpTargetFramework.NET_4_5

        interface IProjectProvider with
            member x.ProjectFileName = null
            member x.TargetFramework = targetFramework
            member x.CompilerOptions = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]
            member x.SourceFiles = [| filePath |]
            member x.FullOutputFilePath = Path.ChangeExtension(filePath, ".dll")
            member x.GetReferencedProjects() = []
            member x.GetAllReferencedProjectFileNames() = []
            member x.GetProjectCheckerOptions languageService =
                languageService.GetScriptCheckerOptions (filePath, null, source, targetFramework)
    
open Microsoft.VisualStudio.Shell.Interop
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell

type ProjectFactory
    [<ImportingConstructor>] ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let cache = Cache<string, ProjectProvider.ProjectProvider>()
 
    let onProjectChanged (projectItem: ProjectItem) =
        projectItem.VSProject
        |> Option.iter (fun item ->
                debug "[ProjectsCache] %s changed." projectItem.Name
                cache.Remove item.Project.UniqueName)
 
    do match events with
        | Some events ->
            events.ProjectItemsEvents.add_ItemRenamed (fun p _ -> onProjectChanged p)
            events.ProjectItemsEvents.add_ItemRemoved (fun p -> onProjectChanged p)
            events.ProjectItemsEvents.add_ItemAdded (fun p -> onProjectChanged p)
            debug "[SolutionEvents] Subscribed for ProjectItemsEvents"
        | _ -> fail "[SolutionEvents] Cannot subscribe for ProjectItemsEvents"

    member x.CreateForProject (project: Project): IProjectProvider = 
        cache.Get project.UniqueName (fun _ -> ProjectProvider.ProjectProvider (project, x.CreateForProject)) :> _

    member x.CreateForFileInProject (filePath: string) project: IProjectProvider option =
        if not (project === null) && not (filePath === null) && isFSharpProject project then
            let projectProvider = x.CreateForProject project
            /// If current file doesn't have 'BuildAction = Compile', it doesn't appear in the list of source files 
            /// Consequently, we should interprete it as a script
            if Array.exists ((=) filePath) projectProvider.SourceFiles then
                Some projectProvider
            else
                Some (ProjectProvider.VirtualProjectProvider(filePath) :> _)
        elif not (filePath === null) then
            let ext = Path.GetExtension filePath
            if String.Equals(ext, ".fsx", StringComparison.OrdinalIgnoreCase) || 
                String.Equals(ext, ".fsscript", StringComparison.OrdinalIgnoreCase) ||
                String.Equals(ext, ".fs", StringComparison.OrdinalIgnoreCase) then
                Some (ProjectProvider.VirtualProjectProvider(filePath) :> _)
            else 
                None
        else 
            None

    member x.CreateForDocument (doc: Document) =
        x.CreateForFileInProject doc.FullName doc.ProjectItem.ContainingProject

    member x.GetSymbolUsageScope (symbol: FSharpSymbol) (dte: DTE) (currentFile: FilePath) : SymbolDeclarationLocation option =
        let isPrivateToFile = 
            match symbol with 
            | :? FSharpMemberFunctionOrValue as m -> not m.IsModuleValueOrMember
            | :? FSharpEntity as m -> m.Accessibility.IsPrivate
            | :? FSharpGenericParameter -> true
            | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
            | :? FSharpField as m -> m.Accessibility.IsPrivate
            | _ -> false
        if isPrivateToFile then Some SymbolDeclarationLocation.File 
        else 
            match symbol.TryGetLocation() with
            | Some loc ->
                let filePath = Path.GetFullPath loc.FileName
                if filePath = currentFile && Path.GetExtension currentFile = ".fsx" then 
                    Some SymbolDeclarationLocation.File
                else
                    let allProjects = dte.ListFSharpProjectsInSolution() |> List.map x.CreateForProject
                    match allProjects |> List.filter (fun p -> p.SourceFiles |> Array.exists ((=) filePath)) with
                    | [] -> None
                    | projects -> Some (SymbolDeclarationLocation.Projects projects)
            | _ -> None

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