namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharp.CompilerBinding
open FSharpVSPowerTools
open VSLangProj
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
type ProjectDependencies =
    | No
    | Simple
    | References

type UniqueProjectName = string
type FilePath = string

type ProjectDescription =
    { ProjectFile: FilePath
      Files: FilePath[]
      CompilerOptions: string[]
      Framework: FSharpTargetFramework
      DependentProjects: ProjectDescription list }
    member x.GetProjectCheckerOptions() =
        { ProjectFileName = x.ProjectFile
          ProjectFileNames = x.Files
          ProjectOptions = x.CompilerOptions 
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = false
          LoadTime = fakeDateTimeRepresentingTimeLoaded x.ProjectFile
          UnresolvedReferences = None
          ReferencedProjects = [||] }

module ProjectGraphFormatter =
    let toGraphViz (descs: ProjectDescription list) =
        let nodeName =
            let n = ref 0
            let nodes = System.Collections.Generic.Dictionary()
            fun (d: ProjectDescription) ->
                let projectFileName = Path.GetFileNameWithoutExtension d.ProjectFile
                match nodes.TryGetValue projectFileName with
                | true, node -> node
                | _ ->
                    incr n
                    let linesCount = 
                        d.Files 
                        |> Array.filter (fun f -> Path.GetExtension(f) = ".fs")
                        |> Array.sumBy (fun f -> 
                            IO.File.ReadAllLines(f) 
                            |> Array.filter (not << String.IsNullOrWhiteSpace) 
                            |> Array.length)
                    let node = sprintf "%d (fc=%d, loc=%d)" !n d.Files.Length linesCount
                    nodes.Add(projectFileName, node)
                    node
        
        let nodes = 
            descs
            |> List.fold (fun acc desc ->
                 desc.DependentProjects 
                |> List.fold (fun res refp -> 
                    res + sprintf "\"%s\" -> \"%s\"\n" (nodeName desc) (nodeName refp)) acc) ""
        sprintf "digraph project_graph {\n%s}" nodes

[<RequireQualifiedAccess; NoComparison>]
type DeclarationLocation = 
    | File
    | Project of ProjectDescription

and IProjectProvider =
    abstract ProjectFileName: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerOptions: string []
    abstract SourceFiles: string []
    abstract GetDescription: EnvDTE.DTE -> includeRefs: bool -> ProjectDescription
    abstract GetDeclarationLocation: FSharpSymbol -> DTE -> FilePath -> DeclarationLocation option

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

    type private ProjectProvider(project: Project) as self =
        static let mutable getField = None
        do Debug.Assert(project <> null, "Input project should be well-formed.")
    
        let getProperty (tag: string) =
            let prop = try project.Properties.[tag] with _ -> null
            match prop with
            | null -> null
            | _ -> try prop.Value.ToString() with _ -> null

        let currentDir = getProperty "FullPath"
    
        let projectFileName = 
            let fileName = getProperty "FileName"
            Debug.Assert(fileName <> null && currentDir <> null, "Should have a file name for the project.")
            Path.Combine(currentDir, fileName)
    
        let getSourcesAndFlags = 
            // warning! this place is very brittle because of assumption it makes about the underlying structure of F# DTE project
            // code below will work in VS2011\VS2012 but compatibility with further versions are not guaranteed
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

        let compilerOptions() = 
            match getSourcesAndFlags() with
            | Some(_, flags) -> flags
            | _ -> [||]

        let targetFramework() =
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
                    with :? ArgumentException -> FSharpTargetFramework.NET_4_5

        member x.GetDependentProjects (dte: DTE) =
            dte.ListFSharpProjectsInSolution()
            |> List.filter (fun p -> 
                p.UniqueName <> project.UniqueName 
                && p.GetReferencedProjects() |> List.exists (fun p -> p.UniqueName = project.UniqueName))

        member x.SourceFiles() =
            match getSourcesAndFlags() with
            | Some(sources, _) -> sources
            | _ -> [||]

        member x.GetDescription (dte: EnvDTE.DTE) (includeRefs: bool) =
            { ProjectFile = projectFileName
              Files = x.SourceFiles()
              CompilerOptions = compilerOptions()
              Framework = targetFramework()
              DependentProjects = 
                if includeRefs then 
                    x.GetDependentProjects dte
                    |> List.map (fun p -> ProjectProvider(p).GetDescription dte false) 
                else [] }
            
        member x.GetDeclarationLocation (symbol: FSharpSymbol) (dte: EnvDTE.DTE) (currentFile: string) =
            let isPrivateToFile = 
                match symbol with 
                | :? FSharpMemberFunctionOrValue as m -> not m.IsModuleValueOrMember || m.Accessibility.IsPrivate
                | :? FSharpEntity as m -> m.Accessibility.IsPrivate
                | :? FSharpGenericParameter -> true
                | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
                | :? FSharpField as m -> m.Accessibility.IsPrivate
                | _ -> false
            if isPrivateToFile then Some DeclarationLocation.File 
            else 
                match symbol.TryGetLocation() with
                | Some loc ->
                    let filePath = Path.GetFullPath loc.FileName
                    if filePath = currentFile || x.SourceFiles() |> Array.exists ((=) filePath) then 
                        if Path.GetExtension currentFile = ".fsx" then Some DeclarationLocation.File
                        else Some (DeclarationLocation.Project (self.GetDescription dte true))
                    else
                        let projects = 
                            project.GetReferencedProjects()
                            |> Seq.map (fun p -> ProjectProvider(p))
                            |> Seq.filter (fun p -> p.SourceFiles() |> Array.exists ((=) filePath))
                            |> Seq.truncate 1
                            |> Seq.toList

                        match projects with
                        | [] -> None
                        | refProject :: _ -> Some (DeclarationLocation.Project (refProject.GetDescription dte true))
                | _ -> None

        interface IProjectProvider with
            member x.ProjectFileName = projectFileName
            member x.TargetFramework = targetFramework()
            member x.CompilerOptions = compilerOptions()
            member x.SourceFiles = x.SourceFiles()
            member x.GetDescription dte includeRefs = x.GetDescription dte includeRefs
            member x.GetDeclarationLocation symbol dte currentFile = x.GetDeclarationLocation symbol dte currentFile

    type private VirtualProjectProvider (filePath: string) = 
        do Debug.Assert (filePath <> null, "FilePath should not be null.")
        let targetFramework = FSharpTargetFramework.NET_4_5
        let compilerOptions = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]
        let files = [| filePath |]
        let description = 
            { ProjectFile = null    
              Files = files
              CompilerOptions = compilerOptions
              Framework = targetFramework
              DependentProjects = [] }

        interface IProjectProvider with
            member x.ProjectFileName = null
            member x.TargetFramework = targetFramework
            member x.CompilerOptions = compilerOptions
            member x.SourceFiles = files
            member x.GetDescription _ _ = description
            member x.GetDeclarationLocation _ _ _ = Some DeclarationLocation.File
    
    let createForProject (project: Project): IProjectProvider = ProjectProvider project :> _

    let createForFileInProject (filePath: string) project: IProjectProvider option =
        if not (project === null) && isFSharpProject project then
            Some (ProjectProvider(project) :> _)
        elif not (filePath === null) then
            let ext = Path.GetExtension filePath
            if String.Equals(ext, ".fsx", StringComparison.OrdinalIgnoreCase) || 
               String.Equals(ext, ".fsscript", StringComparison.OrdinalIgnoreCase) ||
               String.Equals(ext, ".fs", StringComparison.OrdinalIgnoreCase) then
                Some (VirtualProjectProvider(filePath) :> _)
            else 
                None
        else 
            None

    let createForDocument (doc: Document) =
        createForFileInProject doc.FullName doc.ProjectItem.ContainingProject

    