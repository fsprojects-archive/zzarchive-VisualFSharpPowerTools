namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharp.CompilerBinding
open FSharpVSPowerTools
open VSLangProj
open Microsoft.FSharp.Compiler.SourceCodeServices

type FilePath = string

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation = 
    | File
    | Project of IProjectProvider

and IProjectProvider =
    abstract ProjectFileName: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerOptions: string []
    abstract SourceFiles: string []
    abstract GetReferencedProjects: unit -> IProjectProvider list

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

    type private ProjectProvider(project: Project) =
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

        let sourceFiles() =
            match getSourcesAndFlags() with
            | Some(sources, _) -> sources
            | _ -> [||]

        interface IProjectProvider with
            member x.ProjectFileName = projectFileName
            member x.TargetFramework = targetFramework()
            member x.CompilerOptions = compilerOptions()
            member x.SourceFiles = sourceFiles()
            
            member x.GetReferencedProjects() = 
                project.GetReferencedProjects() 
                |> List.map (fun p -> ProjectProvider(p) :> IProjectProvider)
            
    type private VirtualProjectProvider (filePath: string) = 
        do Debug.Assert (filePath <> null, "FilePath should not be null.")

        interface IProjectProvider with
            member x.ProjectFileName = null
            member x.TargetFramework = FSharpTargetFramework.NET_4_5
            member x.CompilerOptions = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]
            member x.SourceFiles = [| filePath |]
            member x.GetReferencedProjects() = []
    
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

    let getSymbolUsageScope (symbol: FSharpSymbol) (dte: DTE) (currentFile: FilePath) : SymbolDeclarationLocation option =
        let isPrivateToFile = 
            match symbol with 
            | :? FSharpMemberFunctionOrValue as m -> not m.IsModuleValueOrMember || m.Accessibility.IsPrivate
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
                    let allProjects = dte.ListFSharpProjectsInSolution() |> List.map (fun p -> ProjectProvider(p) :> IProjectProvider)
                    match allProjects |> List.tryFind (fun p -> p.SourceFiles |> Array.exists ((=) filePath)) with
                    | Some declarationProject -> Some (SymbolDeclarationLocation.Project declarationProject)
                    | _ -> None
            | _ -> None

    let getDependentProjects (dte: DTE) (project: IProjectProvider) =
        dte.ListFSharpProjectsInSolution()
        |> List.map (fun p -> ProjectProvider(p) :> IProjectProvider)
        |> List.filter (fun p -> 
            p.GetReferencedProjects() 
            |> List.exists (fun p -> String.Compare(p.ProjectFileName, project.ProjectFileName, true) = 0))

[<AutoOpen>]
module ProjectProviderExtensions =
    type IProjectProvider with
        member x.GetProjectCheckerOptions() =
            { ProjectFileName = x.ProjectFileName
              ProjectFileNames = x.SourceFiles
              ProjectOptions = x.CompilerOptions
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = false
              LoadTime = fakeDateTimeRepresentingTimeLoaded x.ProjectFileName
              UnresolvedReferences = None
              ReferencedProjects = [||] }
