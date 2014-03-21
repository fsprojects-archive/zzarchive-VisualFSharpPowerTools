namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharp.CompilerBinding
open FSharpVSPowerTools

type IProjectProvider =
    abstract ProjectFileName: string
    abstract TargetFSharpCoreVersion: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerOptions: string []
    abstract SourceFiles: string []

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
            | _ -> prop.Value.ToString()

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

        interface IProjectProvider with
            member x.ProjectFileName = projectFileName

            member x.TargetFSharpCoreVersion = 
                getProperty "TargetFSharpCoreVersion"

            member x.TargetFramework = 
                match getProperty "TargetFrameworkVersion" with
                | null | "v4.5" | "v4.5.1" -> FSharpTargetFramework.NET_4_5
                | "v4.0" -> FSharpTargetFramework.NET_4_0
                | "v3.5" -> FSharpTargetFramework.NET_3_5
                | "v3.0" -> FSharpTargetFramework.NET_3_5
                | "v2.0" -> FSharpTargetFramework.NET_2_0
                | _ -> invalidArg "prop" "Unsupported .NET framework version"

            member x.CompilerOptions = 
                match getSourcesAndFlags() with
                | Some(_, flags) -> flags
                | _ -> [||]
            member x.SourceFiles = 
                match getSourcesAndFlags() with
                | Some(sources, _) -> sources
                | _ -> [||]

    type private VirtualProjectProvider (filePath: string) = 
        do Debug.Assert (filePath <> null, "FilePath should not be null.")
    
        interface IProjectProvider with
            member x.ProjectFileName = null
            member x.TargetFSharpCoreVersion = null
            member x.TargetFramework = FSharpTargetFramework.NET_4_5

            member x.CompilerOptions = 
                [|  
                    yield "--noframework"
                    yield "--debug-"
                    yield "--optimize-"
                    yield "--tailcalls-"
                |]

            member x.SourceFiles = [| filePath |]
    
    let createForProject (project: Project): IProjectProvider = 
        ProjectProvider project :> _

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

    let createForDocument (doc: Document): IProjectProvider option =
        let project = doc.ProjectItem.ContainingProject
        createForFileInProject doc.FullName project

    