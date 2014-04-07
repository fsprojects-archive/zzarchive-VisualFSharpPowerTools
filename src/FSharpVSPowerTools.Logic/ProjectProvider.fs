namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open FSharp.CompilerBinding
open FSharpVSPowerTools
open VSLangProj

type DependentProjects =
    | No
    | Simple
    | References

type IProjectProvider =
    abstract ProjectFileName: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerOptions: string []
    abstract SourceFiles: string []
    abstract GetLeafDependentProjects: DTE -> DependentProjects -> ProjectDescription list
     
type UniqueProjectName = string

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

        let getActiveConfigProperty (tag: string) =
            let prop = try project.ConfigurationManager.ActiveConfiguration.Properties.[tag] with _ -> null
            match prop with
            | null -> null
            | _ -> try prop.Value.ToString() with _ -> null

        let getOutputFilePath() =
            let outputPath = getActiveConfigProperty "OutputPath"
            outputPath

//        let references = 
//            (project.Object :?> VSProject).References
//            |> Seq.cast<Reference>
//            // Somethimes references are empty strings
//            |> Seq.choose (fun r -> if String.IsNullOrWhiteSpace r.Path then None else Some r.Path)
//            // Since project references are resolved automatically, we include it here
//            // Path.GetFullPath will escape path strings correctly
//            |> Seq.map (Path.GetFullPathSafe >> sprintf "-r:%s")
//            |> Seq.toArray

        let getLeafProjects (dte: DTE) (topProjects: Project list) : Set<UniqueProjectName> * Project list =
            let allProjects = 
                dte.Solution.Projects
                |> Seq.cast<Project>
                |> Seq.filter isFSharpProject
                |> Seq.toList

            let getProjectsReferenceThis (project: Project) =
                allProjects
                |> List.filter (fun p -> 
                    p.UniqueName <> project.UniqueName 
                    && p.GetReferencedProjects() |> List.exists (fun p -> p.UniqueName = project.UniqueName))

            let rec doProject (seen: Set<string>, leafs: Project list) (project: Project) : Set<UniqueProjectName> * Project list =
                let seen = seen |> Set.add project.UniqueName
                match getProjectsReferenceThis project with
                | [] -> seen, [project]
                | ps -> ps |> List.fold doProject (seen, leafs)

            let seenProjects, leafProjects = topProjects |> List.fold doProject (Set.empty, [])
            seenProjects, leafProjects |> Seq.distinctBy (fun p -> p.UniqueName) |> Seq.toList

        let compilerOptions() = 
            match getSourcesAndFlags() with
            | Some(_, flags) -> flags
            | _ -> [||]

        let sourceFiles() =
            match getSourcesAndFlags() with
            | Some(sources, _) -> sources
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

        member x.GetDescription (referencesToInclude: Set<UniqueProjectName>) =
            { ProjectFile = projectFileName
              Files = sourceFiles()
              OutputFile = getOutputFilePath()
              CompilerOptions = compilerOptions()
              Framework = targetFramework()
              References = 
                if Set.isEmpty referencesToInclude then []
                else
                    project.GetReferencedProjects()
                    |> List.choose (fun (refp: Project) -> 
                        if referencesToInclude |> Set.contains refp.UniqueName 
                        then Some (ProjectProvider(refp).GetDescription referencesToInclude)
                        else None) }

        interface IProjectProvider with
            member x.ProjectFileName = projectFileName
            member x.TargetFramework = targetFramework()
            member x.CompilerOptions = compilerOptions()
            member x.SourceFiles = sourceFiles()

            member x.GetLeafDependentProjects dte dependentProjects =
                let seenNames, leafProjects =
                    match dependentProjects with
                    | No -> Set.empty, [project]
                    | Simple -> getLeafProjects dte [project]
                    | References -> getLeafProjects dte (project.GetReferencedProjects())
                    
                let res = leafProjects |> List.map (fun p -> ProjectProvider(p).GetDescription seenNames)
                debug "!-!-! GetLeafDependentProjects: SeenProjects: %A, Leafs: %A, Result: %A"
                      seenNames (leafProjects |> List.map (fun p -> p.UniqueName)) res
                res

    type private VirtualProjectProvider (filePath: string) = 
        do Debug.Assert (filePath <> null, "FilePath should not be null.")
        let targetFramework = FSharpTargetFramework.NET_4_5
        let compilerOptions = [| "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-" |]
        let files = [| filePath |]

        interface IProjectProvider with
            member x.ProjectFileName = null
            member x.TargetFramework = targetFramework
            member x.CompilerOptions = compilerOptions
            member x.SourceFiles = files
            member x.GetLeafDependentProjects _ _ =
                [ { ProjectFile = null
                    Files = files
                    OutputFile = null
                    CompilerOptions = compilerOptions
                    Framework = targetFramework
                    References = [] } ]
    
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

    