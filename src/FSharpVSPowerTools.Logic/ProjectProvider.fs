namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open VSLangProj
open FSharp.CompilerBinding
open FSharpVSPowerTools

type IProjectProvider =
    abstract ProjectFileName: string
    abstract UniqueName: string
    abstract TargetFSharpCoreVersion: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerOptions: string array
    abstract SourceFiles: string array

type ProjectProvider(project: VSProject) = 
    do Debug.Assert(project <> null && project.Project <> null, "Input project should be well-formed.")
    
    let getProperty (tag: string) =
        let prop = try project.Project.Properties.[tag] with _ -> null
        match prop with
        | null -> null
        | _ -> prop.Value.ToString()

    let currentDir = getProperty "FullPath"
    
    let projectFileName = 
        let fileName = getProperty "FileName"
        Debug.Assert(fileName <> null && currentDir <> null, "Should have a file name for the project.")
        Path.Combine(currentDir, fileName)

    let projectOpt = ProjectParser.load projectFileName

    let references = 
        project.References
        |> Seq.cast<Reference>
        // Since project references are resolved automatically, we include it here
        // Path.GetFullPath will escape path strings correctly
        |> Seq.map (fun r -> Path.GetFullPath(r.Path))
        |> Seq.map (fun path -> sprintf "-r:%s" path)

    interface IProjectProvider with
        member x.ProjectFileName = projectFileName
        member x.UniqueName = project.Project.UniqueName

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
            match projectOpt with
            | Some p ->
                [| 
                    yield! ProjectParser.getOptionsWithoutReferences p
                    yield! references 
                |]
            | None ->
                debug "[Project System] Can't read project file. Fall back to default compiler flags."
                [|  
                   yield "--noframework"
                   yield "--debug-"
                   yield "--optimize-"
                   yield "--tailcalls-"
                   yield! references
                |]

        member x.SourceFiles = 
            match projectOpt with
            | Some p ->
                ProjectParser.getFiles p
            | None ->
                debug "[Project System] Can't read project file. Fall back to incomplete source files."
                let projectItems = project.Project.ProjectItems
                Debug.Assert (Seq.cast<ProjectItem> projectItems <> null && projectItems.Count > 0, "Should have file names in the project.")
                projectItems
                |> Seq.cast<ProjectItem>
                |> Seq.filter (fun item -> try item.Document <> null with _ -> false)
                |> Seq.choose (fun item -> 
                    let buildAction = item.Properties.["BuildAction"].Value.ToString()
                    if buildAction = "BuildAction=Compile" then Some item else None)    
                |> Seq.map (fun item -> Path.Combine(currentDir, item.Properties.["FileName"].Value.ToString()))
                |> Seq.toArray

type DummyProjectProvider (doc: Document) = 
    do Debug.Assert (doc <> null, "Input document should not be null.")
    
    interface IProjectProvider with
        member x.ProjectFileName = null
        member x.UniqueName = "DummyProject-for-" + doc.FullName
        member x.TargetFSharpCoreVersion = null
        member x.TargetFramework = FSharpTargetFramework.NET_4_5

        member x.CompilerOptions = 
            [|  
                yield "--noframework"
                yield "--debug-"
                yield "--optimize-"
                yield "--tailcalls-"
            |]

        member x.SourceFiles = [|doc.FullName|]

/// Cache of ProjectProviders. Listens for projects changes and invalidates itself.
module ProjectCache =
    type UniqueProjectName = string
    type Projects = Map<UniqueProjectName, IProjectProvider>

    type private Message = 
        | Get of Document * AsyncReplyChannel<IProjectProvider option>
        | Update of VSProject

    let projectUpdated = Event<_>()

    let private agent = MailboxProcessor.Start(fun inbox ->
        let obtainProject (projects: Projects) (vsProject: VSProject option) = 
            let project = maybe {
                let! vsProject = vsProject
                let! project = Option.ofNull vsProject.Project
                return! 
                    match projects |> Map.tryFind project.UniqueName with
                    | None -> 
                        try
                            debug "[ProjectsCache] Creating new project provider."
                            Some (ProjectProvider vsProject :> IProjectProvider)
                        with _ -> 
                            debug "[ProjectsCache] Can't find containing project. Probably the document is opened in an ad-hoc way."
                            None
                    | x -> debug "[ProjectsCache] Found cached project provider."; x }
            let projects =
                match project with
                | Some p -> projects |> Map.add p.UniqueName p
                | _ -> projects
            projects, project

        let removeProject (projects: Projects) (vsProject: VSProject) =
            match Option.ofNull vsProject.Project with
            | Some project ->
                debug "[ProjectsCache] %s has been removed from cache." project.UniqueName
                projects |> Map.remove project.UniqueName
            | None -> projects

        let rec loop (projects: Projects) = async {
            let! msg = inbox.Receive()
            match msg with
            | Get (doc, r) ->
                let projects, project = obtainProject projects doc.ProjectItem.VSProject
                let project = project |> Option.orElse (Some (DummyProjectProvider doc :> _))
                r.Reply project
                return! loop projects
            | Update vsProject -> 
                let projects = removeProject projects vsProject
                let projects, project = obtainProject projects (Some vsProject)
                project |> Option.iter projectUpdated.Trigger
                return! loop projects }
        loop Map.empty)

    agent.Error.Add (fail "%O")

    SolutionEvents.Instance.ProjectChanged.Add (fun vsProject -> agent.Post (Update vsProject))

    /// Returns ProjectProvider for given Document.
    let getProject document = agent.PostAndReply (fun r -> Get (document, r))
    /// Raised when a project is changed.
    let projectChanged = projectUpdated.Publish
