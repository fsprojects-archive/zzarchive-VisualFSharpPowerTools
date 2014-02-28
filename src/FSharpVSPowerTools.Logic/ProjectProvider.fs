namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open VSLangProj
open FSharp.CompilerBinding
open FSharpVSPowerTools

type ProjectProvider(project : VSProject) = 
    do Debug.Assert(project <> null && project.Project <> null, "Input project should be well-formed.")
    
    let getProperty (tag : string) =
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

    member __.ProjectFileName = projectFileName
    member __.UniqueName = project.Project.UniqueName

    member __.TargetFSharpCoreVersion = 
        getProperty "TargetFSharpCoreVersion"

    member __.TargetFramework = 
        match getProperty "TargetFrameworkVersion" with
        | null | "v4.5" | "v4.5.1" -> FSharpTargetFramework.NET_4_5
        | "v4.0" -> FSharpTargetFramework.NET_4_0
        | "v3.5" -> FSharpTargetFramework.NET_3_5
        | "v3.0" -> FSharpTargetFramework.NET_3_5
        | "v2.0" -> FSharpTargetFramework.NET_2_0
        | _ -> invalidArg "prop" "Unsupported .NET framework version"

    member private __.References = 
        project.References
        |> Seq.cast<Reference>
        // Since project references are resolved automatically, we include it here
        // Path.GetFullPath will escape path strings correctly
        |> Seq.map (fun r -> Path.GetFullPath(r.Path))
        |> Seq.map (fun path -> sprintf "-r:%s" path)

    member this.CompilerOptions = 
        match projectOpt with
        | Some p ->
            [| 
                yield! ProjectParser.getOptionsWithoutReferences p
                yield! this.References 
            |]
        | None ->
            debug "[Project System] Can't read project file. Fall back to default compiler flags."
            [|  
               yield "--noframework"
               yield "--debug-"
               yield "--optimize-"
               yield "--tailcalls-"
               yield! this.References
            |]

    member __.SourceFiles = 
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

/// Cache of ProjectProviders. Listens for projects changes and invalidates itself.
module ProjectsCache =
    type private Message = 
        | Get of Document * AsyncReplyChannel<ProjectProvider option>
        | Remove of VSProject

    let private agent = MailboxProcessor.Start(fun inbox ->
        let rec loop (projects: Map<string, ProjectProvider>) = async {
            let! msg = inbox.Receive()
            match msg with
            | Get (doc, r) ->
                let project =
                    doc.ProjectItem.VSProject
                    |> Option.bind (fun vsProject ->
                            match projects |> Map.tryFind vsProject.Project.UniqueName with
                            | None -> 
                                try
                                    debug "[ProjectsCache] Creating new project provider."
                                    Some (ProjectProvider (vsProject))
                                with _ -> 
                                    debug "[ProjectsCache] Can't find containing project. Probably the document is opened in an ad-hoc way."
                                    None
                            | x -> debug "[ProjectsCache] Found cached project provider."; x)

                r.Reply project
                let projects =
                    match project with
                    | Some p -> projects |> Map.add p.UniqueName p
                    | _ -> projects
                return! loop projects
            | Remove vsProject -> 
                let projects =
                    match Option.ofNull vsProject.Project with
                    | Some project ->
                        debug "[ProjectsCache] %s has been removed from cache." project.UniqueName
                        projects |> Map.remove project.UniqueName
                    | None -> projects
                return! loop projects }
        loop Map.empty)

    SolutionEvents.Instance.ProjectChanged.Add (fun vsProject -> agent.Post (Remove vsProject))

    /// Returns ProjectProvider for given Documents (it caches ProjectProviders forever for now).
    let getProject document = agent.PostAndReply (fun r -> Get (document, r))
