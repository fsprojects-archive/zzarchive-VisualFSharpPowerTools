namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.Diagnostics
open EnvDTE
open VSLangProj
open FSharp.CompilerBinding

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
            Debug.WriteLine("[Project System] Can't read project file. Fall back to default compiler flags.")
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
            Debug.WriteLine("[Project System] Can't read project file. Fall back to incomplete source files.")
            let projectItems = project.Project.ProjectItems
            Debug.Assert(Seq.cast<ProjectItem> projectItems <> null && projectItems.Count > 0, "Should have file names in the project.")
            projectItems
            |> Seq.cast<ProjectItem>
            |> Seq.filter (fun item -> try item.Document <> null with _ -> false)
            |> Seq.choose (fun item -> 
                let buildAction = item.Properties.["BuildAction"].Value.ToString()
                if buildAction = "BuildAction=Compile" then Some item else None)    
            |> Seq.map (fun item -> Path.Combine(currentDir, item.Properties.["FileName"].Value.ToString()))
            |> Seq.toArray

[<AutoOpen>]
module Extensions =
    open FSharpVSPowerTools

    type Document with
        member x.Project = 
            try 
                Some (ProjectProvider (x.ProjectItem.ContainingProject.Object :?> VSProject))
            with _ -> 
                debug "Can't find containing project. Probably the document is opened in an ad-hoc way."
                None