namespace FSharpVSPowerTools.Reference

open FSharpVSPowerTools
open EnvDTE80
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open System
open EnvDTE
open FSharpVSPowerTools.ProjectSystem
open System.IO
open System.Collections.Generic
open VSLangProj
open System.ComponentModel.Design
open System.Text
open Microsoft.VisualStudio
open System.Windows
open Microsoft.VisualStudio.Text.Editor
open System.Reflection
open System.Globalization

type FsiReferenceCommand(dte2: DTE2, mcs: OleMenuCommandService, shell: IVsUIShell) =
    static let scriptFolderName = "Scripts"
    
    let containsReferenceScript (project: Project) = 
        project.ProjectItems.Item(scriptFolderName) 
        |> Option.ofNull
        |> Option.bind (fun scriptItem -> Option.ofNull scriptItem.ProjectItems)
        |> Option.map (fun projectItems -> 
            seq {
                for i in 0..projectItems.Count-1 ->
                    projectItems.Item(i + 1)
            }
            |> Seq.exists (fun item -> item.Name.Contains("load-references")))
        |> Option.getOrElse false

    let getActiveProject() =
        let dte = dte2 :?> DTE
        dte.ActiveSolutionProjects :?> obj[]
        |> Seq.tryHead
        |> Option.map (fun o -> o :?> Project)

    let getProjectFolder(project: Project) =
        project.Properties.Item("FullPath").Value.ToString()

    let addFileToActiveProject(project: Project, fileName: string, content: string) = 
        if isFSharpProject project then
            // Create Script folder
            let projectFolder = getProjectFolder project
            let scriptFolder = Path.Combine(projectFolder, scriptFolderName)
            if not (Directory.Exists scriptFolder) then
                Directory.CreateDirectory(scriptFolder) |> ignore

            let textFile = Path.Combine(scriptFolder, fileName)
            use writer = File.CreateText(textFile)
            writer.Write(content)

            let projectFolderScript = 
                if project.ProjectItems.Item(scriptFolderName) <> null then
                    project.ProjectItems.Item(scriptFolderName)
                else 
                    project.ProjectItems.AddFolder(scriptFolderName)
            projectFolderScript.ProjectItems.AddFromFile(textFile) |> ignore
            project.Save()

    let getProjectOutputs (project: Project) =
        let dict = Dictionary()
        let projectPath = getProjectFolder project
        let outputFileName = project.Properties.Item("OutputFileName").Value.ToString()
        seq {
            for i in 0..project.ConfigurationManager.Count-1 ->
                project.ConfigurationManager.Item(i + 1)
        }
        |> Seq.iter (fun config ->
                let outputPath = config.Properties.Item("OutputPath").Value.ToString()
                let p = Path.Combine(Path.Combine(projectPath, outputPath), outputFileName)
                dict.Add(config.ConfigurationName, p))
        dict

    let getOutputFileFullPaths (reference: Reference) =
        reference.GetType().GetProperty("SourceProject")
        |> Option.ofNull
        |> Option.map (fun sourceProject -> sourceProject.GetValue(reference) :?> Project)
        |> Option.map getProjectOutputs

    let generateLoadScriptContent(project: Project, scriptFile: string, tag: string) =
        let projectfolder = Path.Combine(getProjectFolder project, scriptFolderName)
        let load = String.Format("#load @\"{0}\"", Path.Combine(projectfolder, scriptFile))
        let outputs = getProjectOutputs project
        match outputs.TryGetValue(tag) with
        | true, output ->
            let result = String.Format("#r @\"{0}\"\r\n", output)
            String.Format("{0}\r\n{1}", load, result)
        | _ ->
            String.Format("{0}\r\n", load)

    let isReferenceProject (reference: Reference) =
        let sourceProject = reference.GetType().GetProperty("SourceProject")
        sourceProject <> null && sourceProject.GetValue(reference) <> null

    let toReferenceSeq (references: References) =
        seq {
            for i in 0..references.Count-1 ->
                references.Item(i + 1)
        }

    let generateFileContent(project: Project, tag: string) =
        let excludingList = set [| "FSharp.Core"; "mscorlib" |]

        let assemblyRefList = ResizeArray()
        let projectRefList = ResizeArray()

        match project.Object with
        | :? VSProject as vsProject ->
            vsProject.References
            |> toReferenceSeq
            |> Seq.iter (fun reference -> 
                    if not (excludingList.Contains reference.Name) then
                        if isReferenceProject reference then
                            getOutputFileFullPaths reference
                            |> Option.iter (fun outputFilePaths ->
                                match outputFilePaths.TryGetValue(tag) with
                                | true, path ->
                                    projectRefList.Add(path)
                                | _ -> ())
                        else
                            let fullPath = reference.Path
                            if File.Exists fullPath then
                                assemblyRefList.Add(fullPath))
        | _ -> ()

        let assemblyRefList = assemblyRefList |> Seq.map (fun reference -> String.Format("#r @\"{0}\"", reference)) |> Seq.toList
        let projectRefList = projectRefList |> Seq.map (fun reference -> String.Format("#r @\"{0}\"", reference)) |> Seq.toList
        String.Format("// Warning: Generated file, your change could be lost when new file is generated. \r\n{0}\r\n\r\n{1}",
                    String.Join("\r\n", assemblyRefList),
                    String.Join("\r\n", projectRefList))

    let generateFile (project: Project) =
        Option.ofNull project
        |> Option.map getProjectOutputs
        |> Option.iter (fun outputs -> 
            outputs
            |> Seq.iter (fun (KeyValue(tag, _)) ->
                 let fileName = if tag = "Debug" then "load-references.fsx" else String.Format("load-references-{0}.fsx", tag)
                 let content = generateFileContent(project, tag)
                 addFileToActiveProject(project, fileName, content)
                 let content = generateLoadScriptContent(project, fileName, tag)
                 let fileName = if tag = "Debug" then "load-project.fsx" else String.Format("load-project-{0}.fsx", tag)
                 addFileToActiveProject(project, fileName, content)))
    
    let getReferences (project: Project) =
        let excludingList = set [| "FSharp.Core"; "mscorlib" |]

        let assemblyRefList = ResizeArray()

        match project.Object with
        | :? VSProject as vsProject ->
            vsProject.References
            |> toReferenceSeq
            |> Seq.iter (fun reference -> 
                    if not (excludingList.Contains reference.Name) then
                        let fullPath = reference.Path
                        if File.Exists fullPath then
                            assemblyRefList.Add(fullPath))
        | _ -> ()

        assemblyRefList

    let addReferenceInFSI() =
        let fsiWindowGuid = ref (Guid("dee22b65-9761-4a26-8fb2-759b971d6dfc"))
        match shell.FindToolWindow(uint32 __VSFINDTOOLWIN.FTW_fForceCreate, fsiWindowGuid) with
        | VSConstants.S_OK, frame ->
            frame.Show() |> ignore
            getActiveProject()
            |> Option.iter (fun project ->
                let references = getReferences project
                let t = frame.GetType()
                let mi: PropertyInfo = t.GetProperty("FrameView", BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                let frameView = mi.GetValue(frame)
                let v = frameView.GetType().GetProperty("Content").GetValue(frameView) :?> DependencyObject
                let content = v.GetType().GetProperty("Content").GetValue(v) :?> DependencyObject
                let content2 = content.GetType().GetProperty("Content").GetValue(content)
                let content3 = content2.GetType().GetProperty("Content").GetValue(content2)
                let content4 = content3.GetType().GetProperty("TextView").GetValue(content3)
                let wpfView = content4 :?> IWpfTextView
                let textBuffer = wpfView.TextBuffer                
                use edit = textBuffer.CreateEdit()
                let line = wpfView.Caret.ContainingTextViewLine
                let pos = line.End.Position

                let sb = StringBuilder()
                sb.Append("\r\n") |> ignore
                for reference in references do
                    sb.AppendFormat("#r @\"{0}\"\r\n", reference) |> ignore

                edit.Insert(pos, sb.ToString().TrimEnd() + ";;") |> ignore
                edit.Apply() |> ignore

                // Generate script files
                if isFSharpProject project then
                    generateFile project)
        | _ ->
            let clsid = ref Guid.Empty
            let result = ref 0
            ErrorHandler.ThrowOnFailure(
                shell.ShowMessageBox(
                        0u,
                        clsid,
                        "",
                        "Please open FSI.",
                        String.Empty,
                        0u,
                        OLEMSGBUTTON.OLEMSGBUTTON_OK,
                        OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST,
                        OLEMSGICON.OLEMSGICON_INFO,
                        0,        // false
                        result)) |> ignore

    do dte2.Events.BuildEvents.add_OnBuildDone (fun _ _ ->
            let projects = dte2.Solution.Projects
            seq {
                for i in 0..projects.Count-1 ->
                    projects.Item(i + 1)
            }
            |> Seq.iter (fun project ->
                if isFSharpProject project && containsReferenceScript project then
                    generateFile project))

    member __.SetupCommands() =
        let menuCommandID = CommandID(Constants.guidAddReferenceInFSICmdSet, int Constants.cmdidAddReferenceInFSI)
        let menuCommand = OleMenuCommand((fun _ _ -> addReferenceInFSI()), menuCommandID)
        menuCommand.BeforeQueryStatus.Add (fun _ -> 
            let visibility = getActiveProject() |> Option.map isFSharpProject |> Option.getOrElse false
            menuCommand.Visible <- visibility)
        mcs.AddCommand(menuCommand)
