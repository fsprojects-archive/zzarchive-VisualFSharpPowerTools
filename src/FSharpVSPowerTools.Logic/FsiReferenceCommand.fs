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
        dte.ActiveSolutionProjects :?> obj []
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
                project.ProjectItems.Item(scriptFolderName)
                |> Option.ofNull
                |> Option.getOrTry (fun _ ->
                    project.ProjectItems.AddFolder(scriptFolderName))
            projectFolderScript.ProjectItems.AddFromFile(textFile) |> ignore
            project.Save()

    let getActiveProjectOutput (project: Project) =
        Option.attempt (fun _ ->
            let projectPath = getProjectFolder project
            let outputFileName = project.Properties.Item("OutputFileName").Value.ToString()
            let config = project.ConfigurationManager.ActiveConfiguration
            let outputPath = config.Properties.Item("OutputPath").Value.ToString()
            Path.Combine(Path.Combine(projectPath, outputPath), outputFileName))

    let getActiveOutputFileFullPath (reference: Reference) =
        reference.GetType().GetProperty("SourceProject")
        |> Option.ofNull
        |> Option.map (fun sourceProject -> sourceProject.GetValue(reference) :?> Project)
        |> Option.bind getActiveProjectOutput

    let getRelativePath (file: string) (folder: string) =
        let fileUri = Uri(file)
        // Folders must end in a slash
        let folder =
            if not <| folder.EndsWith(Path.DirectorySeparatorChar.ToString()) then
                folder + string Path.DirectorySeparatorChar
            else folder
        let folderUri = Uri(folder)
        Uri.UnescapeDataString(folderUri.MakeRelativeUri(fileUri).ToString().Replace('/', Path.DirectorySeparatorChar))

    let generateLoadScriptContent(project: Project, scriptFile: string) =
        let projectFolder = Path.Combine(getProjectFolder project, scriptFolderName)
        use projectProvider = new ProjectProvider(project, (fun _ -> None), (fun _ -> ()), id)
        let sb = StringBuilder()
        sb.AppendLine(sprintf "#load @\"%s\"" scriptFile) |> ignore
        match (projectProvider :> IProjectProvider).SourceFiles |> Array.toList with
        | [] -> ()
        | first::rest ->
            sb.AppendLine(sprintf "#load @\"%s\"" (getRelativePath first projectFolder)) |> ignore
            for sourceFile in rest do
                sb.AppendLine(sprintf "      @\"%s\"" (getRelativePath sourceFile projectFolder)) |> ignore
        sb.ToString()   

    let isReferenceProject (reference: Reference) =
        let sourceProject = reference.GetType().GetProperty("SourceProject")
        sourceProject <> null && sourceProject.GetValue(reference) <> null

    let toReferenceSeq (references: References) =
        seq {
            for i in 0..references.Count-1 ->
                references.Item(i + 1)
        }

    let generateFileContent(project: Project) =
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
                        getActiveOutputFileFullPath reference
                        |> Option.iter (fun outputFilePath -> projectRefList.Add(outputFilePath))
                    else
                        let fullPath = reference.Path
                        if File.Exists fullPath then
                            assemblyRefList.Add(fullPath))
        | _ -> ()

        let assemblyRefList = assemblyRefList |> Seq.map (sprintf "#r @\"%s\"") |> Seq.toList
        let projectRefList = projectRefList |> Seq.map (sprintf "#r @\"%s\"") |> Seq.toList
        String.Join(Environment.NewLine, 
                    "// Warning: generated file; your changes could be lost when a new file is generated. ",
                    String.Join(Environment.NewLine, assemblyRefList),
                    String.Join(Environment.NewLine, projectRefList))

    let generateFile (project: Project) =
        Option.ofNull project
        |> Option.iter (fun project ->
            addFileToActiveProject(project, "load-references.fsx", generateFileContent project)
            let content = generateLoadScriptContent(project, "load-references.fsx")
            addFileToActiveProject(project, "load-project.fsx", content))
    
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
