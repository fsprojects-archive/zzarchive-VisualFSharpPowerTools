namespace FSharpVSPowerTools.Folders

open EnvDTE
open EnvDTE80
open System
open FSharp
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.Helpers
open System.ComponentModel.Design
open System.Xml.Linq
open System.Xml
open System.Windows
open FSharpVSPowerTools
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio
open System.IO

module PkgCmdConst = 
    let guidNewFolderCmdSet = new Guid("{e396b698-e00e-444b-9f5f-3dcb1ef74e63}")
    let cmdNewFolder = 0x1071
    let cmdNewFolderAbove = 0x1072
    let cmdNewFolderBelow = 0x1073
    let cmdFolderRename = 0x1074

// NewFolder 'mode'
type Action = 
    | New
    | NewAbove
    | NewBelow
    | Rename

module FolderMenuResources = 
    let getWindowTitle action = 
        match action with
        | Action.New -> "F# Power Tools - New Folder"
        | Action.NewAbove -> "F# Power Tools - New Folder Above"
        | Action.NewBelow -> "F# Power Tools - New Folder Below"
        | Action.Rename -> "F# Power Tools - Rename Folder"

type FolderMenuCommandsFilter(dte : DTE2, mcs : OleMenuCommandService, shell : IVsUIShell) = 
    let xn = XName.Get
    let xns name = XName.Get(name, "http://schemas.microsoft.com/developer/msbuild/2003")
    let getSelectedItems() = SolutionExplorerHelper.getSelectedItems dte |> List.ofSeq
    let getSelectedProjects() = SolutionExplorerHelper.getSelectedProjects dte |> List.ofSeq
    
    let isCommandEnabled action = 
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        match items, projects with
        | [item], [] -> 
            SolutionExplorerHelper.isFSharpProject item.ContainingProject.Kind 
            && (SolutionExplorerHelper.isPhysicalFolder item.Kind || action = Action.NewAbove || action = Action.NewBelow)
        | [], [project] -> SolutionExplorerHelper.isFSharpProject project.Kind && action = Action.New
        | _, _ -> false
    
    let getCurrentProject (items : ProjectItem list) (projects : Project list) = 
        match items with
        | [item] -> item.ContainingProject
        | _ -> projects.[0]
    
    let getProperty (item:ProjectItem) name =
        let property = item.Properties |> Seq.cast<Property> |> Seq.tryFind (fun p -> p.Name = name)
        match property with
        | Some(p) -> p.Value :?> string
        | None -> ""        

    let getCurrentName action = 
        match action with
        | Action.Rename -> 
            let items = getSelectedItems()
            let item = items.[0]
            getProperty item "FileName"
        | _ -> ""
    
    let getFullPath item = getProperty item "FullPath"
    
    let renameFolder (project : Project) item newFolderName (filesItemGroup : XContainer) = 
        let projectAbsolutePath = (Path.GetDirectoryName project.FileName) + "\\"
        let fullPath = getFullPath item
        let oldFolderFullPath = Path.GetDirectoryName fullPath
        let newFolderFullPath = Path.Combine(Path.GetDirectoryName(oldFolderFullPath), newFolderName)
        let oldFolder = oldFolderFullPath.Replace(projectAbsolutePath, "")
        let newFolder = newFolderFullPath.Replace(projectAbsolutePath, "")
        let oldLength = oldFolder.Length
        filesItemGroup.Elements()
        |> Seq.filter (fun e -> e.Attribute(xn "Include").Value.StartsWith(oldFolder))
        |> Seq.iter 
               (fun e -> 
               e.Attribute(xn "Include").Value <- newFolder + e.Attribute(xn "Include").Value.Substring(oldLength))
    
    let addNewFolder (project : Project) newFolderName (filesItemGroup : XContainer) = 
        let projectAbsolutePath = (Path.GetDirectoryName project.FileName) + "\\"
        let newFolderFullPath = Path.Combine(projectAbsolutePath, newFolderName)
        if not (Directory.Exists newFolderFullPath) then
            Directory.CreateDirectory newFolderFullPath |> ignore
            filesItemGroup.Add(new XElement(xns "Folder", new XAttribute(xn "Include", newFolderName + "\\")))
    
    let addNewSubFolder (project : Project) item newFolderName (filesItemGroup : XContainer) = 
        let projectAbsolutePath = (Path.GetDirectoryName project.FileName) + "\\"
        let fullPath = getFullPath item
        let newFolderFullPath = Path.Combine(fullPath, newFolderName)
        let newFolder = newFolderFullPath.Replace(projectAbsolutePath, "")
        let parent = Path.GetDirectoryName(newFolder)
        if not (Directory.Exists newFolderFullPath) then
            Directory.CreateDirectory newFolderFullPath |> ignore
            let last = 
                filesItemGroup.Elements()
                |> Seq.filter (fun e -> e.Attribute(xn "Include").Value.StartsWith(parent))
                |> Seq.last
            last.AddAfterSelf(new XElement(xns "Folder", new XAttribute(xn "Include", newFolder + "\\")))
    
    let addNewFolderAbove (project : Project) item newFolderName (filesItemGroup : XContainer) = 
        let projectAbsolutePath = (Path.GetDirectoryName project.FileName) + "\\"
        let fullPath = Path.GetDirectoryName(getFullPath item)
        let newFolderFullPath = Path.Combine(Path.GetDirectoryName(fullPath), newFolderName)
        let newFolder = newFolderFullPath.Replace(projectAbsolutePath, "")
        let relative = fullPath.Replace(projectAbsolutePath, "")
        if not (Directory.Exists newFolderFullPath) then
            Directory.CreateDirectory newFolderFullPath |> ignore
            let first = 
                filesItemGroup.Elements()
                |> Seq.find (fun e -> e.Attribute(xn "Include").Value.StartsWith(relative))
            first.AddBeforeSelf(new XElement(xns "Folder", new XAttribute(xn "Include", newFolder + "\\")))

    let addNewFolderBelow (project : Project) item newFolderName (filesItemGroup : XContainer) = 
        let projectAbsolutePath = (Path.GetDirectoryName project.FileName) + "\\"
        let fullPath = Path.GetDirectoryName(getFullPath item)
        let newFolderFullPath = Path.Combine(Path.GetDirectoryName(fullPath), newFolderName)
        let newFolder = newFolderFullPath.Replace(projectAbsolutePath, "")
        let relative = fullPath.Replace(projectAbsolutePath, "")
        if not (Directory.Exists newFolderFullPath) then
            Directory.CreateDirectory newFolderFullPath |> ignore
            let last = 
                filesItemGroup.Elements()
                |> Seq.filter (fun e -> e.Attribute(xn "Include").Value.StartsWith(relative))
                |> Seq.last
            last.AddAfterSelf(new XElement(xns "Folder", new XAttribute(xn "Include", newFolder + "\\")))
    
    let performAction action (newFolderName : string) = 
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        let project = getCurrentProject items projects
        let projectXml = XDocument.Load(project.FileName)
        let filesItemGroup = 
            projectXml.Root.Elements(xns "ItemGroup") 
            |> Seq.tryFind (fun ig -> ig.Element(xns "Compile") <> null || ig.Element(xns "Folder") <> null)
        match filesItemGroup with
        | Some(group) -> 
            match action with
            | Action.Rename -> renameFolder project items.[0] newFolderName group
            | Action.New -> 
                match items.Length with
                | 1 -> addNewSubFolder project items.[0] newFolderName group
                | _ -> addNewFolder project newFolderName group
            | Action.NewAbove -> addNewFolderAbove project items.[0] newFolderName group
            | Action.NewBelow -> addNewFolderBelow project items.[0] newFolderName group
            projectXml.Save(project.FileName)
        | None -> ()
    
    let askForNewFolderName resources = 
        let model = NewFolderNameDialogModel(resources)
        let wnd = UI.loadDialog model
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        let res = wnd.ShowDialog() |> Option.ofNullable
        match res with
        | Some true -> Some(model.Name)
        | _ -> None
    
    let executeCommand action = 
        let currentName = getCurrentName action
        let resources = 
            { WindowTitle = FolderMenuResources.getWindowTitle action
              OriginalName = currentName }
        askForNewFolderName resources |> Option.iter (performAction action)
    
    let setupCommand guid id action = 
        let command = new CommandID(guid, id)
        let menuCommand = new OleMenuCommand((fun s e -> executeCommand action), command)
        menuCommand.BeforeQueryStatus.AddHandler(fun s e -> (s :?> OleMenuCommand).Enabled <- isCommandEnabled action)
        mcs.AddCommand(menuCommand)
    
    let setupNewFolderCommand = setupCommand PkgCmdConst.guidNewFolderCmdSet
    
    member x.SetupCommands() = 
        setupNewFolderCommand PkgCmdConst.cmdNewFolder Action.New
        setupNewFolderCommand PkgCmdConst.cmdNewFolderAbove Action.NewAbove
        setupNewFolderCommand PkgCmdConst.cmdNewFolderBelow Action.NewBelow
        setupNewFolderCommand PkgCmdConst.cmdFolderRename Action.Rename
    
    member x.ShowDialog(wnd : Window) = 
        try 
            if ErrorHandler.Failed(shell.EnableModeless(0)) then Some false
            else wnd.ShowDialog() |> Option.ofNullable
        finally
            shell.EnableModeless(1) |> ignore
