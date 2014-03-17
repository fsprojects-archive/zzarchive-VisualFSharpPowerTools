namespace FSharpVSPowerTools.Folders

open EnvDTE
open EnvDTE80
open System
open FSharp
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
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

type Action = 
    | New
    | Rename

[<NoEquality; NoComparison>]
type actionInfo =
    { item : ProjectItem option
      project : Project
      action : Action }

[<NoEquality; NoComparison>]
type actionInfoWithItem =
    { item : ProjectItem
      project : Project
      newFolderName : string
      action : Action}

module FolderMenuResources = 
    let getWindowTitle action = 
        match action with
        | Action.New -> "F# Power Tools - New Folder"
        | Action.Rename -> "F# Power Tools - Rename Folder"

type FolderMenuCommandsFilter(dte:DTE2, mcs:OleMenuCommandService, shell:IVsUIShell) = 

    let xn = XName.Get
    let xns name = XName.Get(name, "http://schemas.microsoft.com/developer/msbuild/2003")

    let getSelectedItems() = VSUtils.getSelectedItemsFromSolutionExplorer dte |> List.ofSeq
    let getSelectedProjects() = VSUtils.getSelectedProjectsFromSolutionExplorer dte |> List.ofSeq
    
    let stripEndSlash (x:string) = x.TrimEnd(char "\\")

    let getActionInfo action =
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        match items, projects with
        | [item], [] -> Some { actionInfo.item=Some(item); project=item.ContainingProject; action=action }  // just one item selected
        | [], [project] -> Some { item=None; project=project; action=action }                    // just one project selected
        | _, _ -> None

    let isCommandEnabled (actionInfo: actionInfo option) = 
        match actionInfo with
        | Some info ->
            if VSUtils.isFSharpProject info.project then
                match info.item with
                | Some item -> VSUtils.isPhysicalFolder item
                | None -> info.action = Action.New
            else
                false
        | None -> false
    
    let getProperty (item:ProjectItem) name =
        let property = item.Properties |> Seq.cast<Property> |> Seq.tryFind (fun p -> p.Name = name)
        match property with
        | Some p -> p.Value :?> string
        | None -> ""

    let getCurrentName (actionInfo: actionInfo) = 
        match actionInfo.action with
        | Action.Rename -> 
            match actionInfo.item with
            | Some item -> getProperty item "FileName"
            | None ->
                System.Diagnostics.Debug.Fail("item is None for Action.Rename")
                ""
        | _-> ""
    
    let getFullPath item = getProperty item "FullPath"
    
    let replaceIncludePaths oldFolder newFolder (itemGroup: XContainer) =
        itemGroup.Elements()
        |> Seq.map (fun e -> e.Attribute(xn "Include"))
        |> Seq.filter (fun a -> a.Value.Equals(oldFolder) || a.Value.StartsWith(oldFolder + "\\"))
        |> Seq.iter (fun a -> a.Value <- newFolder + a.Value.Substring(oldFolder.Length))

    let replaceIncludeGroup (xDoc: XDocument) oldFolder newFolder elementName  =
        xDoc.Root.Elements(xns "ItemGroup")
        |> Seq.tryFind (fun ig -> ig.Element(xns elementName) <> null)
        |> Option.iter (replaceIncludePaths oldFolder newFolder)

    let renameFolder info = 
        let projectAbsolutePath = (Path.GetDirectoryName info.project.FullName) + "\\"
        let fullPath = getFullPath info.item
        let oldFolderFullPath = Path.GetDirectoryName fullPath
        let newFolderFullPath = Path.Combine(Path.GetDirectoryName(oldFolderFullPath), info.newFolderName)
        let oldFolder = stripEndSlash(oldFolderFullPath.Replace(projectAbsolutePath, ""))
        let newFolder = stripEndSlash(newFolderFullPath.Replace(projectAbsolutePath, ""))
        if not (Directory.Exists newFolderFullPath) then
            Directory.Move(oldFolderFullPath, newFolderFullPath)
            let xDoc = XDocument.Load(info.project.FileName)
            replaceIncludeGroup xDoc oldFolder newFolder "Compile"
            replaceIncludeGroup xDoc oldFolder newFolder "Folder"
            xDoc.Save(info.project.FileName)

    let createNewFolder relativePath fullPath (projectFile: string) =
        if not (Directory.Exists fullPath) then
            Directory.CreateDirectory fullPath |> ignore
            let xDoc = XDocument.Load(projectFile)
            let filesItemGroup = xDoc.Root.Elements(xns "ItemGroup") |> Seq.tryFind (fun ig -> ig.Element(xns "Folder") <> null)
            let group =
                match filesItemGroup with
                | Some x -> x
                | None ->
                    let x = new XElement(xns "ItemGroup")
                    xDoc.Root.Add x
                    x
            group.Add(new XElement(xns "Folder", new XAttribute(xn "Include", relativePath)))
            xDoc.Save(projectFile)

    let addNewFolder (project : Project) newFolderName = 
        let projectAbsolutePath = (Path.GetDirectoryName project.FileName) + "\\"
        let newFolderFullPath = stripEndSlash(Path.Combine(projectAbsolutePath, newFolderName))
        createNewFolder newFolderName newFolderFullPath project.FileName
    
    let addNewSubFolder info = 
        let projectAbsolutePath = (Path.GetDirectoryName info.project.FileName) + "\\"
        let fullPath = getFullPath info.item
        let newFolderFullPath = Path.Combine(fullPath, info.newFolderName)
        let newFolder = stripEndSlash(newFolderFullPath.Replace(projectAbsolutePath, ""))
        createNewFolder newFolder newFolderFullPath info.project.FileName
    
    let performAction (info: actionInfo) (newFolderName: string) = 
        match info.action with
        | Action.Rename ->
            match info.item with
            | Some item ->
                renameFolder { item=item; project=info.project; action=info.action; newFolderName=newFolderName }
            | None ->
                System.Diagnostics.Debug.Fail("addNewFolderAboveOrBelow called with action = Action.Rename and info = None")
        | Action.New -> 
            match info.item with
            | Some item -> addNewSubFolder { item=item; project=info.project; action=info.action; newFolderName=newFolderName }
            | None -> addNewFolder info.project newFolderName

    let askForNewFolderName resources = 
        let model = NewFolderNameDialogModel(resources)
        let wnd = UI.loadDialog model
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        let res = wnd.ShowDialog() |> Option.ofNullable
        match res with
        | Some true -> Some model.Name
        | _ -> None
    
    let executeCommand action = 
        let actionInfo = getActionInfo action
        match actionInfo with 
        | Some info ->
            let currentName = getCurrentName info
            let resources = 
                { WindowTitle = FolderMenuResources.getWindowTitle action
                  OriginalName = currentName }
            askForNewFolderName resources |> Option.iter (performAction info)
        | None ->
            System.Diagnostics.Debug.Fail("actionInfo is None")
            ()
    
    let setupCommand guid id action = 
        let command = new CommandID(guid, id)
        let menuCommand = new OleMenuCommand((fun s e -> executeCommand action), command)
        menuCommand.BeforeQueryStatus.AddHandler(fun s e -> (s :?> OleMenuCommand).Enabled <- (getActionInfo action |> isCommandEnabled))
        mcs.AddCommand(menuCommand)
    
    let setupNewFolderCommand = setupCommand PkgCmdConst.guidNewFolderCmdSet
    
    member x.SetupCommands() = 
        setupNewFolderCommand PkgCmdConst.cmdNewFolder Action.New
        setupNewFolderCommand PkgCmdConst.cmdFolderRename Action.Rename
    
    member x.ShowDialog(wnd : Window) = 
        try 
            if ErrorHandler.Failed(shell.EnableModeless(0)) then Some false
            else wnd.ShowDialog() |> Option.ofNullable
        finally
            shell.EnableModeless(1) |> ignore