namespace FSharpVSPowerTools.Folders

open EnvDTE
open EnvDTE80
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Text
open System
open System.ComponentModel.Composition
open System.ComponentModel.Design
open System.Diagnostics
open System.Windows
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open Reflection

module PkgCmdConst = 
    let guidNewFolderCmdSet = new Guid("{e396b698-e00e-444b-9f5f-3dcb1ef74e63}")
    let cmdNewFolder = 0x1071
    let cmdFolderRename = 0x1072
    let cmdMoveFolderUp = 0x1073
    let cmdMoveFolderDown = 0x1074

type Action = 
    | New
    | Rename
    | MoveUp
    | MoveDown

[<NoEquality; NoComparison>]
type actionInfo =
    { item : ProjectItem option
      project : Project
      action : Action }

module FolderMenuResources = 
    let getWindowTitle action = 
        match action with
        | Action.New -> Resource.newFolderDialogTitle
        | Action.Rename -> Resource.renameFolderDialogTitle
        | _ -> ""

[<Export>]
type FSharpProjectSystemService [<ImportingConstructor>] (dte: DTE) = 
    let assemblyInfo =
        match VisualStudioVersion.fromDTEVersion dte.Version with
        | VisualStudioVersion.VS2012 ->
            "FSharp.ProjectSystem.FSharp, Version=11.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
        | _ ->
            "FSharp.ProjectSystem.FSharp, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"    
    let asm = lazy try Assembly.Load(assemblyInfo)
                   with _ -> raise (AssemblyMissingException "FSharp.ProjectSystem.FSharp")

    let MSBuildUtilitiesType = lazy asm.Value.GetType("Microsoft.VisualStudio.FSharp.ProjectSystem.MSBuildUtilities")

    member x.MoveFolderDown item next project :unit =
        MSBuildUtilitiesType.Value?MoveFolderDown(item, next, project)

    member x.MoveFolderUp item next project :unit = 
        MSBuildUtilitiesType.Value?MoveFolderUp(item, next, project)

type FolderMenuCommands(dte:DTE2, mcs:OleMenuCommandService, shell:IVsUIShell) = 

    let getSelectedItems() = VSUtils.getSelectedItemsFromSolutionExplorer dte |> List.ofSeq
    let getSelectedProjects() = VSUtils.getSelectedProjectsFromSolutionExplorer dte |> List.ofSeq

    let getActionInfo action =
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        match items, projects with
        | [item], [] -> Some { actionInfo.item=Some(item); project=item.ContainingProject; action=action }
        | [], [project] -> Some { item=None; project=project; action=action }
        | _, _ -> None

    let createFolder (items: ProjectItems) (name: string) = 
        items.AddFolder name |> ignore
        ()

    let renameFolder (item: ProjectItem) (name:string) = 
   //    item?Node?RenameDirectory(name)

        let s = item?Node
        ()

    let performActionWithName (info: actionInfo) (name: string) = 
        match info.action with
        | Action.New ->
            match info.item with
            | Some item -> createFolder item.ProjectItems name
            | None -> createFolder info.project.ProjectItems name
        | Action.Rename ->
            match info.item with
            | Some item -> renameFolder item name
            | None -> Debug.Fail "Action.Rename called with item = None"
        | _ -> Debug.Fail (sprintf "performActionWithName called with action = %s" (string info.action))

    let getNextItemImpl node parent =
        if parent?FirstChild = node then
            let next = node?NextSibling
            node?NextSibling <- (next?NextSibling)
            next?NextSibling <- node
            parent?FirstChild <- next
            next
        else
            let prev = node?PreviousSibling
            let next = node?NextSibling
            node?NextSibling <- next?NextSibling
            prev?NextSibling <- next
            next?NextSibling <- node
            if parent?LastChild = next then
                parent?LastChild <- node
            next

    let getPreviousItemImpl node parent =
        if parent?FirstChild?NextSibling = node then
            let prev = parent?FirstChild
            parent?FirstChild <- node
            prev?NextSibling <- node?NextSibling
            prev?NextSibling <- prev
            if parent?LastChild = node then
                parent?LastChild <- prev
            prev
        else
            let prevPrev = node?PreviousSibling?PreviousSibling
            let prev = prevPrev?NextSibling
            prevPrev?NextSibling <- node
            prev?NextSibling <- node?NextSibling
            node?NextSibling <- prev
            if parent?LastChild = node then
                parent?LastChild <- prev
            prev

    let getItem selector node =
        let parent = node?Parent
        node?OnItemDeleted()
        let item = selector node parent
        parent?OnItemAdded(parent, node)
        item

    let getPreviousItem = getItem getPreviousItemImpl

    let getNextItem = getItem getNextItemImpl

    let performMoveAction (info: actionInfo) =
        let service = new FSharpProjectSystemService(dte :?> DTE)
        let node = info.item.Value?Node
        let project = info.project?Project

        match info.action with
        | Action.MoveUp -> service.MoveFolderUp node (getPreviousItem node) project
        | Action.MoveDown -> service.MoveFolderDown node (getNextItem node) project
        | _ -> Debug.Fail (sprintf "performMoveAction called with action = %s" (string info.action))

        project?SetProjectFileDirty(true)
        project?ComputeSourcesAndFlags()
        ()

    let showDialog (wnd: Window) =
        try
            if ErrorHandler.Failed(shell.EnableModeless(0)) then
                Some false
            else
                wnd.ShowDialog() |> Option.ofNullable
        finally
            shell.EnableModeless(1) |> ignore

    let askForNewFolderName resources = 
        let model = NewFolderNameDialogModel(resources)
        let wnd = UI.loadDialog model
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        let res = showDialog wnd
        match res with
        | Some true -> Some model.Name
        | _ -> None

    let getCurrentName (actionInfo: actionInfo) = 
        match actionInfo.action with
        | Action.Rename -> 
            match actionInfo.item with
            | Some item -> item.GetProperty "FileName"
            | None ->
                System.Diagnostics.Debug.Fail("item is None for Action.Rename")
                ""
        | _-> ""

    let executeCommand action = 
        let actionInfo = getActionInfo action
        match actionInfo with 
        | Some info ->
            match info.action with
            | Action.New | Action.Rename ->
                let currentName = getCurrentName info
                let resources = 
                    { WindowTitle = FolderMenuResources.getWindowTitle action
                      OriginalName = currentName }
                askForNewFolderName resources |> Option.iter (performActionWithName info)
            | Action.MoveDown | Action.MoveUp ->
                performMoveAction info
        | None ->
            System.Diagnostics.Debug.Fail("actionInfo is None")
            ()
        ()

    let isCommandEnabled (actionInfo: actionInfo option) = 
        match actionInfo with
        | Some info ->
            if VSUtils.isFSharpProject info.project then
                match info.item with
                | Some item ->
                    if not (VSUtils.isPhysicalFolder item) then false
                    else
                        let checkItem item = item <> null && VSUtils.isPhysicalFileOrFolderKind (item?ItemTypeGuid?ToString("B"))
                        match info.action with
                        | Action.MoveUp -> checkItem item?Node?PreviousSibling
                        | Action.MoveDown -> checkItem item?Node?NextSibling
                        | Action.Rename -> false // doesn't work now
                        | Action.New -> true
                | None -> info.action = Action.New
            else
                false
        | None -> false

    let setupCommand guid id action = 
        let command = new CommandID(guid, id)
        let menuCommand = new OleMenuCommand((fun s e -> executeCommand action), command)
        menuCommand.BeforeQueryStatus.AddHandler(fun s e -> (s :?> OleMenuCommand).Enabled <- (getActionInfo action |> isCommandEnabled))
        mcs.AddCommand(menuCommand)
    
    let setupNewFolderCommand = setupCommand PkgCmdConst.guidNewFolderCmdSet

    member x.SetupCommands() = 
        setupNewFolderCommand PkgCmdConst.cmdNewFolder Action.New
        setupNewFolderCommand PkgCmdConst.cmdFolderRename Action.Rename
        setupNewFolderCommand PkgCmdConst.cmdMoveFolderUp Action.MoveUp
        setupNewFolderCommand PkgCmdConst.cmdMoveFolderDown Action.MoveDown

