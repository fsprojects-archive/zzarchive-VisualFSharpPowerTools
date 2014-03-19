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

type MoveAction =
    | MoveUp
    | MoveDown

type NameAction = 
    | New

type Action =
    | NameAction of NameAction
    | MoveAction of MoveAction

[<NoEquality; NoComparison>]
type ActionInfo =
    { item : ProjectItem option
      project : Project }

module FolderMenuResources = 
    let getWindowTitle action = 
        match action with
        | NameAction.New -> Resource.newFolderDialogTitle

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

    let rec getFolderNames (item:ProjectItem) =
        let current = item.Name
        let subFolders = 
            item.ProjectItems
            |> Seq.cast<ProjectItem>
            |> Seq.filter (fun i -> VSUtils.isPhysicalFolder i)
            |> Seq.collect (fun f -> getFolderNames f)
        Seq.singleton current |> Seq.append subFolders

    let getfolderNamesFromProject (project: Project) =
        let names =
            project.ProjectItems
            |> Seq.cast<ProjectItem>
            |> Seq.filter (fun i -> VSUtils.isPhysicalFolder i)
            |> Seq.map (fun f -> getFolderNames f)
            |> Seq.collect (fun n -> n)
        Set names

    let getActionInfo() =
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        match items, projects with
        | [item], [] -> Some { ActionInfo.item=Some(item); project=item.ContainingProject }
        | [], [project] -> Some { item=None; project=project }
        | _, _ -> None

    let createFolder (items: ProjectItems) (name: string) = 
        items.AddFolder name |> ignore
        ()

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

    let performMoveAction (info: ActionInfo) action =
        let service = new FSharpProjectSystemService(dte :?> DTE)
        let node = info.item.Value?Node
        let project = info.project?Project

        match action with
        | MoveAction.MoveUp -> service.MoveFolderUp node (getPreviousItem node) project
        | MoveAction.MoveDown -> service.MoveFolderDown node (getNextItem node) project

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

    let getCurrentName (actionInfo: ActionInfo) action = 
        match action with
        | NameAction.New -> ""

    let performNameAction info action name =
        match action with
        | NameAction.New ->
            match info.item with
            | Some item -> createFolder item.ProjectItems name
            | None -> createFolder info.project.ProjectItems name

    let executeCommand (action:Action) = 
        let actionInfo = getActionInfo()
        match actionInfo with 
        | Some info ->
            match action with
            | Action.NameAction a ->
                let resources = 
                    { WindowTitle = FolderMenuResources.getWindowTitle a
                      OriginalName = getCurrentName info a
                      FolderNames = getfolderNamesFromProject info.project }
                askForNewFolderName resources |> Option.iter (performNameAction info a)
            | Action.MoveAction a -> performMoveAction info a
        | None ->
            System.Diagnostics.Debug.Fail("actionInfo is None")
            ()
        ()

    let isMoveCommandEnabled item action =
        match item with
        | Some item ->
            if not (VSUtils.isPhysicalFolder item) then false
            else
                let checkItem item = item <> null && VSUtils.isPhysicalFileOrFolderKind (item?ItemTypeGuid?ToString("B"))
                match action with
                | MoveAction.MoveUp -> checkItem item?Node?PreviousSibling
                | MoveAction.MoveDown -> checkItem item?Node?NextSibling
        | None -> false

    let isNameCommandEnabled item action =
        match item with 
        | Some item -> VSUtils.isPhysicalFolder item
        | None -> action = NameAction.New

    let isCommandEnabled (actionInfo: ActionInfo option) (action:Action) = 
        match actionInfo with
        | Some info ->
            if VSUtils.isFSharpProject info.project then
                match action with
                | Action.NameAction a -> isNameCommandEnabled info.item a
                | Action.MoveAction a -> isMoveCommandEnabled info.item a
            else
                false
        | None -> false

    let setupCommand guid id action = 
        let command = new CommandID(guid, id)
        let menuCommand = new OleMenuCommand((fun s e -> executeCommand action), command)
        menuCommand.BeforeQueryStatus.AddHandler(fun s e -> (s :?> OleMenuCommand).Enabled <- (isCommandEnabled (getActionInfo()) action))
        mcs.AddCommand(menuCommand)
    
    let setupNewFolderCommand = setupCommand PkgCmdConst.guidNewFolderCmdSet

    member x.SetupCommands() = 
        setupNewFolderCommand PkgCmdConst.cmdNewFolder (NameAction NameAction.New)
        setupNewFolderCommand PkgCmdConst.cmdMoveFolderUp (MoveAction MoveAction.MoveUp)
        setupNewFolderCommand PkgCmdConst.cmdMoveFolderDown (MoveAction MoveAction.MoveDown)

