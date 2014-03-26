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

type Action =
    | New
    | MoveAction of MoveAction

[<NoEquality; NoComparison>]
type ActionInfo =
    { Item : ProjectItem option
      Project : Project }

type FSharpProjectSystemService(dte: DTE) = 
    let assemblyInfo =
        match VisualStudioVersion.fromDTEVersion dte.Version with
        | VisualStudioVersion.VS2012 ->
            "FSharp.ProjectSystem.FSharp, Version=11.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
        | _ ->
            "FSharp.ProjectSystem.FSharp, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"    
    let asm = lazy try Assembly.Load(assemblyInfo)
                   with _ ->
                        AssemblyMissingException "FSharp.ProjectSystem.FSharp" |> logException |> raise

    let MSBuildUtilitiesType = lazy asm.Value.GetType("Microsoft.VisualStudio.FSharp.ProjectSystem.MSBuildUtilities")

    member x.MoveFolderDown item next project :unit =
        MSBuildUtilitiesType.Value?MoveFolderDown(item, next, project)

    member x.MoveFolderUp item next project :unit = 
        MSBuildUtilitiesType.Value?MoveFolderUp(item, next, project)

type FolderMenuCommands(dte:DTE2, mcs:OleMenuCommandService, shell:IVsUIShell, logger:Logger) = 

    let getSelectedItems() = VSUtils.getSelectedItemsFromSolutionExplorer dte |> List.ofSeq
    let getSelectedProjects() = VSUtils.getSelectedProjectsFromSolutionExplorer dte |> List.ofSeq

    let rec getFolderNamesFromItems (items:ProjectItems) =
        seq { 
            for item in items do
                if VSUtils.isPhysicalFolder item then
                    yield item.Name
                    yield! getFolderNamesFromItems item.ProjectItems
        }

    let getfolderNamesFromProject (project: Project) =
        Set (getFolderNamesFromItems project.ProjectItems)

    let getActionInfo() =
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        match items, projects with
        | [item], [] -> Some { ActionInfo.Item=Some(item); Project=item.ContainingProject }
        | [], [project] -> Some { Item=None; Project=project }
        | _, _ -> None

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
            node?NextSibling <- prev
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
        let node = info.Item.Value?Node
        let project = info.Project?Project

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

    let performNewFolderAction (info: ActionInfo) name =
        let items = 
            match info.Item with
            | Some item -> item.ProjectItems
            | None -> info.Project.ProjectItems
        items.AddFolder name |> ignore

    let executeCommand (action:Action) = 
        let actionInfo = getActionInfo()
        match actionInfo with 
        | Some info ->
            match action with
            | Action.New ->
                let resources = 
                    { WindowTitle = Resource.newFolderDialogTitle
                      FolderNames = getfolderNamesFromProject info.Project }
                askForNewFolderName resources |> Option.iter (performNewFolderAction info)
            | Action.MoveAction a -> performMoveAction info a
        | None -> fail "actionInfo is None"

    let isMoveCommandEnabled item action =
        match item with
        | Some item ->
            if not (VSUtils.isPhysicalFolder item) then false
            else
                let checkItem item =
                    item <> null && VSUtils.isPhysicalFileOrFolderKind (item?ItemTypeGuid?ToString("B"))
                match action with
                | MoveAction.MoveUp -> checkItem item?Node?PreviousSibling
                | MoveAction.MoveDown -> checkItem item?Node?NextSibling
        | None -> false

    let isNewFolderCommandEnabled item =
        match item with 
        | Some item -> VSUtils.isPhysicalFolder item
        | None -> true

    let isCommandEnabled (actionInfo: ActionInfo option) (action:Action) = 
        match actionInfo with
        | Some info ->
            if VSUtils.isFSharpProject info.Project then
                match action with
                | Action.New -> isNewFolderCommandEnabled info.Item
                | Action.MoveAction a -> isMoveCommandEnabled info.Item a
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
        setupNewFolderCommand PkgCmdConst.cmdNewFolder Action.New
        setupNewFolderCommand PkgCmdConst.cmdMoveFolderUp (MoveAction MoveAction.MoveUp)
        setupNewFolderCommand PkgCmdConst.cmdMoveFolderDown (MoveAction MoveAction.MoveDown)

