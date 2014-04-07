﻿namespace FSharpVSPowerTools.Folders

open EnvDTE
open EnvDTE80
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Text
open System
open System.IO
open System.ComponentModel.Composition
open System.ComponentModel.Design
open System.Diagnostics
open System.Windows
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open Reflection

module PkgCmdConst = 
    let guidNewFolderCmdSet = Guid "{9EDC1279-C317-43A6-B554-3A4D7853D55E}"
    let cmdNewFolder = 0x1071
    let guidMoveCmdSet = Guid "{7B573ACF-2772-4F46-B290-9B0EA94CBFAB}"
    let cmdMoveFolderUp = 0x1070
    let cmdMoveFolderDown = 0x1071
    let cmdMoveToFolder = 0x1072

type VerticalMoveAction = 
    | MoveUp
    | MoveDown

type Action = 
    | New
    | VerticalMoveAction of VerticalMoveAction
    | MoveToFolder

[<NoEquality>]
[<NoComparison>]
type ActionInfo = 
    { Items: ProjectItem list
      Project: Project }

[<Export>]
type FSharpProjectSystemService [<ImportingConstructor>] (dte: DTE) = 
    
    let assemblyInfo = 
        match VisualStudioVersion.fromDTEVersion dte.Version with
        | VisualStudioVersion.VS2012 -> 
            "FSharp.ProjectSystem.FSharp, Version=11.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
        | _ -> "FSharp.ProjectSystem.FSharp, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
    
    let asm = 
        lazy try 
                 Assembly.Load(assemblyInfo)
             with _ -> raise (AssemblyMissingException "FSharp.ProjectSystem.FSharp")
    
    let MSBuildUtilitiesType = lazy asm.Value.GetType("Microsoft.VisualStudio.FSharp.ProjectSystem.MSBuildUtilities")
    member x.MoveFolderDown item next project: unit = MSBuildUtilitiesType.Value?MoveFolderDown (item, next, project)
    member x.MoveFolderUp item next project: unit = MSBuildUtilitiesType.Value?MoveFolderUp (item, next, project)

type FolderMenuCommands(dte: DTE2, mcs: OleMenuCommandService, shell: IVsUIShell) = 
    let getSelectedItems() = VSUtils.getSelectedItemsFromSolutionExplorer dte |> Seq.toList
    let getSelectedProjects() = VSUtils.getSelectedProjectsFromSolutionExplorer dte |> Seq.toList
    
    let rec getFolderNamesFromItems (items: ProjectItems) = 
        seq { 
            for item in items do
                if VSUtils.isPhysicalFolder item then 
                    yield item.Name
                    yield! getFolderNamesFromItems item.ProjectItems
        }
    
    let getfolderNamesFromProject (project: Project) = Set(getFolderNamesFromItems project.ProjectItems)
    
    let rec getFoldersFromItems (items: ProjectItems) = 
        [ for item in items do
              if VSUtils.isPhysicalFolder item then 
                  yield { Name = item.Name
                          FullPath = item.Object?Url
                          IsProject = false
                          SubFolders = getFoldersFromItems item.ProjectItems } ]
    
    let getFoldersFromProject (project: Project) = 
        [ { Name = project.Name
            FullPath = Path.GetDirectoryName project.FullName
            IsProject = true
            SubFolders = getFoldersFromItems project.ProjectItems } ]
    
    let rec getFolderItems (items: ProjectItems) = 
        seq { 
            for item in items do
                if VSUtils.isPhysicalFolder item then 
                    yield item
                    yield! getFolderItems item.ProjectItems
        }
    
    let getFolderItemByName (project: Project) name = 
        getFolderItems project.ProjectItems |> Seq.tryFind (fun i -> i.Name = name)
    
    let getActionInfo() = 
        let items = getSelectedItems()
        let projects = getSelectedProjects()
        match items, projects with
        | [], [ project ] -> 
            Some { Items = []
                   Project = project }
        | [ item ], [] -> 
            Some { Items = [ item ]
                   Project = item.ContainingProject }
        | _ :: _, [] -> 
            let projects = 
                items
                |> List.toSeq
                |> Seq.map (fun i -> i.ContainingProject)
                |> Seq.distinctBy (fun p -> p.UniqueName)
                |> Seq.toList
            match projects with
            | [ project ] -> 
                Some { Items = items
                       Project = project } // items from the same project
            | _ -> None // items from different projects
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
            if parent?LastChild = next then parent?LastChild <- node
            next
    
    let getPreviousItemImpl node parent = 
        if parent?FirstChild?NextSibling = node then 
            let prev = parent?FirstChild
            parent?FirstChild <- node
            prev?NextSibling <- node?NextSibling
            node?NextSibling <- prev
            if parent?LastChild = node then parent?LastChild <- prev
            prev
        else 
            let prevPrev = node?PreviousSibling?PreviousSibling
            let prev = prevPrev?NextSibling
            prevPrev?NextSibling <- node
            prev?NextSibling <- node?NextSibling
            node?NextSibling <- prev
            if parent?LastChild = node then parent?LastChild <- prev
            prev
    
    let getItem selector node = 
        let parent = node?Parent
        node?OnItemDeleted ()
        let item = selector node parent
        parent?OnItemAdded (parent, node)
        item
    
    let getPreviousItem = getItem getPreviousItemImpl
    let getNextItem = getItem getNextItemImpl
    
    let performVerticalMoveAction (info: ActionInfo) action = 
        match info.Items with
        | [ item ] -> 
            let service = new FSharpProjectSystemService(dte :?> DTE)
            let node = item?Node
            let project = info.Project?Project
            match action with
            | VerticalMoveAction.MoveUp -> service.MoveFolderUp node (getPreviousItem node) project
            | VerticalMoveAction.MoveDown -> service.MoveFolderDown node (getNextItem node) project
            project?SetProjectFileDirty (true)
            project?ComputeSourcesAndFlags ()
            ()
        | [] -> logError "performVerticalMoveAction called with empty info.Items"
        | _ -> logError "performVerticalMoveAction called with more than one item in info.Items"
    
    let showDialog (wnd: Window) = 
        try 
            if ErrorHandler.Failed(shell.EnableModeless(0)) then Some false
            else wnd.ShowDialog() |> Option.ofNullable
        finally
            shell.EnableModeless(1) |> ignore
    
    let askForDestinationFolder resources = 
        let model = MoveToFolderDialogModel resources
        let wnd = FolderMenuUI.loadMoveToFolderDialog model
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        let res = showDialog wnd
        match res with
        | Some true -> model.SelectedFolder
        | _ -> None
    
    let performMoveToFolderAction (info: ActionInfo) (folder: Folder) = 
        let destination = 
            if folder.IsProject then info.Project.ProjectItems
            else 
                let item = getFolderItemByName info.Project folder.Name
                match item with
                | Some x -> x.ProjectItems
                | None -> 
                    let ex = ArgumentException(sprintf "folder named %s not found." folder.Name)
                    logException ex
                    raise ex
        
        let folderExists =
            if Directory.Exists(folder.FullPath) then true
            else
                try Directory.CreateDirectory(folder.FullPath) |> ignore; true with _ -> false
        if folderExists then
            for item in info.Items do
                Debug.Assert(item.FileCount = 1s, "Item should contain only one file.")
                let filePath = item.FileNames(0s)
                destination.AddFromFileCopy(filePath) |> ignore
                item.Delete()
            info.Project.IsDirty <- true
        else
            messageBoxError Resource.validationFolderDoesNotExist
    
    let askForNewFolderName resources = 
        let model = NewFolderNameDialogModel resources
        let wnd = FolderMenuUI.loadNewFolderDialog model
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        let res = showDialog wnd
        match res with
        | Some true -> Some model.Name
        | _ -> None
    
    let performNewFolderAction (info: ActionInfo) name = 
        let items = 
            match info.Items with
            | [ item ] -> item.ProjectItems
            | _ -> info.Project.ProjectItems
        items.AddFolder name |> ignore
    
    let executeCommand (action: Action) = 
        let actionInfo = getActionInfo()
        match actionInfo with
        | Some info -> 
            match action with
            | Action.New -> 
                let resources = 
                    { WindowTitle = Resource.newFolderDialogTitle
                      FolderNames = getfolderNamesFromProject info.Project }
                askForNewFolderName resources |> Option.iter (performNewFolderAction info)
            | Action.VerticalMoveAction a -> performVerticalMoveAction info a
            | Action.MoveToFolder -> 
                let resources = 
                    { FileNames = info.Items |> List.map (fun item -> Path.GetFileName item.Object?Url)
                      Root = getFoldersFromProject info.Project }
                askForDestinationFolder resources |> Option.iter (performMoveToFolderAction info)
        | None -> fail "actionInfo is None"
    
    let isVerticalMoveCommandEnabled info action = 
        match info.Items with
        | [ item ] -> 
            if not (VSUtils.isPhysicalFolder item) then false
            else 
                let checkItem item = 
                    item <> null && VSUtils.isPhysicalFileOrFolderKind (item?ItemTypeGuid?ToString ("B"))
                match action with
                | VerticalMoveAction.MoveUp -> checkItem item?Node?PreviousSibling
                | VerticalMoveAction.MoveDown -> checkItem item?Node?NextSibling
        | _ -> false
    
    let isNewFolderCommandEnabled info = 
        match info.Items with
        | [ item ] -> VSUtils.isPhysicalFolder item
        | [] -> true
        | _ -> false
    
    let isMoveToFolderCommandEnabled info = 
        match info.Items with
        | [] -> false
        | _ -> 
            let filesOnly = info.Items |> List.forall (fun i -> VSUtils.isPhysicalFile i)
            let distinctByName = info.Items |> Seq.distinctBy (fun i -> i.Name)
            filesOnly && (Seq.length distinctByName = List.length info.Items)
    
    let isCommandEnabled (actionInfo: ActionInfo option) (action: Action) = 
        match actionInfo with
        | Some info -> 
            if VSUtils.isFSharpProject info.Project then 
                match action with
                | Action.New -> isNewFolderCommandEnabled info
                | Action.VerticalMoveAction a -> isVerticalMoveCommandEnabled info a
                | Action.MoveToFolder -> isMoveToFolderCommandEnabled info
            else false
        | None -> false
    
    let setupCommand guid id action = 
        let command = new CommandID(guid, id)
        let menuCommand = new OleMenuCommand((fun s e -> executeCommand action), command)
        menuCommand.BeforeQueryStatus.AddHandler
            (fun s e -> (s :?> OleMenuCommand).Enabled <- (isCommandEnabled (getActionInfo()) action))
        mcs.AddCommand(menuCommand)
    
    let setupNewFolderCommand = setupCommand PkgCmdConst.guidNewFolderCmdSet
    let setupMoveCommand = setupCommand PkgCmdConst.guidMoveCmdSet
    member x.SetupCommands() = 
        setupNewFolderCommand PkgCmdConst.cmdNewFolder Action.New
        setupMoveCommand PkgCmdConst.cmdMoveFolderUp (VerticalMoveAction VerticalMoveAction.MoveUp)
        setupMoveCommand PkgCmdConst.cmdMoveFolderDown (VerticalMoveAction VerticalMoveAction.MoveDown)
        setupMoveCommand PkgCmdConst.cmdMoveToFolder Action.MoveToFolder
