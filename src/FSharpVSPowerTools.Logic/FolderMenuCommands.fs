namespace FSharpVSPowerTools.Folders

open EnvDTE
open EnvDTE80
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.OLE.Interop
open System
open System.IO
open System.ComponentModel.Composition
open System.ComponentModel.Design
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Reflection

[<RequireQualifiedAccess>]
type VerticalMoveAction = 
    | MoveUp
    | MoveDown

[<RequireQualifiedAccess>]
type NameAction = 
    | New
    | Rename

type Action = 
    | NameAction of NameAction
    | VerticalMoveAction of VerticalMoveAction
    | MoveToFolder

[<NoComparison; NoEquality>]
type ActionInfo = 
    { Items: ProjectItem list
      Project: Project }

type Name = string
type Path = string

[<RequireQualifiedAccess>]
type RenameItem =
    | Folder of Name * RenameItem list
    | File of Path
    | Unsupported

[<Export>]
type FSharpProjectSystemService [<ImportingConstructor>] (dte: DTE) = 
    
    let assemblyInfo = 
        let version = VisualStudioVersion.fromDTEVersion dte.Version
        String.Format("FSharp.ProjectSystem.FSharp, Version={0}.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a", 
                        VisualStudioVersion.toString version)
    
    let asm = 
        lazy try 
                 Assembly.Load(assemblyInfo)
             with _ -> raise (AssemblyMissingException "FSharp.ProjectSystem.FSharp")
    
    let MSBuildUtilitiesType = lazy asm.Value.GetType("Microsoft.VisualStudio.FSharp.ProjectSystem.MSBuildUtilities")
    member __.MoveFolderDown item next project: unit = MSBuildUtilitiesType.Value?MoveFolderDown (item, next, project)
    member __.MoveFolderUp item next project: unit = MSBuildUtilitiesType.Value?MoveFolderUp (item, next, project)

type FolderMenuCommands(dte: DTE2, mcs: OleMenuCommandService, shell: IVsUIShell) = 
    let rec getFolderNamesFromItems (items: ProjectItems) = 
        seq { 
            for item in items do
                if isPhysicalFolder item then 
                    yield item.Name
                    yield! getFolderNamesFromItems item.ProjectItems
        }
    
    let getFolderNamesFromProject (project: Project) = Set(getFolderNamesFromItems project.ProjectItems)
    
    let rec getFoldersFromItems (items: ProjectItems) = 
        [ for item in items do
              if isPhysicalFolder item then 
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
                if isPhysicalFolder item then 
                    yield item
                    yield! getFolderItems item.ProjectItems
        }
    
    let getFolderItemByName (project: Project) name = 
        getFolderItems project.ProjectItems |> Seq.tryFind (fun i -> i.Name = name)
    
    let getActionInfo() = 
        let items = getSelectedFromSolutionExplorer<ProjectItem> dte
        let projects = getSelectedFromSolutionExplorer<Project> dte
        match items, projects with
        | [], [ project ] -> Some { Items = []; Project = project }
        | [ item ], [] -> Some { Items = [ item ]; Project = item.ContainingProject }
        | _ :: _, [] -> 
            items
            |> List.toSeq 
            |> Seq.map (fun i -> i.ContainingProject)
            |> Seq.distinctBy (fun p -> p.FullName)
            |> Seq.toList
            |> function 
               | [ project ] -> Some { Items = items; Project = project } // items from the same project
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
    
    let performVerticalMove (info: ActionInfo) action = 
        match info.Items with
        | [ item ] -> 
            let service = new FSharpProjectSystemService(dte :?> DTE)
            let node = item?Node
            let project = info.Project?Project
            match action with
            | VerticalMoveAction.MoveUp -> service.MoveFolderUp node (getPreviousItem node) project
            | VerticalMoveAction.MoveDown -> service.MoveFolderDown node (getNextItem node) project
            project?SetProjectFileDirty true
            project?ComputeSourcesAndFlags()
            ()
        | [] -> Logging.logError "performVerticalMoveAction called with empty info.Items"
        | _ -> Logging.logError "performVerticalMoveAction called with more than one item in info.Items"
    
    let askForDestinationFolder resources = 
        let model = MoveToFolderDialogModel resources
        let wnd = FolderMenuUI.loadMoveToFolderDialog model
        let res = showDialog wnd shell
        match res with
        | Some true -> model.SelectedFolder
        | _ -> None
    
    let performMoveToFolder (info: ActionInfo) (folder: Folder) = 
        let destination = 
            if folder.IsProject then Some info.Project.ProjectItems
            else 
                getFolderItemByName info.Project folder.Name
                |> Option.map (fun x -> x.ProjectItems)
        
        let folderExists =
            if Directory.Exists(folder.FullPath) then true
            else
                protectOrDefault (fun _ -> 
                    Directory.CreateDirectory(folder.FullPath) |> ignore; true) 
                    false
        match folderExists, destination with
        | true, Some destination ->
            for item in info.Items do
                let filePath = VSUtils.filePath item
                let buildAction = item.TryGetProperty("ItemType")
                let newItem = destination.AddFromFileCopy(filePath)
                // The new item may lose ItemType; we try to recover it.
                buildAction |> Option.iter (fun buildAction -> 
                    let property = newItem.Properties.["ItemType"]
                    property.Value <- buildAction)
                item.Delete()
            info.Project.IsDirty <- true
        | _ ->
            Logging.messageBoxError Resource.validationDestinationFolderDoesNotExist
    
    let askForFolderName resources = 
        let model = NewFolderNameDialogModel resources
        let wnd = FolderMenuUI.loadNewFolderDialog model
        let res = showDialog wnd shell
        match res with
        | Some true -> Some model.Name
        | _ -> None
    
    let performNewFolder (info: ActionInfo) name = 
        let items = 
            match info.Items with
            | [ item ] -> item.ProjectItems
            | _ -> info.Project.ProjectItems
        let folder = items.AddFolder name
        let folderFilename = VSUtils.filePath folder
        if Directory.Exists(folderFilename) then
            // We tolerate that folder might already exist
            Logging.messageBoxInfo Resource.validationExistingFolderOnDisk
        else
            try
                Directory.CreateDirectory(folderFilename) |> ignore
            with _ ->
                Logging.messageBoxError Resource.validationCannotCreateFolder
                // Can't create folder, remove folder item for consistency
                folder.Remove()        

    let performRenameFolder (info: ActionInfo) name = 
        let project = info.Project?Project
        // Try to sync with internal project system        
        project?ComputeSourcesAndFlags()

        let folder = 
            match info.Items with
            | [ item ] -> Some item
            | _ -> 
                if info.Project.ProjectItems.Count > 0 then
                    Some info.Project.ProjectItems.[0]
                else None

        match folder with
        | Some folder ->
            /// ProjectItem will be disposed once we remove them from the parent node
            /// We capture required information for renaming i.e. replacing paths by those in new folder names
            let rec createRenameItem path (item: ProjectItem) =
                if isPhysicalFolder item then
                    let subItems = 
                        [
                            for subItem in item.ProjectItems -> 
                                createRenameItem (Path.Combine(path, item.Name)) subItem 
                        ]
                    RenameItem.Folder(item.Name, subItems)                
                elif isPhysicalFile item then
                    RenameItem.File(Path.Combine(path, item.Name))
                else
                    RenameItem.Unsupported

            let path: string = folder.Object?Url
            let parentPath = Directory.GetParent(path.TrimEnd('\\')).FullName
            let newPath = Path.Combine(parentPath, name)

            if Directory.Exists(newPath) then
                Logging.messageBoxError Resource.validationRenameFolderAlreadyExistsOnDisk
            else
                // F# project system can't rename folders with any item in it.
                // We remove all items in the folder and add them back later       
                let items = ResizeArray()        
                for it in folder.ProjectItems do
                    items.Add(createRenameItem newPath it)            
                    it.Remove()

                // Rename the empty folder (on Solution Explorer and on disk)
                folder.Name <- name

                let rec addToFolder (folder: ProjectItem) (item: RenameItem) =
                    match item with
                    | RenameItem.Folder(name, subItems) ->
                        let subFolder = folder.ProjectItems.AddFolder(name)
                        for subItem in subItems do
                            addToFolder subFolder subItem
                    | RenameItem.File path ->
                        folder.ProjectItems.AddFromFile(path) |> ignore
                    | RenameItem.Unsupported -> ()

                // Add back all children recursively
                for item in items do
                    addToFolder folder item

                // Synchronize between external changes and internal representation of F# project system        
                project?SetProjectFileDirty true
                project?ComputeSourcesAndFlags()
            | _ -> 
                Logging.logWarning "Can't find folder '%s'for renaming." name

    let performNameAction (info: ActionInfo) action name =
        match action with
        | NameAction.New -> performNewFolder info name
        | NameAction.Rename -> performRenameFolder info name

    let executeCommand (action: Action) = 
        let actionInfo = getActionInfo()
        match actionInfo with
        | Some info -> 
            match action with
            | Action.NameAction a -> 
                let folderNames = getFolderNamesFromProject info.Project
                let uniqueFolderName =
                    match VisualStudioVersion.fromDTEVersion dte.Version with
                    | VisualStudioVersion.VS2012
                    | VisualStudioVersion.VS2013 ->
                        true
                    | VisualStudioVersion.VS2015 -> 
                        false
                    | v ->
                        Logging.logWarning "Unknown Visual Studio version detected while adding/renaming folder: %O" v
                        true

                let resources = 
                    match a with
                    | NameAction.New ->
                        { WindowTitle = Resource.newFolderDialogTitle
                          OriginalName = ""
                          CheckUniqueFolderNames = uniqueFolderName
                          FolderNames = folderNames }
                    | NameAction.Rename -> 
                        { WindowTitle = Resource.renameFolderDialogTitle
                          OriginalName = info.Items.Head.Name
                          CheckUniqueFolderNames = uniqueFolderName
                          FolderNames = folderNames }
                askForFolderName resources |> Option.iter (performNameAction info a)
            | Action.VerticalMoveAction a -> performVerticalMove info a
            | Action.MoveToFolder -> 
                let resources = 
                    { FileNames = info.Items |> List.map (fun item -> Path.GetFileName item.Object?Url)
                      Root = getFoldersFromProject info.Project }
                askForDestinationFolder resources |> Option.iter (performMoveToFolder info)
        | None -> fail "actionInfo is None"
    
    let isVerticalMoveCommandEnabled info action = 
        match info.Items with
        | [ item ] -> 
            if not (isPhysicalFolder item) then false
            else 
                let checkItem item = 
                    item <> null && isPhysicalFileOrFolderKind (item?ItemTypeGuid?ToString ("B"))
                match action with
                | VerticalMoveAction.MoveUp -> checkItem item?Node?PreviousSibling
                | VerticalMoveAction.MoveDown -> checkItem item?Node?NextSibling
        | _ -> false
    
    let isNameCommandEnabled info action = 
        match info.Items with
        | [ item ] -> isPhysicalFolder item
        | [] -> action = NameAction.New
        | _ -> false
    
    let isMoveToFolderCommandEnabled info = 
        match info.Items with
        | [] -> false
        | _ -> 
            let filesOnly = info.Items |> List.forall isPhysicalFile
            let distinctByName = info.Items |> Seq.distinctBy (fun i -> i.Name)
            filesOnly && (Seq.length distinctByName = List.length info.Items)
    
    let isCommandEnabled (actionInfo: ActionInfo option) (action: Action) = 
        match actionInfo with
        | Some info -> 
            if isFSharpProject info.Project then 
                match action with
                | Action.NameAction a -> isNameCommandEnabled info a
                | Action.VerticalMoveAction a -> isVerticalMoveCommandEnabled info a
                | Action.MoveToFolder -> isMoveToFolderCommandEnabled info
            else false
        | None -> false
    
    let setupCommand guid id action = 
        let id = CommandID(guid, id)
        let command = OleMenuCommand((fun _ _ -> executeCommand action), id)
        command.BeforeQueryStatus.AddHandler
            (fun s _ -> (s :?> OleMenuCommand).Enabled <- (isCommandEnabled (getActionInfo()) action))
        mcs.AddCommand command
    
    let setupNewFolderCommand = setupCommand Constants.guidNewFolderCmdSet
    let setupMoveCommand = setupCommand Constants.guidMoveCmdSet

    member __.SetupCommands() = 
        setupNewFolderCommand Constants.cmdNewFolder (NameAction NameAction.New)
        setupNewFolderCommand Constants.cmdRenameFolder (NameAction NameAction.Rename)
        setupMoveCommand Constants.cmdMoveFolderUp (VerticalMoveAction VerticalMoveAction.MoveUp)
        setupMoveCommand Constants.cmdMoveFolderDown (VerticalMoveAction VerticalMoveAction.MoveDown)
        setupMoveCommand Constants.cmdMoveToFolder Action.MoveToFolder

    interface IOleCommandTarget with
        member __.QueryStatus(pguidCmdGroup: byref<Guid>, _cCmds: uint32, prgCmds: OLECMD [], _pCmdText: IntPtr): int = 
            if pguidCmdGroup = Constants.guidSolutionExplorerCmdSet &&
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.fsPowerToolsSubMenuGroup) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            elif pguidCmdGroup = Constants.guidOldStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdStandardNewFolder) then
                match getActionInfo() with
                | Some info ->
                    if isFSharpProject info.Project then
                        prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_INVISIBLE)
                        VSConstants.S_OK
                    else
                        int Constants.OLECMDERR_E_UNKNOWNGROUP
                | None ->
                    int Constants.OLECMDERR_E_UNKNOWNGROUP
            elif pguidCmdGroup = Constants.guidOldStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdStandardRenameFolder) then
                match getActionInfo() with
                | Some info ->
                    if isFSharpProject info.Project && List.exists isPhysicalFolder info.Items then
                        prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_INVISIBLE)
                        VSConstants.S_OK
                    else
                        int Constants.OLECMDERR_E_UNKNOWNGROUP
                | None ->
                    int Constants.OLECMDERR_E_UNKNOWNGROUP
            else
                int Constants.OLECMDERR_E_UNKNOWNGROUP
        
        member __.Exec(_pguidCmdGroup: byref<Guid>, _nCmdID: uint32, _nCmdexecopt: uint32, _pvaIn: IntPtr, _pvaOut: IntPtr): int = 
            // We should not return OK here because it would short-circuit the command chain
            int Constants.OLECMDERR_E_UNKNOWNGROUP
