namespace FSharpVSPowerTools.TaskList

open System
open FSharpVSPowerTools.ProjectSystem
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open System.IO
open EnvDTE80

type internal FilesChangedEventArgs(files: string[]) =
    inherit EventArgs()

    member __.Files = files


type internal FileChangeMonitor() =
    let filesChanged = new Event<FilesChangedEventArgs>()
    [<CLIEvent>]
    member __.FilesChanged = filesChanged.Publish

    interface IVsFileChangeEvents with
        member __.DirectoryChanged(_pszDirectory: string): int = 
            Microsoft.VisualStudio.VSConstants.E_NOTIMPL
        
        member __.FilesChanged(_cChanges: uint32, rgpszFile: string [], _rggrfChange: uint32 []): int = 
            filesChanged.Trigger(new FilesChangedEventArgs(rgpszFile))
            Microsoft.VisualStudio.VSConstants.S_OK
        

type CrossSolutionTaskListCommentManager(serviceProvider: IServiceProvider) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events = dte.Events :?> Events2
    let projectItemsEvents = events.ProjectItemsEvents
    let solutionEvents = events.SolutionEvents

    let optionsReader = new OptionsReader(serviceProvider)
    let optionsMonitor = new OptionsMonitor(serviceProvider)
    let mutable options = optionsReader.GetOptions()

    let fileChangeService = serviceProvider.GetService<IVsFileChangeEx, SVsFileChangeEx>()
    let fileChangeMonitor = new FileChangeMonitor()
    
    do TaskListManager.Initialize(serviceProvider)
    let taskListManager = TaskListManager.GetInstance()
    let projectFactory = new BasicProjectFactory(events)

    let mutable fileChangeCookies = Map.empty<string, uint32>
    let trackedChange = uint32 (LanguagePrimitives.EnumToValue(_VSFILECHANGEFLAGS.VSFILECHG_Time))

    static let mutable openDocsTracker: OpenDocumentsTracker option = None

    let newLines = [| Environment.NewLine; "\r\n"; "\r"; "\n" |]
    let getTaskListCommentsFromFiles () =
        let preferOpenDocOverDiskContent filePath =
            (filePath, match openDocsTracker.Value.TryFindOpenDocument(filePath) with
                       | Some(doc) -> doc.Snapshot.GetText().Split(newLines, StringSplitOptions.None)
                       | None -> File.ReadAllLines(filePath))
        
        let sources =
            projectFactory.ListFSharpProjectsInSolution(dte)
            |> List.map projectFactory.CreateForProject
            |> List.toArray
            |> Array.collect (fun projProvider -> projProvider.SourceFiles)
            |> Array.map (fun filePath -> match openDocsTracker with
                                          | Some(_) -> preferOpenDocOverDiskContent filePath
                                          | None -> (filePath, File.ReadAllLines(filePath)))

        let commentExtractor = new CommentExtractor(options)
        sources
        |> Array.map (fun (filePath, lines) -> (filePath, commentExtractor.GetComments(filePath, lines)))

    let populateTaskList = getTaskListCommentsFromFiles
                           >> Array.iter taskListManager.MergeTaskListComments

    let repopulateTaskList = taskListManager.ClearTaskList
                             >> getTaskListCommentsFromFiles
                             >> Array.iter taskListManager.MergeTaskListComments

    let handleFilesChanged files =
        let handleFilesChangedOutsideVS files =
            let commentExtractor = new CommentExtractor(options)
            files
            |> Array.iter (fun file -> (file, commentExtractor.GetComments(file, File.ReadAllLines(file)))
                                       |> taskListManager.MergeTaskListComments)

        match openDocsTracker with
        | Some(tracker) ->
            files
            |> Array.map (fun f -> (f, tracker.TryFindOpenDocument(f)))
            |> Array.filter (fun (_, doc) -> doc.IsNone)
            |> Array.map (fun (f, _) -> f)
            |> handleFilesChangedOutsideVS
        | None -> handleFilesChangedOutsideVS files

    let onSolutionClosed () =
        fileChangeCookies
        |> Map.iter (fun _ cookie -> fileChangeService.UnadviseFileChange(cookie) |> ignore)

        taskListManager.ClearTaskList()

    let onSolutionOpened () =
        fileChangeCookies <-
            projectFactory.ListFSharpProjectsInSolution(dte)
            |> List.map projectFactory.CreateForProject
            |> List.collect (fun projProvider -> projProvider.SourceFiles |> Array.toList)
            |> List.map (fun file ->
                            let cookie = ref 0u
                            fileChangeService.AdviseFileChange(file, trackedChange, fileChangeMonitor, cookie) |> ignore
                            (file, !cookie))
            |> Map.ofList

        populateTaskList ()

    let onOptionsChanged =
        new Handler<OptionsChangedEventArgs>(fun _ e -> options <- e.NewOptions; repopulateTaskList ())
    let onFilesChanged =
        new Handler<FilesChangedEventArgs>(fun _ e -> handleFilesChanged e.Files)

    let isCompiledFSharpProjectItem (pi: ProjectItem) =
        isFSharpProject pi.ContainingProject &&
        match pi.TryGetProperty "ItemType" with | Some("Compile") -> true | _ -> false

    let onProjectAdded (proj: Project) =
        if isFSharpProject proj then
            let commentExtractor = new CommentExtractor(options)
            projectFactory.CreateForProject(proj).SourceFiles
            |> Array.iter (fun file -> (file, commentExtractor.GetComments(file, File.ReadAllLines(file)))
                                       |> taskListManager.MergeTaskListComments
                                       let cookie = ref 0u
                                       fileChangeService.AdviseFileChange(file, trackedChange, fileChangeMonitor, cookie) |> ignore
                                       fileChangeCookies <- fileChangeCookies.Add(file, !cookie))

    let onProjectRemoved (proj: Project) =
        if isFSharpProject proj then
            projectFactory.CreateForProject(proj).SourceFiles
            |> Array.iter (fun file ->
                               taskListManager.MergeTaskListComments(file, [||])
                               fileChangeService.UnadviseFileChange(fileChangeCookies.[file]) |> ignore
                               fileChangeCookies <- fileChangeCookies.Remove(file))

    let onProjectItemAdded (projItem: ProjectItem) =
        if isCompiledFSharpProjectItem projItem then
            let filePath = projItem.GetProperty("FullPath")
            let comments = (new CommentExtractor(options)).GetComments(filePath, File.ReadAllLines(filePath))
            taskListManager.AddToTaskList(comments)

            let cookie = ref 0u
            fileChangeService.AdviseFileChange(filePath, trackedChange, fileChangeMonitor, cookie) |> ignore
            fileChangeCookies <- fileChangeCookies.Add(filePath, !cookie)

    let onProjectItemRemoved (projItem: ProjectItem) =
        if isCompiledFSharpProjectItem projItem then
            let filePath = projItem.GetProperty("FullPath")
            taskListManager.MergeTaskListComments(filePath, [||])
            fileChangeService.UnadviseFileChange(fileChangeCookies.[filePath]) |> ignore
            fileChangeCookies <- fileChangeCookies.Remove(filePath)

    let onProjectItemRenamed (projItem: ProjectItem) (oldName: string) =
        if isCompiledFSharpProjectItem projItem then
            let newFilePath = projItem.GetProperty("FullPath")
            let oldFilePath =
                let dirName = Path.GetDirectoryName(newFilePath)
                Path.Combine(dirName, oldName)

            let comments = (new CommentExtractor(options)).GetComments(newFilePath, File.ReadAllLines(newFilePath))
            taskListManager.AddToTaskList(comments)
            taskListManager.MergeTaskListComments(oldFilePath, [||])

            let cookie = ref 0u
            fileChangeService.AdviseFileChange(newFilePath, trackedChange, fileChangeMonitor, cookie) |> ignore
            fileChangeCookies <- fileChangeCookies.Add(newFilePath, !cookie)
            fileChangeService.UnadviseFileChange(fileChangeCookies.[oldFilePath]) |> ignore
            fileChangeCookies <- fileChangeCookies.Remove(oldFilePath)

    static member SetOpenDocumentsTracker(tracker) =
        openDocsTracker <- Some(tracker)

    member __.Activate() =
        fileChangeMonitor.FilesChanged.AddHandler(onFilesChanged)
        optionsMonitor.OptionsChanged.AddHandler(onOptionsChanged)
        optionsMonitor.Start()
        projectItemsEvents.add_ItemAdded(fun pi -> onProjectItemAdded pi)
        projectItemsEvents.add_ItemRemoved(fun pi -> onProjectItemRemoved pi)
        projectItemsEvents.add_ItemRenamed(fun pi oldName -> onProjectItemRenamed pi oldName)
        solutionEvents.add_ProjectAdded(fun p -> onProjectAdded p)
        solutionEvents.add_ProjectRemoved(fun p -> onProjectRemoved p)
        solutionEvents.add_Opened(fun () -> onSolutionOpened ())
        solutionEvents.add_AfterClosing(fun () -> onSolutionClosed ())