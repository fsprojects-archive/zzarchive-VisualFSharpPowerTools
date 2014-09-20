namespace FSharpVSPowerTools.TaskList

open System
open FSharpVSPowerTools.ProjectSystem
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open System.IO
open Microsoft.VisualStudio.Shell
open EnvDTE80

type CrossSolutionTaskListCommentManager(serviceProvider: IServiceProvider) =
    let slnEventsListener = new SolutionEventsListener(serviceProvider)
    let optionsReader = new OptionsReader(serviceProvider)
    let optionsMonitor = new OptionsMonitor(serviceProvider)
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let projItemsEvents = (dte.Events :?> Events2).ProjectItemsEvents
    do TaskListManager.Initialize(new TaskProvider(serviceProvider))
    let taskListManager = TaskListManager.GetInstance()

    static let mutable openDocsTracker: OpenDocumentsTracker option = None

    let getTaskListCommentsFromFiles options =
        let preferOpenDocOverDiskContent filePath =
            (filePath, match openDocsTracker.Value.TryFindOpenDocument(filePath) with
                       | Some(doc) -> doc.Snapshot.GetText().Split([| Environment.NewLine; "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
                       | None -> File.ReadAllLines(filePath))
        let projFactory = new BasicProjectFactory()
        let sources =
            projFactory.ListFSharpProjectsInSolution(dte)
            |> List.map projFactory.CreateForProject
            |> List.toArray
            |> Array.collect (fun projProvider -> projProvider.SourceFiles)
            |> Array.map (fun filePath -> match openDocsTracker with
                                          | Some(_) -> preferOpenDocOverDiskContent filePath
                                          | None -> (filePath, File.ReadAllLines(filePath)))

        sources
        |> Array.collect (new CommentExtractor(options)).GetComments

    let populateTaskList = getTaskListCommentsFromFiles
                           >> taskListManager.AddToTaskList

    let repopulateTaskList options =
        taskListManager.ClearTaskList()
        getTaskListCommentsFromFiles options
        |> taskListManager.AddToTaskList

    let onSolutionClosed =
        new Handler<EventArgs>(fun _ _ -> taskListManager.ClearTaskList())
    let onSolutionOpened =
        new Handler<EventArgs>(fun _ _ -> populateTaskList (optionsReader.GetOptions()))
    let onOptionsChanged =
        new Handler<OptionsChangedEventArgs>(fun _ e -> repopulateTaskList e.NewOptions)

    let onProjectItemAdded (projItem: ProjectItem) =
        let options = optionsReader.GetOptions()
        let filePath = projItem.GetProperty("FullPath")
        let comments = (new CommentExtractor(options)).GetComments(filePath, File.ReadAllLines(filePath))
        taskListManager.AddToTaskList(comments)

    let onProjectItemRemoved (projItem: ProjectItem) =
        taskListManager.MergeTaskListComments(projItem.GetProperty("FullPath"), [||])

    let onProjectItemRenamed (projItem: ProjectItem) (oldName: string) =
        let options = optionsReader.GetOptions()
        let newFilePath = projItem.GetProperty("FullPath")
        let oldFilePath =
            let dirName = newFilePath
                          |> Path.GetDirectoryName
            Path.Combine(dirName, oldName)

        let comments = (new CommentExtractor(options)).GetComments(newFilePath, File.ReadAllLines(newFilePath))
        taskListManager.AddToTaskList(comments)
        taskListManager.MergeTaskListComments(oldFilePath, [||])

    static member SetOpenDocumentsTracker(tracker) =
        openDocsTracker <- Some(tracker)

    member __.Activate() =
        slnEventsListener.SolutionOpened.AddHandler(onSolutionOpened)
        slnEventsListener.SolutionClosed.AddHandler(onSolutionClosed)
        optionsMonitor.OptionsChanged.AddHandler(onOptionsChanged)
        optionsMonitor.Start()
        projItemsEvents.add_ItemAdded(fun pi -> onProjectItemAdded pi)
        projItemsEvents.add_ItemRemoved(fun pi -> onProjectItemRemoved pi)
        projItemsEvents.add_ItemRenamed(fun pi oldName -> onProjectItemRenamed pi oldName)

    member __.Deactivate() =
        slnEventsListener.SolutionOpened.RemoveHandler(onSolutionOpened)
        slnEventsListener.SolutionClosed.RemoveHandler(onSolutionClosed)
        optionsMonitor.OptionsChanged.RemoveHandler(onOptionsChanged)
        optionsMonitor.Stop()