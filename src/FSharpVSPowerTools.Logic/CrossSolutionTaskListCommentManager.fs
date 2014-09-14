namespace FSharpVSPowerTools.TaskList

open System
open FSharpVSPowerTools.ProjectSystem
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open System.IO
open Microsoft.VisualStudio.Shell

type CrossSolutionTaskListCommentManager(serviceProvider: IServiceProvider) =
    let slnEventsListener = new SolutionEventsListener(serviceProvider)
    let optionsReader = new OptionsReader(serviceProvider)
    let optionsMonitor = new OptionsMonitor(serviceProvider)
    let dte = serviceProvider.GetService<DTE, SDTE>()
    do TaskListManager.Initialize(new TaskProvider(serviceProvider))
    let taskListManager = TaskListManager.GetInstance()

    static let mutable openDocsTracker: OpenDocumentsTracker option = None

    let getTaskListCommentsFromFiles options =
        let preferOpenDocOverDiskContent filePath =
            (filePath, match openDocsTracker.Value.TryFindOpenDocument(filePath) with
                       | Some(doc) -> doc.Snapshot.GetText().Split([| Environment.NewLine |], StringSplitOptions.None)
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

    static member SetOpenDocumentsTracker(tracker) =
        openDocsTracker <- Some(tracker)

    member __.Activate() =
        slnEventsListener.SolutionOpened.AddHandler(onSolutionOpened)
        slnEventsListener.SolutionClosed.AddHandler(onSolutionClosed)
        optionsMonitor.OptionsChanged.AddHandler(onOptionsChanged)
        optionsMonitor.Start()

    member __.Deactivate() =
        slnEventsListener.SolutionOpened.RemoveHandler(onSolutionOpened)
        slnEventsListener.SolutionClosed.RemoveHandler(onSolutionClosed)
        optionsMonitor.OptionsChanged.RemoveHandler(onOptionsChanged)
        optionsMonitor.Stop()