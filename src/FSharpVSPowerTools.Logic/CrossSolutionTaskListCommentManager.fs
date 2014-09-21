namespace FSharpVSPowerTools.TaskList

open System
open FSharpVSPowerTools.ProjectSystem
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open System.IO
open Microsoft.VisualStudio.Shell
open EnvDTE80

type CrossSolutionTaskListCommentManager(serviceProvider: IServiceProvider) =
    let optionsReader = new OptionsReader(serviceProvider)
    let optionsMonitor = new OptionsMonitor(serviceProvider)
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events = dte.Events :?> Events2
    let projectItemsEvents = events.ProjectItemsEvents
    let solutionEvents = events.SolutionEvents
    
    do TaskListManager.Initialize(new TaskProvider(serviceProvider))
    let taskListManager = TaskListManager.GetInstance()
    let projectFactory = new BasicProjectFactory(events)

    static let mutable openDocsTracker: OpenDocumentsTracker option = None

    let getTaskListCommentsFromFiles options =
        let preferOpenDocOverDiskContent filePath =
            (filePath, match openDocsTracker.Value.TryFindOpenDocument(filePath) with
                       | Some(doc) -> doc.Snapshot.GetText().Split([| Environment.NewLine; "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
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

    let repopulateTaskList options =
        taskListManager.ClearTaskList()
        getTaskListCommentsFromFiles options
        |> Array.iter taskListManager.MergeTaskListComments

    let onSolutionClosed () =
        taskListManager.ClearTaskList()
    let onSolutionOpened () =
        populateTaskList (optionsReader.GetOptions())
    let onOptionsChanged =
        new Handler<OptionsChangedEventArgs>(fun _ e -> repopulateTaskList e.NewOptions)

    let isCompiledFSharpProjectItem (pi: ProjectItem) =
        isFSharpProject pi.ContainingProject &&
        match pi.TryGetProperty "ItemType" with | Some("Compile") -> true | _ -> false

    let onProjectAdded (proj: Project) =
        if isFSharpProject proj then
            let options = optionsReader.GetOptions()
            let commentExtractor = new CommentExtractor(options)
            projectFactory.CreateForProject(proj).SourceFiles
            |> Array.iter (fun file -> (file, commentExtractor.GetComments(file, File.ReadAllLines(file)))
                                       |> taskListManager.MergeTaskListComments)

    let onProjectRemoved (proj: Project) =
        if isFSharpProject proj then
            projectFactory.CreateForProject(proj).SourceFiles
            |> Array.iter (fun file -> taskListManager.MergeTaskListComments(file, [||]))

    let onProjectItemAdded (projItem: ProjectItem) =
        if isCompiledFSharpProjectItem projItem then
            let options = optionsReader.GetOptions()
            let filePath = projItem.GetProperty("FullPath")
            let comments = (new CommentExtractor(options)).GetComments(filePath, File.ReadAllLines(filePath))
            taskListManager.AddToTaskList(comments)

    let onProjectItemRemoved (projItem: ProjectItem) =
        if isCompiledFSharpProjectItem projItem then
            taskListManager.MergeTaskListComments(projItem.GetProperty("FullPath"), [||])

    let onProjectItemRenamed (projItem: ProjectItem) (oldName: string) =
        if isCompiledFSharpProjectItem projItem then
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
        optionsMonitor.OptionsChanged.AddHandler(onOptionsChanged)
        optionsMonitor.Start()
        projectItemsEvents.add_ItemAdded(fun pi -> onProjectItemAdded pi)
        projectItemsEvents.add_ItemRemoved(fun pi -> onProjectItemRemoved pi)
        projectItemsEvents.add_ItemRenamed(fun pi oldName -> onProjectItemRenamed pi oldName)
        solutionEvents.add_ProjectAdded(fun p -> onProjectAdded p)
        solutionEvents.add_ProjectRemoved(fun p -> onProjectRemoved p)
        solutionEvents.add_Opened(fun () -> onSolutionOpened ())
        solutionEvents.add_AfterClosing(fun () -> onSolutionClosed ())