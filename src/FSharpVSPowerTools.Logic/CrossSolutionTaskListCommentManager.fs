namespace FSharpVSPowerTools.TaskList

open System
open FSharpVSPowerTools.ProjectSystem
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open System.IO
open EnvDTE80
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell

type internal FilesChangedEventArgs(files: string []) =
    inherit EventArgs()

    member __.Files = files

type internal FileChangeMonitor() =
    let filesChanged = Event<FilesChangedEventArgs>()
    [<CLIEvent>]
    member __.FilesChanged = filesChanged.Publish

    interface IVsFileChangeEvents with
        member __.DirectoryChanged(_pszDirectory: string): int = 
            Microsoft.VisualStudio.VSConstants.E_NOTIMPL
        
        member __.FilesChanged(_cChanges: uint32, rgpszFile: string [], _rggrfChange: uint32 []): int = 
            filesChanged.Trigger(new FilesChangedEventArgs(rgpszFile))
            Microsoft.VisualStudio.VSConstants.S_OK
        
[<Export>]
type CrossSolutionTaskListCommentManager [<ImportingConstructor>]
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider,
     openDocsTracker: OpenDocumentsTracker,
     taskListManager: TaskListManager,
     projectFactory: ProjectFactory) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events = dte.Events :?> Events2
    let projectItemsEvents = events.ProjectItemsEvents
    let solutionEvents = events.SolutionEvents

    let optionsReader = OptionsReader(serviceProvider)
    let optionsMonitor = OptionsMonitor(serviceProvider)
    let mutable options = optionsReader.GetOptions()

    let fileChangeService = serviceProvider.GetService<IVsFileChangeEx, SVsFileChangeEx>()
    let fileChangeMonitor = FileChangeMonitor()
    
    let mutable fileChangeCookies = Map.empty<string, uint32>
    let trackedChange = uint32 _VSFILECHANGEFLAGS.VSFILECHG_Time

    let readAllLinesSafe filePath =
        protectOrDefault 
            (fun _ -> File.ReadAllLines(filePath))
            [||]

    let newLines = [| Environment.NewLine; "\r\n"; "\r"; "\n" |]
    let getTaskListCommentsFromFiles () =
        let preferOpenDocOverDiskContent filePath =
            (filePath, match openDocsTracker.TryFindOpenDocument(filePath) with
                       | Some(doc) -> doc.Snapshot.GetText().Split(newLines, StringSplitOptions.None)
                       | None -> readAllLinesSafe(filePath))
        
        let sources =
            projectFactory.ListFSharpProjectsInSolution(dte)
            |> List.map projectFactory.CreateForProject
            |> List.toArray
            |> Array.collect (fun projProvider -> projProvider.SourceFiles)
            |> Array.map preferOpenDocOverDiskContent

        sources
        |> Array.map (fun (file, lines) -> (file, getComments options file lines))

    let populateTaskList = getTaskListCommentsFromFiles
                           >> Array.iter taskListManager.MergeTaskListComments

    let repopulateTaskList = taskListManager.ClearTaskList
                             >> getTaskListCommentsFromFiles
                             >> Array.iter taskListManager.MergeTaskListComments

    let handleFilesChanged files =
        let handleFilesChangedOutsideVS files =
            files
            |> Array.map (fun file -> (file, getComments options file (readAllLinesSafe(file))))
            |> Array.iter taskListManager.MergeTaskListComments
        
        files
        |> Array.map (fun f -> (f, openDocsTracker.TryFindOpenDocument(f)))
        |> Array.filter (fun (_, doc) -> doc.IsNone)
        |> Array.map (fun (f, _) -> f)
        |> handleFilesChangedOutsideVS

    let onSolutionClosed () =
        fileChangeCookies
        |> Map.iter (fun _ cookie -> fileChangeService.UnadviseFileChange(cookie) |> ignore)

        taskListManager.ClearTaskList()

    let onSolutionOpened () =
        async {
          time "[Populate task list when opening solution]" <| fun _ ->
            fileChangeCookies <-
                projectFactory.ListFSharpProjectsInSolution(dte)
                |> Seq.map projectFactory.CreateForProject
                |> Seq.collect (fun projProvider -> projProvider.SourceFiles)
                |> Seq.map (fun file ->
                                let cookie = ref 0u
                                fileChangeService.AdviseFileChange(file, trackedChange, fileChangeMonitor, cookie) |> ignore
                                (file, !cookie))
                |> Map.ofSeq

            populateTaskList ()
        } |> Async.StartInThreadPoolSafe

    let onOptionsChanged (arg: OptionsChangedEventArgs) =
        options <- arg.NewOptions
        async { 
            repopulateTaskList () 
        }
        |> Async.StartInThreadPoolSafe

    let onFilesChanged (arg: FilesChangedEventArgs) =
        handleFilesChanged arg.Files

    let isCompiledFSharpProjectItem (pi: ProjectItem) =
        isFSharpProject pi.ContainingProject &&
        match pi.TryGetProperty "ItemType" with | Some("Compile") -> true | _ -> false

    let onProjectAdded (proj: Project) =
        if isFSharpProject proj then
            projectFactory.CreateForProject(proj).SourceFiles
            |> Array.iter (fun file -> taskListManager.MergeTaskListComments(file, getComments options file (readAllLinesSafe(file)))
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
            let comments = getComments options filePath (readAllLinesSafe(filePath))
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

            let comments = getComments options newFilePath (readAllLinesSafe(newFilePath))
            taskListManager.AddToTaskList(comments)
            taskListManager.MergeTaskListComments(oldFilePath, [||])

            let cookie = ref 0u
            fileChangeService.AdviseFileChange(newFilePath, trackedChange, fileChangeMonitor, cookie) |> ignore
            fileChangeCookies <- fileChangeCookies.Add(newFilePath, !cookie)
            fileChangeService.UnadviseFileChange(fileChangeCookies.[oldFilePath]) |> ignore
            fileChangeCookies <- fileChangeCookies.Remove(oldFilePath)

    let fileChangeSubscription = fileChangeMonitor.FilesChanged.Subscribe(onFilesChanged)
    let optionsChangeSubscription = optionsMonitor.OptionsChanged.Subscribe(onOptionsChanged)

    member __.Activate() =
        optionsMonitor.Start()
        projectItemsEvents.add_ItemAdded(fun pi -> onProjectItemAdded pi)
        projectItemsEvents.add_ItemRemoved(fun pi -> onProjectItemRemoved pi)
        projectItemsEvents.add_ItemRenamed(fun pi oldName -> onProjectItemRenamed pi oldName)
        solutionEvents.add_ProjectAdded(fun p -> onProjectAdded p)
        solutionEvents.add_ProjectRemoved(fun p -> onProjectRemoved p)
        solutionEvents.add_Opened(fun () -> onSolutionOpened ())
        solutionEvents.add_AfterClosing(fun () -> onSolutionClosed ())

    interface IDisposable with
        member __.Dispose(): unit = 
            fileChangeSubscription.Dispose()
            optionsChangeSubscription.Dispose()
        