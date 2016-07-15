namespace FSharp.Editing.VisualStudio.TaskList

open System
open FSharp.Editing
open FSharp.Editing.VisualStudio
open EnvDTE
open Microsoft.VisualStudio.Shell.Interop
open System.IO
open EnvDTE80
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open System.Collections.Generic
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.Editing.Features

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
     [<Import(typeof<FileSystem>)>] fileSystem: IFileSystem,
     taskListManager: TaskListManager,
     projectFactory: ProjectFactory) =
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events = dte.Events :?> Events2
    let projectItemsEvents = events.ProjectItemsEvents
    let solutionEvents = events.SolutionEvents

    let optionsReader = OptionsReader(serviceProvider)
    let optionsMonitor = new OptionsMonitor(serviceProvider)
    let mutable options = optionsReader.GetOptions()

    let fileChangeService = serviceProvider.GetService<IVsFileChangeEx, SVsFileChangeEx>()
    let fileChangeMonitor = FileChangeMonitor()
    
    let fileChangeCookies = Dictionary()
    let trackedChange = uint32 _VSFILECHANGEFLAGS.VSFILECHG_Time

    let readAllLinesSafe filePath =
        protectOrDefault 
            (fun _ -> 
                if fileSystem.SafeExists(filePath) then
                    [|
                        use reader = new StreamReader(fileSystem.FileStreamReadShim(filePath))
                        while not reader.EndOfStream do
                            yield reader.ReadLine()
                    |]
                 else [||])
            [||]

    let getTaskListCommentsFromFiles () =
        let preferOpenDocOverDiskContent filePath =
            (filePath, readAllLinesSafe(filePath))
        
        let sources =
            projectFactory.ListFSharpProjectsInSolution(dte)
            |> List.map projectFactory.CreateForProject
            |> List.toArray
            |> Array.collect (fun projProvider -> projProvider.SourceFiles)
            |> Array.map preferOpenDocOverDiskContent

        sources
        |> Array.map (fun (file, lines) -> (file, CommentExtractor.getComments options file lines))

    let populateTaskList = getTaskListCommentsFromFiles
                           >> Array.iter taskListManager.MergeTaskListComments

    let repopulateTaskList = taskListManager.ClearTaskList
                             >> getTaskListCommentsFromFiles
                             >> Array.iter taskListManager.MergeTaskListComments

    let handleFilesChanged files =
        files
        |> Array.map (fun file -> (file, CommentExtractor.getComments options file (readAllLinesSafe(file))))
        |> Array.iter taskListManager.MergeTaskListComments
                
    let onSolutionClosed () =
        fileChangeCookies
        |> Seq.iter (fun (KeyValue(_, cookie)) -> fileChangeService.UnadviseFileChange(cookie) |> ignore)

        taskListManager.ClearTaskList()

    let onSolutionOpened () =
        // Move the main computation off the UI thread.
        async {
          time "[Populating task list when opening solution]" <| fun _ ->
            fileChangeCookies.Clear()
            projectFactory.ListFSharpProjectsInSolution(dte)
            |> Seq.map projectFactory.CreateForProject
            |> Seq.collect (fun projProvider -> projProvider.SourceFiles)
            |> Seq.iter (fun file ->
                            let cookie = ref 0u
                            fileChangeService.AdviseFileChange(file, trackedChange, fileChangeMonitor, cookie) |> ignore
                            fileChangeCookies.[file] <- !cookie)

            populateTaskList ()
        } |> Async.StartInThreadPoolSafe

    let onOptionsChanged (arg: OptionsChangedEventArgs) =
        options <- arg.NewOptions
        // Move the main computation off the UI thread.
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
            |> Array.iter (fun file -> 
                taskListManager.MergeTaskListComments(file, CommentExtractor.getComments options file (readAllLinesSafe(file)))
                let cookie = ref 0u
                fileChangeService.AdviseFileChange(file, trackedChange, fileChangeMonitor, cookie) |> ignore
                fileChangeCookies.[file] <- !cookie)

    let tryRemoveFileCookie filePath =
        match fileChangeCookies.TryGetValue(filePath) with
        | true, cookie ->
            fileChangeService.UnadviseFileChange(cookie) |> ignore
            fileChangeCookies.Remove(filePath) |> ignore
        | _ -> ()

    let onProjectRemoved (proj: Project) =
        if isFSharpProject proj then
            projectFactory.CreateForProject(proj).SourceFiles
            |> Array.iter (fun filePath ->
                               taskListManager.MergeTaskListComments(filePath, [||])
                               tryRemoveFileCookie filePath)

    let onProjectItemAdded (projItem: ProjectItem) =
        if isCompiledFSharpProjectItem projItem then
            let filePath = projItem.GetProperty("FullPath")
            let comments = CommentExtractor.getComments options filePath (readAllLinesSafe(filePath))
            taskListManager.AddToTaskList(comments)

            let cookie = ref 0u
            fileChangeService.AdviseFileChange(filePath, trackedChange, fileChangeMonitor, cookie) |> ignore
            fileChangeCookies.[filePath] <- !cookie

    let onProjectItemRemoved (projItem: ProjectItem) =
        if isCompiledFSharpProjectItem projItem then
            let filePath = projItem.GetProperty("FullPath")
            taskListManager.MergeTaskListComments(filePath, [||])
            tryRemoveFileCookie filePath

    let onProjectItemRenamed (projItem: ProjectItem) (oldName: string) =
        if isCompiledFSharpProjectItem projItem then
            let newFilePath = projItem.GetProperty("FullPath")
            let oldFilePath =
                let dirName = Path.GetDirectoryName(newFilePath)
                Path.Combine(dirName, oldName)

            let comments = CommentExtractor.getComments options newFilePath (readAllLinesSafe(newFilePath))
            taskListManager.AddToTaskList(comments)
            taskListManager.MergeTaskListComments(oldFilePath, [||])

            let cookie = ref 0u
            fileChangeService.AdviseFileChange(newFilePath, trackedChange, fileChangeMonitor, cookie) |> ignore
            fileChangeCookies.[newFilePath] <- !cookie
            tryRemoveFileCookie oldFilePath

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
        member __.Dispose() = 
            fileChangeSubscription.Dispose()
            optionsChangeSubscription.Dispose()
            (optionsMonitor :> IDisposable).Dispose()
        