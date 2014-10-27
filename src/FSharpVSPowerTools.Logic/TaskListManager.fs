namespace FSharpVSPowerTools.TaskList

open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio
open System
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.TextManager.Interop
open System.ComponentModel.Composition

[<Export>]
type TaskListManager [<ImportingConstructor>]
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =
    let taskProvider = new TaskProvider(serviceProvider)

    let navigateTo file line column =
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>
        let isOpened = 
            VsShellUtilities.IsDocumentOpen(
                serviceProvider, 
                file, 
                Constants.LogicalViewTextGuid,
                &hierarchy,
                &itemId,
                &windowFrame)
        let canShow = 
            if isOpened then true
            else
                // TODO: track the project that contains document and open document in project context
                try
                    VsShellUtilities.OpenDocument(
                        serviceProvider, 
                        file, 
                        Constants.LogicalViewTextGuid, 
                        &hierarchy,
                        &itemId,
                        &windowFrame)
                    true
                with _ -> false
        if canShow then
            windowFrame.Show()
            |> ensureSucceeded

            let vsTextView = VsShellUtilities.GetTextView(windowFrame)
            let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
            let mutable vsTextBuffer = Unchecked.defaultof<_>
            vsTextView.GetBuffer(&vsTextBuffer)
            |> ensureSucceeded

            vsTextManager.NavigateToLineAndColumn(vsTextBuffer, ref Constants.LogicalViewTextGuid,
                                                  line, column, line, column)
            |> ensureSucceeded

    let convertCommentToTask (taskListComment: Comment) =
        let task = new Task()
        task.Text <- taskListComment.Text
        task.Category <- TaskCategory.Comments
        task.Line <- taskListComment.Line
        task.Column <- taskListComment.Column
        task.Document <- taskListComment.File
        task.Priority <- match taskListComment.Priority with
                         | 1 -> TaskPriority.Low
                         | 2 -> TaskPriority.Normal
                         | _ -> TaskPriority.High
        task.Navigate.AddHandler(fun _ _ -> navigateTo task.Document task.Line task.Column)
        task
        
    let addToTaskList newTasks =
        for t in newTasks do
            ErrorHandler.ThrowOnFailure(taskProvider.Tasks.Add(t)) |> ignore

    member x.MergeTaskListComments(filePath, newComments) =
        let currentComments =
            taskProvider.Tasks
            |> Seq.cast
            |> Seq.toList

        x.AddToTaskList newComments
        
        currentComments
        |> List.filter (fun (t: Task) -> t.Document = filePath)
        |> List.iter taskProvider.Tasks.Remove

    member __.ClearTaskList() =
        taskProvider.Tasks.Clear()

    member __.AddToTaskList(comments: Comment[]) =
        comments
        |> Array.map convertCommentToTask
        |> addToTaskList