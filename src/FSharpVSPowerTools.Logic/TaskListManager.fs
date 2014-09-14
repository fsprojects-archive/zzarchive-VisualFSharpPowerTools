namespace FSharpVSPowerTools.TaskList

open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio

[<AllowNullLiteral>]
type internal TaskListManager private (taskProvider: TaskProvider) =
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
        task
        
    let addToTaskList newTasks =
        for t in newTasks do
            ErrorHandler.ThrowOnFailure(taskProvider.Tasks.Add(t)) |> ignore

    static let mutable instance = null

    static member Initialize(taskProvider: TaskProvider) =
        instance <- new TaskListManager(taskProvider)

    static member GetInstance() =
        if instance = null then
            invalidOp "Please ensure the Initialize method is called before attempting to obtain the TaskListManager instance"
        instance

    member x.MergeTaskListComments(filePath, newComments) =
        let currentComments =
            taskProvider.Tasks
            |> Seq.cast
            |> Seq.toList

        x.AddToTaskList newComments
        
        currentComments
        |> List.filter (fun (t: Task) -> t.Document = filePath)
        |> List.iter taskProvider.Tasks.Remove

    member x.ClearTaskList() =
        taskProvider.Tasks.Clear()

    member x.AddToTaskList(comments: Comment[]) =
        comments
        |> Array.map convertCommentToTask
        |> addToTaskList