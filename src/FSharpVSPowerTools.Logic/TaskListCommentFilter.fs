namespace FSharpVSPowerTools.TaskList

open System
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

type TaskListCommentFilter(view: IWpfTextView,
                           serviceProvider: IServiceProvider,
                           taskListManager: TaskListManager) =
    let optionsReader = OptionsReader(serviceProvider)

    let handleTextChanged (newText: string) =
        match view.TextBuffer.Properties.TryGetProperty(typeof<ITextDocument>) with
        | true, x ->
            match box x with
            | :? ITextDocument as textDocument ->
                let filePath = textDocument.FilePath
                let lines = String.getLines newText

                let comments = getComments (optionsReader.GetOptions()) filePath lines
                taskListManager.MergeTaskListComments(filePath, comments)
            | _ -> ()
        | _ -> ()

    let onTextChanged () =
        protect (fun _ ->
            view.TextBuffer.CurrentSnapshot.GetText() |> handleTextChanged)

    let docEventListener =
        new DocumentEventListener([ViewChange.bufferEvent view.TextBuffer], 1250us, onTextChanged)

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()