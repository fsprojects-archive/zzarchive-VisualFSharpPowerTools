namespace FSharp.Editing.VisualStudio.TaskList

open System
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.AsyncMaybe

type TaskListCommentFilter(view: IWpfTextView,
                           serviceProvider: IServiceProvider,
                           taskListManager: TaskListManager,
                           openDocumentTracker: IOpenDocumentsTracker) =
    let optionsReader = OptionsReader(serviceProvider)

    let onTextChanged (CallInUIContext callInUIContext) =
        asyncMaybe {
            match view.TextBuffer.Properties.TryGetProperty typeof<ITextDocument> with
            | true, x ->
                match box x with
                | :? ITextDocument as textDocument ->
                    let filePath = textDocument.FilePath
                    let! source = openDocumentTracker.TryGetDocumentText textDocument.FilePath
                    let lines = String.getLines source
                    let comments = getComments (optionsReader.GetOptions()) filePath lines
                    do! callInUIContext <| fun _ -> taskListManager.MergeTaskListComments(filePath, comments) 
                        |> liftAsync
                | _ -> ()
            | _ -> ()
        } |> Async.Ignore

    let docEventListener =
        new DocumentEventListener([ViewChange.bufferEvent view.TextBuffer], 1250us, onTextChanged)

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()