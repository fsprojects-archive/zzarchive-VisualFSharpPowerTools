namespace FSharpVSPowerTools.TaskList

open System
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools.ProjectSystem

type TaskListCommentFilter(view: IWpfTextView,
                           serviceProvider: IServiceProvider,
                           openDocsTracker: OpenDocumentsTracker) =
    let optionsReader = new OptionsReader(serviceProvider)
    do CrossSolutionTaskListCommentManager.SetOpenDocumentsTracker(openDocsTracker)

    let handleTextChanged (newText: string) =
        let filePath =
            let docProperty = view.TextBuffer.Properties.PropertyList
                              |> Seq.find (fun p -> p.Key <> null && obj.Equals(p.Key, typeof<ITextDocument>))
            (docProperty.Value :?> ITextDocument).FilePath

        let lines = newText.Split([| Environment.NewLine |], StringSplitOptions.None)

        (new CommentExtractor(optionsReader.GetOptions())).GetComments(filePath, lines)
        |> TaskListManager.GetInstance().MergeTaskListComments

    let onTextChanged () =
        view.TextBuffer.CurrentSnapshot.GetText() |> handleTextChanged

    let docEventListener =
        new DocumentEventListener([ViewChange.bufferEvent view.TextBuffer], 1250us, onTextChanged)

    interface IDisposable with
        member x.Dispose() = 
            (docEventListener :> IDisposable).Dispose()