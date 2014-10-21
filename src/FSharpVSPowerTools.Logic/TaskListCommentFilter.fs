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

    static let newLines = [| Environment.NewLine; "\r\n"; "\r"; "\n" |]
    let handleTextChanged (newText: string) =
        let filePath =
            let docProperty = view.TextBuffer.Properties.PropertyList
                              |> Seq.find (fun p -> obj.Equals(p.Key, typeof<ITextDocument>))
            (docProperty.Value :?> ITextDocument).FilePath

        let lines = newText.Split(newLines, StringSplitOptions.None)

        let comments = getComments (optionsReader.GetOptions()) filePath lines
        TaskListManager.GetInstance().MergeTaskListComments(filePath, comments)

    let onTextChanged () =
        view.TextBuffer.CurrentSnapshot.GetText() |> handleTextChanged

    let docEventListener =
        new DocumentEventListener([ViewChange.bufferEvent view.TextBuffer], 1250us, onTextChanged)

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()