module FSharpVSPowerTools.TaskListCommentFilterProvider

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open FSharpVSPowerTools.TaskList
open Microsoft.VisualStudio.Editor
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.TextManager.Interop


[<Export (typeof<IVsTextViewCreationListener>)>]
[<ContentType "F#">]
[<TextViewRole (PredefinedTextViewRoles.Editable)>]
type TaskListCommentFilterProvider [<ImportingConstructor>]
    ( [<Import (typeof<SVsServiceProvider>)>] 
        serviceProvider             :   IServiceProvider                ,
        editorFactory               :   IVsEditorAdaptersFactoryService ,
        textDocumentFactoryService  :   ITextDocumentFactoryService     ,
        taskListManager             :   TaskListManager                 ) =

    let mutable taskCommentFilter = Unchecked.defaultof<TaskListCommentFilter>

    interface IVsTextViewCreationListener with

        member __.VsTextViewCreated textViewAdapter =
            maybe {
                let! textView = editorFactory.TryGetWpfTextView textViewAdapter
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                if not generalOptions.TaskListCommentsEnabled then return! None else
                taskCommentFilter <- new TaskListCommentFilter 
                                        (   textView, serviceProvider, taskListManager, 
                                            OpenDocumentsTracker textDocumentFactoryService )
            } |> ignore
