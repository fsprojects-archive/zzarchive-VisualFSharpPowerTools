namespace FSharpVSPowerTools.ProjectSystem

open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Interactive)>]
type ActiveViewRegistratorListener() = 
    [<Import; DefaultValue>]
    val mutable openDocumentsTracker: IOpenDocumentsTracker
    interface IWpfTextViewCreationListener with
        member x.TextViewCreated(view) = x.openDocumentsTracker.RegisterView(view)