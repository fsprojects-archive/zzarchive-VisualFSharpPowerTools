namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Collections.Generic
open System.ComponentModel.Composition

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Interactive)>]
type ActiveViewRegistratorListener() = 
    [<Import; DefaultValue>]
    val mutable openDocumentsTracker: OpenDocumentsTracker
    interface IWpfTextViewCreationListener with
        member this.TextViewCreated(view) = this.openDocumentsTracker.RegisterView(view)