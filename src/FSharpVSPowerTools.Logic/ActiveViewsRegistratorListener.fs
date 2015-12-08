namespace FSharpVSPowerTools.ProjectSystem

open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.PrimaryDocument)>]
type ActiveViewRegistratorListener [<ImportingConstructor>]([<Import>] openDocumentsTracker: IOpenDocumentsTracker) = 
    interface IWpfTextViewCreationListener with
        member __.TextViewCreated view = openDocumentsTracker.RegisterView view