module FSharpXmlDoc.Program

open System
open System.Diagnostics
open System.ComponentModel.Composition
open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text 

// Useful:  http://msdn.microsoft.com/en-us/library/dd885243.aspx
[<Export(typeof<IVsTextViewCreationListener>)>]
[<ContentType("FSharp")>]
[<TextViewRole(PredefinedTextViewRoles.Interactive)>]
type VsTextViewCreationListener() =
    let mutable editorFactory : IVsEditorAdaptersFactoryService = null;
    let mutable textDocumentFactoryService : ITextDocumentFactoryService = null

    [<Import(typeof<IVsEditorAdaptersFactoryService>)>]    
    member this.TextDocumentFactoryService with get() = textDocumentFactoryService and set(x) = textDocumentFactoryService <- x    

    [<Import(typeof<IVsEditorAdaptersFactoryService>)>]
    member private this.EditorFactory with get() = editorFactory and set(x) = editorFactory <- x

    interface IVsTextViewCreationListener with
        member this.VsTextViewCreated(textView:IVsTextView) =
            let wpfTextView = editorFactory.GetWpfTextView(textView)
            match this.TextDocumentFactoryService.TryGetTextDocument(wpfTextView.TextBuffer) with
            | false, _ -> ()
            | true, doc ->
                new FSharpXmlDoc.XmlDocFilter(textView, wpfTextView, doc.FilePath) |> ignore
