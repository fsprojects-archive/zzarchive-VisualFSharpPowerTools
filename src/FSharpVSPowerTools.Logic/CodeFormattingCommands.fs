﻿namespace FSharpVSPowerTools.CodeFormatting

open System
open System.IO
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open Microsoft.VisualStudio.Shell.Interop
open Fantomas.FormatConfig
open Fantomas.CodeFormatter
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

[<NoComparison>]
type FormattingResult = {
    OldTextStartIndex: int
    OldTextLength: int
    NewText: string
}

[<AbstractClass>]
type CommandBase() =
    member val TextView: IWpfTextView = null with get, set
    member val Services: CodeFormattingServices = null with get, set
    member x.TextBuffer: ITextBuffer = x.TextView.TextBuffer
    abstract Execute: unit -> unit

[<AbstractClass>]
type FormatCommand(getConfig: Func<FormatConfig>) =
    inherit CommandBase()

    // Rebind to this method call as it's more F#-friendly
    let getConfig() = getConfig.Invoke()

    member x.TryCreateTextUndoTransaction() =
        let textBufferUndoManager = x.Services.TextBufferUndoManagerProvider.GetTextBufferUndoManager(x.TextBuffer)

        // It is possible for an ITextBuffer to have a null ITextUndoManager.  This will happen in 
        // cases like the differencing viewer.  If VS doesn't consider the document to be editable then 
        // it won't create an undo history for it.  Need to be tolerant of this behavior. 
        match textBufferUndoManager with
        | null -> null
        | _ -> textBufferUndoManager.TextBufferUndoHistory.CreateTransaction("Format Code")

    member x.ExecuteFormat() =
        let editorOperations = x.Services.EditorOperationsFactoryService.GetEditorOperations(x.TextView)
        use textUndoTransaction = x.TryCreateTextUndoTransaction()
        // Handle the special case of a null ITextUndoTransaction up here because it simplifies
        // the rest of the method.  The implementation of operations such as 
        // AddBeforeTextBufferUndoChangePrimitive will directly access the ITextUndoHistory of 
        // the ITextBuffer.  If there is no history then this operation will throw a NullReferenceException
        // instead of failing gracefully.  If we have an ITextUndoTransaction then we know that the 
        // ITextUndoHistory exists and can call all of the methods as appropriate. 
        if textUndoTransaction = null then
            x.ExecuteFormatCore() |> ignore
        else
            // This command will capture the caret position as it currently exists inside the undo 
            // transaction.  That way the undo command will reset the caret back to this position.  
            editorOperations.AddBeforeTextBufferChangePrimitive()

            if x.ExecuteFormatCore() then
                // Capture the caret as it exists now.  This way any redo of this edit will 
                // reposition the caret as it exists now. 
                editorOperations.AddAfterTextBufferChangePrimitive()
                textUndoTransaction.Complete()
            else
                textUndoTransaction.Cancel()
    
    member x.ExecuteFormatCore() =
        let source = x.TextBuffer.CurrentSnapshot.GetText()
        let isSignatureFile = x.IsSignatureFile(x.TextBuffer)

        let config = getConfig()
        let statusBar = x.Services.ServiceProvider.GetService<IVsStatusbar, SVsStatusbar>()

        try
            let formattingResult = x.GetFormatted(isSignatureFile, source, config)

            if isValidFSharpCode isSignatureFile (formattingResult.NewText) then
                use edit = x.TextBuffer.CreateEdit()
                let (caretPos, scrollBarPos, currentSnapshot) = x.TakeCurrentSnapshot()

                edit.Replace(formattingResult.OldTextStartIndex,
                             formattingResult.OldTextLength,
                             formattingResult.NewText) |> ignore
                edit.Apply() |> ignore

                x.SetNewCaretPosition(caretPos, scrollBarPos, currentSnapshot)
                true
            else
                statusBar.SetText(Resource.formattingValidationMessage) |> ignore 
                false
        with
        | :? FormatException as ex ->
            statusBar.SetText(ex.Message) |> ignore 
            false
        | ex ->
            statusBar.SetText(sprintf "%s: %O" Resource.formattingErrorMessage ex) |> ignore
            false

    member x.IsSignatureFile(buffer: ITextBuffer) =
        match x.Services.TextDocumentFactoryService.TryGetTextDocument(buffer) with
        | true, textDocument ->
            let fileExtension = Path.GetExtension(textDocument.FilePath)
            // There isn't a distinct content type for FSI files, so we have to use the file extension
            let isSignatureFile = ".fsi".Equals(fileExtension, StringComparison.OrdinalIgnoreCase)
            isSignatureFile
        | false, _ ->
            // If this isn't backed by an actual document then it can't be considered 
            // a signature file
            false

    member x.TakeCurrentSnapshot() =
        let caretPos = x.TextView.Caret.Position.BufferPosition
        let originalSnapshot = x.TextBuffer.CurrentSnapshot
        // Get start line of scroll bar
        let scrollBarLine = x.TextView.TextViewLines |> Seq.tryFind (fun l -> l.VisibilityState <> VisibilityState.Hidden)
        let scrollBarPos =
            match scrollBarLine with
            | None -> 0
            | Some scrollBarLine -> originalSnapshot.GetLineNumberFromPosition(int scrollBarLine.Start)
        (caretPos, scrollBarPos, originalSnapshot)

    abstract GetFormatted: isSignatureFile: bool * source: string * config: FormatConfig -> FormattingResult
    abstract SetNewCaretPosition: caretPos: SnapshotPoint * scrollBarPos: int * originalSnapshot: ITextSnapshot -> unit

