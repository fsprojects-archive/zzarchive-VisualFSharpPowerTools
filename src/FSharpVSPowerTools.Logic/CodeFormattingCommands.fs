namespace FSharpVSPowerTools.CodeFormatting

open System
open System.IO
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open Microsoft.VisualStudio.Shell.Interop
open Fantomas
open Fantomas.FormatConfig
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.AsyncMaybe
open Microsoft.FSharp.Compiler.SourceCodeServices

[<NoComparison>]
type FormattingResult = 
    {
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
        async {
            let editorOperations = x.Services.EditorOperationsFactoryService.GetEditorOperations(x.TextView)
            use textUndoTransaction = x.TryCreateTextUndoTransaction()
            // Handle the special case of a null ITextUndoTransaction up here because it simplifies
            // the rest of the method.  The implementation of operations such as 
            // AddBeforeTextBufferUndoChangePrimitive will directly access the ITextUndoHistory of 
            // the ITextBuffer.  If there is no history then this operation will throw a NullReferenceException
            // instead of failing gracefully.  If we have an ITextUndoTransaction then we know that the 
            // ITextUndoHistory exists and can call all of the methods as appropriate. 
            if isNull textUndoTransaction then
                let! _ = x.ExecuteFormatCore()
                ()
            else
                // This command will capture the caret position as it currently exists inside the undo 
                // transaction.  That way the undo command will reset the caret back to this position.  
                editorOperations.AddBeforeTextBufferChangePrimitive()

                let! success = x.ExecuteFormatCore()
                if success then
                    // Capture the caret as it exists now.  This way any redo of this edit will 
                    // reposition the caret as it exists now. 
                    editorOperations.AddAfterTextBufferChangePrimitive()
                    textUndoTransaction.Complete()
                else
                    textUndoTransaction.Cancel()
            }
        |> Async.StartInThreadPoolSafe

    member x.ExecuteFormatCore() =
        asyncMaybe {
            let source = x.TextBuffer.CurrentSnapshot.GetText()
            
            let config = getConfig()
            let statusBar = x.Services.ServiceProvider.GetService<IVsStatusbar, SVsStatusbar>()

            try
                let! filePath = 
                    match x.Services.TextDocumentFactoryService.TryGetTextDocument(x.TextBuffer) with
                    | true, textDocument ->
                        Some textDocument.FilePath
                    | _ -> None
                let dte = x.Services.ServiceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! document = dte.GetCurrentDocument filePath
                let! project = x.Services.ProjectFactory.CreateForDocument x.TextBuffer document
                let! projectOptions = x.Services.LanguageService.GetProjectCheckerOptions project |> liftAsync
                let checker = x.Services.LanguageService.RawChecker
                let! formattingResult = x.GetFormatted(filePath, source, config, projectOptions, checker) |> liftAsync
                let! isValid = 
                    CodeFormatter.IsValidFSharpCodeAsync(filePath, formattingResult.NewText, projectOptions, checker)
                    |> liftAsync
                if isValid then
                    use edit = x.TextBuffer.CreateEdit()
                    let (caretPos, scrollBarPos, currentSnapshot) = x.TakeCurrentSnapshot()

                    edit.Replace(formattingResult.OldTextStartIndex,
                                 formattingResult.OldTextLength,
                                 formattingResult.NewText) |> ignore
                    edit.Apply() |> ignore

                    x.SetNewCaretPosition(caretPos, scrollBarPos, currentSnapshot)
                    return! Some()
                else
                    statusBar.SetText(Resource.formattingValidationMessage) |> ignore 
                    return! None
            with
            | :? FormatException as ex ->
                statusBar.SetText(ex.Message) |> ignore 
                return! None
            | ex ->
                statusBar.SetText(sprintf "%s: %O" Resource.formattingErrorMessage ex) |> ignore
                return! None
        }
        |> Async.map Option.isSome
       
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

    abstract GetFormatted: filePath:string * source:string * config:FormatConfig * projectOptions:FSharpProjectOptions * checker:FSharpChecker -> Async<FormattingResult>
    abstract SetNewCaretPosition: caretPos:SnapshotPoint * scrollBarPos:int * originalSnapshot:ITextSnapshot -> unit

