namespace FSharpVSPowerTools.Refactoring

open System.IO
open System.Windows
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharp.ViewModule.Progress
open FSharpVSPowerTools.AsyncMaybe

[<NoEquality; NoComparison>]
type private DocumentState =
    { Word: (SnapshotSpan * Symbol) option
      File: string
      Project: IProjectProvider }

type RenameCommandFilter(view: IWpfTextView, vsLanguageService: VSLanguageService, serviceProvider: System.IServiceProvider,
                         projectFactory: ProjectFactory) =
    let mutable state = None
    let documentUpdater = DocumentUpdater(serviceProvider)

    let canRename() = 
        state <-
            maybe {
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = projectFactory.CreateForDocument view.TextBuffer doc
                return { Word = vsLanguageService.GetSymbol(caretPos, project); File = doc.FullName; Project = project }
            }
        state |> Option.bind (fun s -> s.Word) |> Option.isSome

    let rename (oldText: string) (symbolKind: SymbolKind) (newText: string) (foundUsages: (string * range list) list) =
        try
            let newText = IdentifierUtils.encapsulateIdentifier symbolKind newText
            let undo = documentUpdater.BeginGlobalUndo "Rename Refactoring"
            try
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                dte.GetActiveDocument()
                |> Option.iter (fun doc ->
                    for (fileName, ranges) in foundUsages do
                        let buffer = documentUpdater.GetBufferForDocument(fileName)
                        let spans =
                            match state with
                            | Some { Word = Some (word, _); File = currentFile } when currentFile = fileName ->
                                seq {
                                    let spans = List.choose (fromFSharpRange buffer.CurrentSnapshot) ranges
                                    if List.forall ((<>) word) spans then
                                        // Ensure that current word is always renamed
                                        yield word
                                    yield! spans
                                }
                            | _ -> Seq.choose (fromFSharpRange buffer.CurrentSnapshot) ranges
                            |> fixInvalidSymbolSpans buffer.CurrentSnapshot oldText
                        spans
                        |> List.fold (fun (snapshot: ITextSnapshot) span ->
                            let span = span.TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive)
                            snapshot.TextBuffer.Replace(span.Span, newText)) buffer.CurrentSnapshot
                        |> ignore

                    // Refocus to the current document
                    doc.Activate())
            finally
                documentUpdater.EndGlobalUndo(undo)
        with e -> Logging.logException e

    member x.HandleRename() = maybe {
        let! state = state
        let! cw, symbol = state.Word
        // cancellation token source used to cancel all async operations throughout the rename process
        use cts = new System.Threading.CancellationTokenSource()
        let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
        // This is the workflow used to initialize the rename operation.  It should return the appropriate scope and symbols on success, and cancel on failure
        let initialContext = 
            asyncMaybe {
                let! fsSymbolUse, fileScopedCheckResults = 
                    // We pass AllowStaleResults.No here because we really need a 100% accurate symbol w.r.t. all prior files,
                    // in order to by able to make accurate symbol comparisons during renaming.
                    vsLanguageService.GetFSharpSymbolUse(cw, symbol, state.File, state.Project, AllowStaleResults.No)
                
                let symbolDeclarationLocation = 
                    projectFactory.GetSymbolDeclarationLocation fsSymbolUse.Symbol state.File state.Project

                return! liftMaybe <|
                    match symbolDeclarationLocation with
                    | Some location ->
                        Some { Symbol = symbol
                               FSharpSymbol = fsSymbolUse.Symbol
                               SymbolDeclarationLocation = location
                               ParseAndCheckResults = fileScopedCheckResults }
                    | _ -> 
                        cts.Cancel()
                        Logging.messageBoxError Resource.renameErrorMessage
                        None
            } 
         
        // This is the actual async workflow used to rename.  It should report progress as possible
        let rename context name (ShowProgress showProgress) = 
            let report msg = reportProgress (Some showProgress) (Reporting msg)
            asyncMaybe {
                let! _, lastIdent, symbolUses =
                    match context.SymbolDeclarationLocation with
                    | SymbolDeclarationLocation.File -> 
                        report "Renaming symbols in file..."
                        vsLanguageService.FindUsagesInFile (cw, symbol, context.ParseAndCheckResults)
                    | SymbolDeclarationLocation.Projects declarationProjects -> 
                        report "Performing Rename in projects..."
                        let dependentProjects = projectFactory.GetDependentProjects dte declarationProjects
                        report (sprintf "Performing Rename in %d projects..." dependentProjects.Length)
                        vsLanguageService.FindUsages (cw, state.File, state.Project, dependentProjects, showProgress)
                
                let usages =
                    symbolUses 
                    |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse.RangeAlternate))
                    |> Seq.groupBy (fst >> Path.GetFullPathSafe)
                    |> Seq.map (fun (fileName, symbolUses) -> fileName, Seq.map snd symbolUses |> Seq.toList)
                    |> Seq.toList

                report "Performing Rename ..."
                rename lastIdent symbol.Kind name usages
            } |> Async.map ignore

        Window.GetWindow view.VisualElement
        |> UI.loadRenameDialog (RenameDialogViewModel (cw.GetText(), initialContext, rename, cts))
        |> x.ShowDialog 
        |> ignore 
    }

    member __.ShowDialog (wnd: Window) =
        let vsShell = serviceProvider.GetService<IVsUIShell, SVsUIShell>()
        try
            if ErrorHandler.Failed(vsShell.EnableModeless(0)) then
                Some false
            else wnd.ShowDialog() |> Option.ofNullable
        finally
            vsShell.EnableModeless(1) |> ignore

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            if (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidStandardRenameCommand) && canRename() then
                x.HandleRename() |> ignore
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = Constants.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidStandardRenameCommand) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            