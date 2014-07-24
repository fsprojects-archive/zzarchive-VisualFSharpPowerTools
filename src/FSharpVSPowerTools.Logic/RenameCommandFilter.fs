namespace FSharpVSPowerTools.Refactoring

open System
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

[<NoEquality; NoComparison>]
type DocumentState =
    { Word: (SnapshotSpan * Symbol) option
      File: string
      Project: IProjectProvider }

type RenameCommandFilter(view: IWpfTextView, vsLanguageService: VSLanguageService, serviceProvider: System.IServiceProvider,
                         projectFactory: ProjectFactory) =
    let mutable state = None
    let documentUpdater = DocumentUpdater(serviceProvider)

    let canRename() = 
        let s =
            maybe {
                let! caretPos = view.TextBuffer.GetSnapshotPoint view.Caret.Position
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = projectFactory.CreateForDocument view.TextBuffer doc
                return { Word = vsLanguageService.GetSymbol(caretPos, project); File = doc.FullName; Project = project }
            }
        state <- s
        state |> Option.bind (fun s -> s.Word) |> Option.isSome

    let rename (oldText: string) (symbolKind:SymbolKind) (newText: string) (foundUsages: (string * range list) list) =
        try
            let newText = IdentifierUtils.encapsulateIdentifier symbolKind newText
            let undo = documentUpdater.BeginGlobalUndo("Rename Refactoring")
            try
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                dte.GetActiveDocument()
                |> Option.iter (fun doc ->
                    for (fileName, ranges) in foundUsages do
                        let buffer = documentUpdater.GetBufferForDocument(fileName)
                        let spans =
                            ranges
                            |> Seq.choose (fun range -> maybe {
                                let! snapshotSpan = fromFSharpRange buffer.CurrentSnapshot range
                                let i = snapshotSpan.GetText().LastIndexOf(oldText)
                                return
                                    if i > 0 then 
                                        // Subtract lengths of qualified identifiers
                                        SnapshotSpan(buffer.CurrentSnapshot, snapshotSpan.Start.Position + i, snapshotSpan.Length - i) 
                                    else snapshotSpan })
                            |> Seq.toList

                        spans
                        |> List.fold (fun (snapshot: ITextSnapshot) span ->
                            let span = span.TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive)
                            snapshot.TextBuffer.Replace(span.Span, newText)) buffer.CurrentSnapshot
                        |> ignore

                    // Refocus to the current document
                    doc.Activate())
            finally
                documentUpdater.EndGlobalUndo(undo)
        with e ->
            logException e

    member x.HandleRename() =
        let word = state |> Option.bind (fun s -> s.Word |> Option.map (fun (cw, sym) -> (s, cw, sym)))
        
        match word with
        | Some (state, cw, symbol) ->
            // cancellation token source used to cancel all async operations throughout the rename process
            use cts = new System.Threading.CancellationTokenSource()
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()

            // This is the workflow used to initialize the rename operation.  It should return the appropriate scope and symbols on success, and cancel on failure
            let initializationWorkflow = 
                async {
                    let! results = 
                        // We pass AllowStaleResults.No here because we really need a 100% accurate symbol w.r.t. all prior files,
                        // in order to by able to make accurate symbol comparisons during renaming.
                        vsLanguageService.GetFSharpSymbolUse(cw, symbol, state.File, state.Project, AllowStaleResults.No)
                    match results with
                    | None ->
                        return None
                    | Some(fsSymbolUse, fileScopedCheckResults) ->
                    
                        let symbolDeclarationLocation = 
                            projectFactory.GetSymbolDeclarationLocation fsSymbolUse.Symbol state.File state.Project

                        match symbolDeclarationLocation with
                        | Some scope ->
                            return Some(fileScopedCheckResults, scope, fsSymbolUse.Symbol) 
                        | _ -> 
                            cts.Cancel()
                            messageBoxError Resource.renameErrorMessage
                            return None
                } 
            
            // This is the actual async workflow used to rename.  It should report progress as possible
            let renameWorkflow parseAndCheckResults symbolLocation name (prg : OperationState -> unit) = 
                let progress = Some(prg)
                async {
                    let! results =
                        match symbolLocation with
                        | SymbolDeclarationLocation.File -> 
                            reportProgress progress (Reporting("Renaming symbols in file..."))
                            vsLanguageService.FindUsagesInFile (cw, symbol, parseAndCheckResults)
                        | SymbolDeclarationLocation.Projects declarationProjects -> 
                            reportProgress progress (Reporting("Performing Rename in projects..."))
                            let dependentProjects = projectFactory.GetDependentProjects dte declarationProjects
                            reportProgress progress (Reporting(sprintf "Performing Rename in %d projects..." dependentProjects.Length))
                            vsLanguageService.FindUsages (cw, state.File, state.Project, dependentProjects, prg)
                    let usages =
                        results
                        |> Option.map (fun (symbol, lastIdent, refs) -> 
                            symbol, lastIdent,
                                refs 
                                |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse.RangeAlternate))
                                |> Seq.groupBy (fst >> Path.GetFullPath)
                                |> Seq.map (fun (fileName, symbolUses) -> fileName, Seq.map snd symbolUses |> Seq.toList)
                                |> Seq.toList)
                    match usages with
                    | Some (_, currentName, references) ->
                        reportProgress progress (Reporting("Performing Rename ..."))
                        return rename currentName symbol.Kind name references
                    | None -> 
                        return ()
                }

            let hostWnd = Window.GetWindow(view.VisualElement)
            let viewmodel = RenameDialogViewModel (cw.GetText(), symbol, initializationWorkflow, renameWorkflow, cts)
            let wnd = UI.loadRenameDialog viewmodel hostWnd                        
            x.ShowDialog wnd |> ignore
        | _ -> ()

    member x.ShowDialog (wnd: Window) =
        let vsShell = serviceProvider.GetService<IVsUIShell, SVsUIShell>()
        try
            if ErrorHandler.Failed(vsShell.EnableModeless(0)) then
                Some false
            else
                wnd.ShowDialog() |> Option.ofNullable
        finally
            vsShell.EnableModeless(1) |> ignore

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec(pguidCmdGroup: byref<Guid>, nCmdId: uint32, nCmdexecopt: uint32, pvaIn: IntPtr, pvaOut: IntPtr) =
            if (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidStandardRenameCommand) && canRename() then
                x.HandleRename()
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = Constants.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidStandardRenameCommand) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            