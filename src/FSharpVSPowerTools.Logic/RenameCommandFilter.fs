namespace FSharpVSPowerTools.Refactoring

open System
open System.IO
open System.Windows
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

module PkgCmdConst =
    let cmdidStandardRenameCommand = uint32 VSConstants.VSStd2KCmdID.RENAME // ECMD_RENAME
    let guidStandardCmdSet = typedefof<VSConstants.VSStd2KCmdID>.GUID

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

    let rename (oldText: string) (newText: string) (foundUsages: (string * range list) list) =
        try
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
        
        let cts = new System.Threading.CancellationTokenSource()
        let hostWnd = Window.GetWindow(view.VisualElement)
        
        match word with
        | Some (state, cw, symbol) ->
            let wf = async {
                let! results = 
                    // We pass AllowStaleResults.No here because we really need a 100% accurate symbol w.r.t. all prior files,
                    // in order to by able to make accurate symbol comparisons during renaming.
                    vsLanguageService.GetFSharpSymbolUse(cw, symbol, state.File, state.Project, AllowStaleResults.No)
                match results with
                | None ->
                    return ()
                | Some(fsSymbolUse, fileScopedCheckResults) ->
                    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                    let symbolDeclarationLocation = projectFactory.GetSymbolUsageScope state.Project.IsForStandaloneScript fsSymbolUse.Symbol dte state.File

                    match symbolDeclarationLocation with
                    | Some scope ->
                        let renameWorkflow _ name = 
                            async {
                                let! results =
                                    match scope with
                                    | SymbolDeclarationLocation.File -> vsLanguageService.FindUsagesInFile (cw, symbol, fileScopedCheckResults)
                                    | SymbolDeclarationLocation.Projects declarationProjects -> 
                                        let dependentProjects = projectFactory.GetDependentProjects dte declarationProjects
                                        vsLanguageService.FindUsages (cw, state.File, state.Project, dependentProjects)
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
                                    return rename currentName name references
                                | None -> 
                                    return ()
                            }

                        let model = RenameDialogModel (cw.GetText(), symbol, fsSymbolUse.Symbol, cts, renameWorkflow)
                        let wnd = UI.loadRenameDialog model hostWnd                        
                            
                        x.ShowDialog wnd |> ignore                                        
                    | _ -> return messageBoxError Resource.renameErrorMessage
            } 
            Async.StartImmediateSafe(wf, cts.Token)
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
            if (pguidCmdGroup = PkgCmdConst.guidStandardCmdSet && nCmdId = PkgCmdConst.cmdidStandardRenameCommand) && canRename() then
                x.HandleRename()
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = PkgCmdConst.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdConst.cmdidStandardRenameCommand) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            