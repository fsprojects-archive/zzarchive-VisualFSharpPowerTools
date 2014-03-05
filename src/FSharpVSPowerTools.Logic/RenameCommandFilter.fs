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
open FSharp.CompilerBinding

module PkgCmdIDList =
    let CmdidBuiltinRenameCommand = 1550u // ECMD_RENAME
    let GuidBuiltinCmdSet = typedefof<VSConstants.VSStd2KCmdID>.GUID

[<NoComparison>]
type DocumentState =
    { Word: (SnapshotSpan * Symbol) option
      File: string
      Project: IProjectProvider }

type RenameCommandFilter(view: IWpfTextView, serviceProvider: System.IServiceProvider) =
    let mutable state = None
    let documentUpdater = DocumentUpdater(serviceProvider)

    let canRename() = 
        // TODO: it should be a symbol and is defined in current project
        state |> Option.bind (fun x -> x.Word) |> Option.isSome

    let updateAtCaretPosition(caretPosition: CaretPosition) = 
        maybe {
            let! point = view.TextBuffer.GetSnapshotPoint caretPosition
            let! doc = Dte.getActiveDocument()
            let! project = ProjectCache.getProject doc
            state <- Some
                { File = doc.FullName
                  Project =  project
                  Word = VSLanguageService.getSymbol point project }} |> ignore

    let viewLayoutChanged = 
        EventHandler<_>(fun _ (e: TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(view.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e: CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    do
        view.LayoutChanged.AddHandler(viewLayoutChanged)
        view.Caret.PositionChanged.AddHandler(caretPositionChanged)

    let rename (oldText: string) (newText: string) (foundUsages: (string * range list) list) =
        try
            let undo = documentUpdater.BeginGlobalUndo("Rename Refactoring")
            try
                Dte.getActiveDocument()
                |> Option.iter (fun doc ->
                    for (fileName, ranges) in foundUsages do
                        let buffer = documentUpdater.GetBufferForDocument(fileName)
                        let spans =
                            ranges
                            |> Seq.map (fun range ->
                                let snapshotSpan = fromFSharpPos buffer.CurrentSnapshot range
                                let i = snapshotSpan.GetText().LastIndexOf(oldText)
                                if i > 0 then 
                                    // Subtract lengths of qualified identifiers
                                    SnapshotSpan(buffer.CurrentSnapshot, snapshotSpan.Start.Position + i, snapshotSpan.Length - i) 
                                else snapshotSpan)
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
            debug "[Rename Refactoring] Error %O occurs while renaming symbols." e

    member x.HandleRename() =
        maybe {
            let! state = state
            let! cw, sym = state.Word
            let! (symbol, currentName, references) = 
                  VSLanguageService.findUsages cw state.File state.Project
                  |> Async.RunSynchronously
                  |> Option.map (fun (symbol, lastIdent, _, refs) -> 
                        symbol, lastIdent,
                            refs 
                            |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse.RangeAlternate))
                            |> Seq.groupBy (fst >> Path.GetFullPath)
                            |> Seq.map (fun (fileName, symbolUses) -> fileName, Seq.map snd symbolUses |> Seq.toList)
                            |> Seq.toList)

            let isSymbolDeclaredInCurrentProject =
                match VSLanguageService.tryGetLocation symbol with
                | Some loc ->
                    let filePath = Path.GetFullPath loc.FileName
                    // NB: this isn't a foolproof way to match two paths
                    state.Project.SourceFiles |> Array.exists ((=) filePath)
                | _ -> false

            if isSymbolDeclaredInCurrentProject then
                let model = RenameDialogModel (cw.GetText(), sym, symbol)
                let wnd = UI.loadRenameDialog model
                let hostWnd = Window.GetWindow(view.VisualElement)
                wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
                wnd.Owner <- hostWnd
                let! res = x.ShowDialog wnd
                if res then rename currentName model.Name references
            else
                MessageBox.Show(Resource.renameErrorMessage, Resource.vsPackageTitle, 
                    MessageBoxButton.OK, MessageBoxImage.Error) |> ignore 
        } |> ignore

    member x.ShowDialog (wnd: Window) =
        let vsShell = serviceProvider.GetService(typeof<SVsUIShell>) :?> IVsUIShell
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
            if (pguidCmdGroup = PkgCmdIDList.GuidBuiltinCmdSet && nCmdId = PkgCmdIDList.CmdidBuiltinRenameCommand) && canRename() then
                x.HandleRename()
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus(pguidCmdGroup: byref<Guid>, cCmds: uint32, prgCmds: OLECMD[], pCmdText: IntPtr) =
            if pguidCmdGroup = PkgCmdIDList.GuidBuiltinCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdIDList.CmdidBuiltinRenameCommand) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            

    interface IDisposable with
        member x.Dispose() = 
            view.LayoutChanged.RemoveHandler(viewLayoutChanged)
            view.Caret.PositionChanged.RemoveHandler(caretPositionChanged)