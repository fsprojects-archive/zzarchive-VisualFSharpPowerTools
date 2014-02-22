namespace FSharpVSPowerTools.Refactoring

open System
open System.Diagnostics
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Windows.Media
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open EnvDTE
open VSLangProj
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Windows
open FSharpVSPowerTools.Core

type PkgCmdIDList =
    static member CmdidMyCommand = 0x2001u
    static member CmdidMyMenu = 0x1100u

type RenameCommandFilter(tv : IWpfTextView, uh : ITextUndoHistory) =
    let textView = tv
    let undoHistory = uh

    let mutable currentWord = None
    let mutable currentFile = Unchecked.defaultof<_>
    let mutable currentProject = Unchecked.defaultof<_>

    let canRename (word : SnapshotSpan option) = 
        // TODO : it should be a symbol and is defined in current project
        word.IsSome

    let updateAtCaretPosition(caretPosition : CaretPosition) = 
        match textView.GetSnapshotPoint caretPosition with
        | Some point ->
            let doc = Dte.getActiveDocument()
            currentFile <- doc.FullName

            match doc.Project with
            | Some project ->
                let ident =
                    let projectProvider = ProjectProvider(project)
                    let source = point.Snapshot.GetText()
                    let line = point.Snapshot.GetLineNumberFromPosition(point.Position)
                    let col = point.Position - point.GetContainingLine().Start.Position
                    let lineStr = point.GetContainingLine().GetText()                
                    let args = projectProvider.CompilerOptions        
                    currentProject <- projectProvider        
                    VSLanguageService.Instance.GetSymbol (source, line, col, lineStr, args)
                    |> Option.map (fun symbol -> point.FromRange symbol.Range)
                currentWord <- ident
            | _ -> debug "[Rename Refactoring] Can't find containing project. Probably the document is opened in an ad-hoc way."
        | _ -> ()

    let analyze _ = 
        // TODO: it seems too early to reparse everything when text changed
        ()

    let viewLayoutChanged = 
        EventHandler<_>(fun _ (e : TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(textView.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e : CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    let textChanged =
        EventHandler<_>(fun _ (e : TextContentChangedEventArgs) ->
            analyze(e.After.GetText()))

    do
      textView.LayoutChanged.AddHandler(viewLayoutChanged)
      textView.Caret.PositionChanged.AddHandler(caretPositionChanged)
      textView.TextBuffer.Changed.AddHandler(textChanged)

    let guidMenuAndCommandsCmdSet = "{5debbcf2-6cb1-480c-9e69-edcb2196bad7}"

    let rename (lastIdent : string) (newText : string) (references: SnapshotSpan list) =
        let description = String.Format("Rename -> '{0}'", newText)
        use transaction = undoHistory.CreateTransaction(description)
        references
        |> List.fold (fun snapshot span ->
            let span = span.TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive)
            textView.TextBuffer.Replace(span.Span, newText)) textView.TextSnapshot
        |> ignore
        transaction.Complete()

        // Try to save current document
        let dte = Package.GetGlobalService(typedefof<DTE>) :?> DTE
        let doc = dte.ActiveDocument
        Debug.Assert(doc <> null, "Should be able to find active document.")
        doc.Save() |> ignore

    member this.HandleRename() =
        match currentWord with
        | None -> ()
        | Some w ->
            match VSLanguageService.findUsages w currentFile currentProject textView.TextSnapshot |> Async.RunSynchronously with
            | None -> ()
            | Some (lastIdent, references) ->
                let wnd = UI.loadRenameDialog(this)
                let hostWnd = Window.GetWindow(textView.VisualElement)
                wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
                wnd.Owner <- hostWnd
                this.Name <- ""
                let res = wnd.ShowDialog()
                if res.HasValue && res.Value then
                    rename lastIdent this.Name references

    member val IsAdded = false with get, set
    member val NextTarget : IOleCommandTarget = null with get, set
    member val Name = "" with get, set

    interface IOleCommandTarget with
        member this.Exec(pguidCmdGroup : byref<Guid>, nCmdId : uint32, nCmdexecopt : uint32, pvaIn : IntPtr, pvaOut : IntPtr) =
            if (pguidCmdGroup = Guid.Parse(guidMenuAndCommandsCmdSet) && nCmdId = PkgCmdIDList.CmdidMyCommand) then
                this.HandleRename()
            this.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member this.QueryStatus(pguidCmdGroup:byref<Guid>, cCmds:uint32, prgCmds:OLECMD[], pCmdText:IntPtr) =
            if pguidCmdGroup = Guid.Parse(guidMenuAndCommandsCmdSet) && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdIDList.CmdidMyMenu) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            elif pguidCmdGroup = Guid.Parse(guidMenuAndCommandsCmdSet) && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdIDList.CmdidMyCommand) &&
                canRename currentWord then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                this.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            

    interface IDisposable with
        member __.Dispose() = 
            textView.LayoutChanged.RemoveHandler(viewLayoutChanged)
            textView.Caret.PositionChanged.RemoveHandler(caretPositionChanged)
            textView.TextBuffer.Changed.RemoveHandler(textChanged)


