namespace FSharpVSPowerTools.Refactoring

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open ExtCore.Control
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open System.Windows

module PkgCmdIDList =
    let CmdidMyCommand = 0x2001u
    let CmdidMyMenu = 0x1100u

type RenameCommandFilter(view : IWpfTextView, undoHistory : ITextUndoHistory) =
    let mutable currentWord = None
    let mutable currentFile = None
    let mutable currentProject = None

    let canRename (word : SnapshotSpan option) = 
        // TODO : it should be a symbol and is defined in current project
        word.IsSome

    let updateAtCaretPosition(caretPosition : CaretPosition) = 
        maybe {
            let! point = view.TextBuffer.GetSnapshotPoint caretPosition
            let doc = Dte.getActiveDocument()
            currentFile <- Some doc.FullName
            let! project = doc.Project
            currentProject <- Some project
            currentWord <- VSLanguageService.getSymbol point project } |> ignore

    let analyze _ = 
        // TODO: it seems too early to reparse everything when text changed
        ()

    let viewLayoutChanged = 
        EventHandler<_>(fun _ (e : TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(view.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e : CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    let textChanged =
        EventHandler<_>(fun _ (e : TextContentChangedEventArgs) ->
            analyze(e.After.GetText()))

    do
      view.LayoutChanged.AddHandler(viewLayoutChanged)
      view.Caret.PositionChanged.AddHandler(caretPositionChanged)
      view.TextBuffer.Changed.AddHandler(textChanged)

    let guidMenuAndCommandsCmdSet = "{5debbcf2-6cb1-480c-9e69-edcb2196bad7}"

    let rename (newText : string) (references: SnapshotSpan list) =
        let description = String.Format("Rename -> '{0}'", newText)
        use transaction = undoHistory.CreateTransaction(description)
        references
        |> List.fold (fun snapshot span ->
            let span = span.TranslateTo(snapshot, SpanTrackingMode.EdgeExclusive)
            view.TextBuffer.Replace(span.Span, newText)) view.TextSnapshot
        |> ignore
        transaction.Complete()
        Dte.getActiveDocument().Save() |> ignore

    member this.HandleRename() =
        maybe {
            let! currentFile = currentFile
            let! currentProject = currentProject
            let! cw = currentWord
            let! references = 
                  VSLanguageService.findUsages cw currentFile currentProject view.TextSnapshot 
                  |> Async.RunSynchronously
            let wnd = UI.loadRenameDialog(this)
            let hostWnd = Window.GetWindow(view.VisualElement)
            wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
            wnd.Owner <- hostWnd
            this.Name <- match currentWord with Some w -> w.GetText() | _ -> ""
            let! res = wnd.ShowDialog() |> Option.ofNullable
            if res then rename this.Name references } |> ignore

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
            view.LayoutChanged.RemoveHandler(viewLayoutChanged)
            view.Caret.PositionChanged.RemoveHandler(caretPositionChanged)
            view.TextBuffer.Changed.RemoveHandler(textChanged)