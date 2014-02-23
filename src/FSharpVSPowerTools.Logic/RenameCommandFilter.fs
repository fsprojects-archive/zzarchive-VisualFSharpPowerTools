namespace FSharpVSPowerTools.Refactoring

open System
open System.IO
open System.Windows
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

module PkgCmdIDList =
    let CmdidRenameCommand = 0x2001u
    let CmdidRefactorMenu = 0x1100u

type State =
    { Word: SnapshotSpan option
      File: string
      Project: ProjectProvider }

type RenameCommandFilter(view : IWpfTextView, undoHistory : ITextUndoHistory) =
    let mutable state = None

    let canRename() = 
        // TODO : it should be a symbol and is defined in current project
        state |> Option.bind (fun x -> x.Word) |> Option.isSome

    let updateAtCaretPosition(caretPosition : CaretPosition) = 
        maybe {
            let! point = view.TextBuffer.GetSnapshotPoint caretPosition
            let doc = Dte.getActiveDocument()
            let! project = doc.Project
            state <- Some
                { File = doc.FullName
                  Project =  project
                  Word = VSLanguageService.getSymbol point project }} |> ignore

    let viewLayoutChanged = 
        EventHandler<_>(fun _ (e : TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(view.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e : CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    do
      view.LayoutChanged.AddHandler(viewLayoutChanged)
      view.Caret.PositionChanged.AddHandler(caretPositionChanged)

    let guidPowerToolsCmdSet = "{5debbcf2-6cb1-480c-9e69-edcb2196bad7}"

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

    let isFileInCurrentProject fileName (projectProvider : ProjectProvider) =
        let filePath = Path.GetFullPath(fileName)
        // NB: this is not the fullproof way to match two paths
        projectProvider.SourceFiles |> Array.exists ((=) filePath)

    let validateName (symbol : FSharpSymbol) (projectProvider : ProjectProvider) = 
        { new IRenameValidator with
            member __.ValidateName(name) = 
                debug "[Rename Refactoring] Check the following name: %s" name
                match symbol.DeclarationLocation with
                // TODO: this should be determined before opening rename dialog
                | Some loc when not <| isFileInCurrentProject loc.FileName projectProvider ->
                    Choice2Of2 "Can't rename. The symbol isn't defined in current project."
                | _ ->
                    match symbol with
                    | :? FSharpUnionCase ->
                        // Union case shouldn't be lowercase
                        if isIdentifier name && not (String.IsNullOrEmpty(name) || Char.IsLower(name.[0])) then
                            Choice1Of2()
                        else
                            Choice2Of2 "Invalid name for union cases."
                    | _ ->
                        // TODO: possibly check operators and identifiers separately
                        if isIdentifier name || isOperator name then
                            Choice1Of2()
                        else
                            Choice2Of2 "Invalid name for identifiers or operators." }

    member this.HandleRename() =
        maybe {
            let! state = state
            let! cw = state.Word
            let! (symbol, references) = 
                  VSLanguageService.findUsages cw state.File state.Project
                  |> Async.RunSynchronously
                  // TODO: This part is going to diverse since we use all references (not only in the active document)
                  |> Option.map (fun (symbol, lastIdent, _, refs) -> 
                        symbol, refs 
                                |> Seq.choose (fun symbolUse -> 
                                    // We have to filter by file name otherwise the range is invalid wrt current snapshot
                                    if symbolUse.FileName = state.File then
                                        // Range01 type consists of zero-based values, which is a bit confusing
                                        Some (fromVSPos view.TextSnapshot symbolUse.Range)
                                    else None)
                                |> Seq.map (fun span -> 
                                    // Sometimes F.C.S returns a composite identifier which should be truncated
                                    let index = span.GetText().LastIndexOf (lastIdent)
                                    if index > 0 then 
                                        SnapshotSpan(view.TextSnapshot, span.Start.Position + index, span.Length - index)
                                    else span)
                                |> Seq.toList)
            let wnd = UI.loadRenameDialog(this, validateName symbol state.Project)
            let hostWnd = Window.GetWindow(view.VisualElement)
            wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
            wnd.Owner <- hostWnd
            this.Name <- cw.GetText()
            let! res = wnd.ShowDialog() |> Option.ofNullable
            if res then rename this.Name references } |> ignore

    member val IsAdded = false with get, set
    member val NextTarget : IOleCommandTarget = null with get, set
    member val Name = "" with get, set

    interface IOleCommandTarget with
        member this.Exec(pguidCmdGroup : byref<Guid>, nCmdId : uint32, nCmdexecopt : uint32, pvaIn : IntPtr, pvaOut : IntPtr) =
            if (pguidCmdGroup = Guid.Parse(guidPowerToolsCmdSet) && nCmdId = PkgCmdIDList.CmdidRenameCommand) then
                this.HandleRename()
            this.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member this.QueryStatus(pguidCmdGroup:byref<Guid>, cCmds:uint32, prgCmds:OLECMD[], pCmdText:IntPtr) =
            if pguidCmdGroup = Guid.Parse(guidPowerToolsCmdSet) && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdIDList.CmdidRefactorMenu) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            elif pguidCmdGroup = Guid.Parse(guidPowerToolsCmdSet) && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = PkgCmdIDList.CmdidRenameCommand) &&
                canRename() then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                this.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            

    interface IDisposable with
        member __.Dispose() = 
            view.LayoutChanged.RemoveHandler(viewLayoutChanged)
            view.Caret.PositionChanged.RemoveHandler(caretPositionChanged)