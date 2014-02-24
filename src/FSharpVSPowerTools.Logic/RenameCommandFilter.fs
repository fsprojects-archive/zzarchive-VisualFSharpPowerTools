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
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

module PkgCmdIDList =
    let CmdidRenameCommand = 0x2001u
    let CmdidRefactorMenu = 0x1100u

type DocumentState =
    { Word: SnapshotSpan option
      File: string
      Project: ProjectProvider }

type RenameCommandFilter(view : IWpfTextView, serviceProvider : System.IServiceProvider) =
    let mutable state = None
    let documentUpdater = DocumentUpdater(serviceProvider)

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

    let rename (oldText : string) (newText : string) (foundUsages: (string * Range01 seq) seq) =
        try
            let undo = documentUpdater.BeginGlobalUndo("Rename Refactoring")
            try
                let doc = Dte.getActiveDocument()
                for (fileName, ranges) in foundUsages do
                    let buffer = documentUpdater.GetBufferForDocument(fileName)
                    for r in ranges do
                        let snapshotSpan = fromVSPos buffer.CurrentSnapshot r
                        let i = snapshotSpan.GetText().LastIndexOf(oldText)
                        let span = 
                            if i > 0 then 
                                // Subtract lengths of qualified identifiers
                                Span(snapshotSpan.Start.Position + i, snapshotSpan.Length - i) 
                            else snapshotSpan.Span
                        buffer.Replace(span, newText) |> ignore
                // Refocus to the current document
                doc.Activate()
            finally
                documentUpdater.EndGlobalUndo(undo)
        with e ->
            debug "[Rename Refactoring] Error %O occurs while renaming symbols." e

    let isFileInCurrentProject fileName (projectProvider : ProjectProvider) =
        let filePath = Path.GetFullPath(fileName)
        // NB: this isn't a foolproof way to match two paths
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
            let! (symbol, currentName, references) = 
                  VSLanguageService.findUsages cw state.File state.Project
                  |> Async.RunSynchronously
                  // TODO: This part is going to diverse since we use all references (not only in the active document)
                  |> Option.map (fun (symbol, lastIdent, _, refs) -> 
                        symbol, lastIdent,
                            refs 
                            |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse.Range))
                            |> Seq.groupBy (fst >> Path.GetFullPath)
                            |> Seq.map (fun (fileName, symbolUses) -> fileName, Seq.map snd symbolUses))
            let wnd = UI.loadRenameDialog(this, validateName symbol state.Project)
            let hostWnd = Window.GetWindow(view.VisualElement)
            wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
            wnd.Owner <- hostWnd
            this.Name <- cw.GetText()
            let! res = wnd.ShowDialog() |> Option.ofNullable
            if res then rename currentName this.Name references } |> ignore

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