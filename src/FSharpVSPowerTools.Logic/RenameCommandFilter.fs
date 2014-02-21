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
open System.Windows

type PkgCmdIDList =
    static member CmdidMyCommand = 0x2001u
    static member CmdidMyMenu = 0x1100u

type RenameCommandFilter(tv : ITextView, sn : ITextStructureNavigator, h : ITextUndoHistory) =
    let view = tv
    let textStructureNavigator = sn
    let undoHistory = h

    let mutable currentWord = None

    let canRename (word : SnapshotSpan option) = 
        // TODO : it should be a symbol and it is define in current project
        word.IsSome

    let updateAtCaretPosition(caretPosition : CaretPosition) = 
        let point = caretPosition.Point.GetPoint(view.TextBuffer, caretPosition.Affinity)
        if point.HasValue then
            currentWord <- textStructureNavigator.FindAllWords(point.Value, None)

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

    let findUsages (requestPoint : SnapshotPoint) (word : SnapshotSpan) (newWordSpans : SnapshotSpan seq) = 
        async {
              try
                let (_, _, endLine, endCol) = word.GetRange()
                let dte = Package.GetGlobalService(typedefof<DTE>) :?> DTE
                let doc = dte.ActiveDocument
                Debug.Assert(doc <> null && doc.ProjectItem.ContainingProject <> null, "Should be able to find active document.")
                let project = try doc.ProjectItem.ContainingProject.Object :?> VSProject with _ -> null
                if project = null then
                    Debug.WriteLine("[Highlight Usage] Can't find containing project. Probably the document is opened in an ad-hoc way.")
                    return Seq.empty
                else
                    let currentFile = doc.FullName
                    let projectProvider = ProjectProvider(project)
                    let projectFileName = projectProvider.ProjectFileName
                    let source = requestPoint.Snapshot.GetText()
                    let currentLine = requestPoint.GetContainingLine().GetText()
                    let framework = projectProvider.TargetFramework
                    let args = projectProvider.CompilerOptions
                    let sourceFiles = 
                        let files = projectProvider.SourceFiles
                        // If there is no source file, use currentFile as an independent script
                        if Array.isEmpty files then [| currentFile |] else files
                    Debug.WriteLine("[Highlight Usage] Get symbol references for '{0}' at line {1} col {2} on {3} framework and '{4}' arguments", 
                        word.GetText(), endLine, endCol, sprintf "%A" framework, String.concat " " args)
                    let! results = 
                        VSLanguageService.Instance.GetUsesOfSymbolAtLocation(projectFileName, currentFile, source, sourceFiles, 
                                                                             endLine, endCol, currentLine, args, framework)
                    return 
                        match results with
                        | Some(_currentSymbolName, lastIdent, _currentSymbolRange, references) -> 
                            let possibleSpans = HashSet(newWordSpans)
                            // Since we can't select multi-word, lastIdent is for debugging only.
                            Debug.WriteLine(sprintf "[Highlight Usage] The last identifier found is '%s'" lastIdent)
                            references
                            |> Seq.choose (fun (fileName, ((beginLine, beginCol), (endLine, endCol))) -> 
                                // We have to filter by file name otherwise the range is invalid wrt current snapshot
                                if fileName = currentFile then
                                    // Range01 type consists of zero-based values, which is a bit confusing
                                    Some (fromVSPos(word.Snapshot, beginLine, beginCol, endLine, endCol))
                                else None)
                            |> Seq.choose (fun span -> 
                                let subSpan = 
                                    // Sometimes F.C.S returns a composite identifier which should be truncated
                                    let index = span.GetText().LastIndexOf (lastIdent)
                                    if index > 0 then 
                                        SnapshotSpan(word.Snapshot, span.Start.Position + index, span.Length - index)
                                    else span
                                if possibleSpans.Contains(subSpan) then Some subSpan else None)
                        | None ->
                            Seq.empty
              with e ->
                Debug.WriteLine(sprintf "[Highlight Usage] %O exception occurs while updating." e)
                return Seq.empty
        }

    let rename newText foundUsages = ()

    let handleRename word =
        match word with
        | None -> ()
        | Some w ->
            rename "newVal" ()        
            MessageBox.Show("Should do rename refactoring", "F# Power Tools") |> ignore

    member val IsAdded = false with get, set
    member val NextTarget : IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member this.Exec(pguidCmdGroup : byref<Guid>, nCmdId : uint32, nCmdexecopt : uint32, pvaIn : IntPtr, pvaOut : IntPtr) =
            if (pguidCmdGroup = Guid.Parse(guidMenuAndCommandsCmdSet) && nCmdId = PkgCmdIDList.CmdidMyCommand) then
                handleRename currentWord
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


