namespace FSharpVSPowerTools.Refactoring

open System.Windows
open System.Windows.Threading
open System.Collections.Generic
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open System
open System.IO
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type ImplementInterfaceSmartTag(actionSets) = 
    inherit SmartTag(SmartTagType.Factoid, actionSets)

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type ImplementInterfaceSmartTagger(view: ITextView, buffer: ITextBuffer, 
                                   editorOptionsFactory: IEditorOptionsFactoryService, textUndoHistory: ITextUndoHistory,
                                   vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable interfaceDefinition = None

    let collectInterfaceData (point: SnapshotPoint) (doc: EnvDTE.Document) (project: IProjectProvider) =
        async {
            let line = point.Snapshot.GetLineNumberFromPosition point.Position
            let column = point.Position - point.GetContainingLine().Start.Position
            let source = point.Snapshot.GetText()
            let! ast = vsLanguageService.ParseFileInProject(doc.FullName, source, project)
            let pos = Pos.fromZ line column
            let data =
                ast.ParseTree
                |> Option.bind (InterfaceStubGenerator.tryFindInterfaceDeclaration pos)
                |> Option.map (fun iface -> 
                    let lineStr = point.GetContainingLine().GetText()
                    let tokens = vsLanguageService.TokenizeLine(buffer, project.CompilerOptions, line) 
                    let endPosOfWith =
                        tokens |> List.tryPick (fun (t: TokenInformation) ->
                                    if t.CharClass = TokenCharKind.Keyword && t.LeftColumn >= column then
                                        let text = lineStr.[t.LeftColumn..t.RightColumn]
                                        match text with
                                        | "with" -> Some (Pos.fromZ line (t.RightColumn + 1))
                                        | _ -> None
                                    else None)
                    iface, endPosOfWith)
            return data
        }

    let hasSameStartPos (r1: range) (r2: range) =
        r1.Start = r2.Start

    let updateAtCaretPosition() =
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> AsyncMaybe.liftMaybe
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetActiveDocument() |> AsyncMaybe.liftMaybe
            let! project = ProjectProvider.createForDocument doc |> AsyncMaybe.liftMaybe
            let! newWord, symbol = vsLanguageService.GetSymbol(point, project) |> AsyncMaybe.liftMaybe

            match symbol.Kind with
            | SymbolKind.Ident ->
                let! interfaceData = collectInterfaceData point doc project
                let! (fsSymbolUse, _) = 
                    vsLanguageService.GetFSharpSymbolUse (newWord, symbol, doc.FullName, project, AllowStaleResults.MatchingSource)
                // Recheck cursor position to ensure it's still in new word
                let! point = buffer.GetSnapshotPoint view.Caret.Position |> AsyncMaybe.liftMaybe
                return 
                    if (fsSymbolUse.Symbol :? FSharpEntity) && point.InSpan newWord then
                        let entity = fsSymbolUse.Symbol :?> FSharpEntity
                        // The entity might correspond to another symbol so we check for symbol text and start ranges as well
                        if InterfaceStubGenerator.isInterface entity && entity.DisplayName = symbol.Text && hasSameStartPos fsSymbolUse.RangeAlternate (fst interfaceData).Range  then
                            interfaceDefinition <- Some (interfaceData, fsSymbolUse.DisplayContext, entity)
                            currentWord <- Some newWord
                            let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                            Some (tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
                        else None
                    else None
            | _ -> return None 
        }
        |> Async.map (function None -> interfaceDefinition <- None | _ -> ())
        |> Async.StartImmediate

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    200us, updateAtCaretPosition)

    let inferStartColumn = function
        | InterfaceData.Interface(_, Some (m :: _)) ->
            let line = buffer.CurrentSnapshot.GetLineFromLineNumber(m.Range.StartLine-1)
            let str = line.GetText()
            str.Length - str.TrimStart(' ').Length
        | InterfaceData.ObjExpr(_, b :: _) -> 
            let line = buffer.CurrentSnapshot.GetLineFromLineNumber(b.RangeOfBindingSansRhs.StartLine-1)
            let str = line.GetText()
            str.Length - str.TrimStart(' ').Length
        | iface ->
            // There is no implemented member, we indent the content at the start column of the interface
            iface.Range.StartColumn

    // Check whether the interface has been fully implemented
    let shouldImplementInterface (interfaceData: InterfaceData, _) (entity: FSharpEntity) =
        // TODO: counting members is not enough.
        // We should match member signatures, 
        // it will be tricky in case of specialized interface implementation
        let count = InterfaceStubGenerator.countInterfaceMembers entity
        count > 0 && count > interfaceData.MemberCount

    let handleImplementInterface (span: SnapshotSpan) (interfaceData, posOpt: pos option) displayContext entity = 
        let startColumn = inferStartColumn interfaceData
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let typeParams = interfaceData.TypeParameters
        let stub = InterfaceStubGenerator.formatInterface 
                        startColumn indentSize typeParams "x" "raise (System.NotImplementedException())" displayContext entity

        use transaction = textUndoHistory.CreateTransaction("Implement Interface Explicitly")
        match posOpt with
        | Some pos -> 
            let currentPos = span.Snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
            buffer.Insert(currentPos, stub + new String(' ', startColumn)) |> ignore
        | None ->
            let range = interfaceData.Range
            let currentPos = span.Snapshot.GetLineFromLineNumber(range.EndLine-1).Start.Position + range.EndColumn
            buffer.Insert(currentPos, " with") |> ignore
            buffer.Insert(currentPos + 5, stub + new String(' ', startColumn)) |> ignore
        transaction.Complete()

    let implementInterface span data displayContext entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = "Implement Interface Explicitly"
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = 
                if shouldImplementInterface data entity then
                    handleImplementInterface span data displayContext entity
                else
                    messageBoxError Resource.implementInterfaceErrorMessage }

    member x.GetSmartTagActions(span: SnapshotSpan, data, displayContext, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        let trackingSpan = span.Snapshot.CreateTrackingSpan(span.Span, SpanTrackingMode.EdgeInclusive)
        let _snapshot = trackingSpan.TextBuffer.CurrentSnapshot

        actionList.Add(implementInterface span data displayContext entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<ImplementInterfaceSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<ImplementInterfaceSmartTag> seq =
            seq {
                match currentWord, interfaceDefinition with
                | Some word, Some (data, displayContext, entity) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<ImplementInterfaceSmartTag>(span, 
                            ImplementInterfaceSmartTag(x.GetSmartTagActions(span, data, displayContext, entity)))
                            :> ITagSpan<_>
                | _ -> ()
            }

        member x.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member x.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)