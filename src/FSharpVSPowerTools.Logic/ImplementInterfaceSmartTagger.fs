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
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type ImplementInterfaceSmartTag(actionSets) = 
    inherit SmartTag(SmartTagType.Factoid, actionSets)

[<NoEquality; NoComparison>]
type InterfaceState =
    { InterfaceData: InterfaceData 
      EndPosOfWith: pos option
      Tokens: TokenInformation list }

type ImplementInterfaceSmartTagger(view: ITextView, buffer: ITextBuffer, 
                                   editorOptionsFactory: IEditorOptionsFactoryService, textUndoHistory: ITextUndoHistory,
                                   vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord = None
    let mutable state = None

    let queryInterfaceState (point: SnapshotPoint) (doc: EnvDTE.Document) (project: IProjectProvider) =
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
                    let tokens = vsLanguageService.TokenizeLine(buffer, project.CompilerOptions, line) 
                    let endPosOfWidth =
                        tokens 
                        |> List.tryPick (fun (t: TokenInformation) ->
                                if t.CharClass = TokenCharKind.Keyword && t.LeftColumn >= column && t.TokenName = "WITH" then
                                    Some (Pos.fromZ line (t.RightColumn + 1))
                                else None)
                    { InterfaceData = iface; EndPosOfWith = endPosOfWidth; Tokens = tokens })
            return data
        }

    let hasSameStartPos (r1: range) (r2: range) =
        r1.Start = r2.Start

    let updateAtCaretPosition() =
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            match currentWord with
            | Some word when point.InSpan word -> 
                return! liftMaybe state
            | _ ->
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument() |> liftMaybe
                let! project = ProjectProvider.createForDocument doc |> liftMaybe
                let! newWord, symbol = vsLanguageService.GetSymbol(point, project) |> liftMaybe
                match symbol.Kind with
                | SymbolKind.Ident ->
                    let! interfaceState = queryInterfaceState point doc project
                    let! (fsSymbolUse, results) = 
                        vsLanguageService.GetFSharpSymbolUse (newWord, symbol, doc.FullName, project, AllowStaleResults.MatchingSource)
                    // Recheck cursor position to ensure it's still in new word
                    let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
                    return!
                       (match fsSymbolUse.Symbol with
                        | :? FSharpEntity as entity when point.InSpan newWord ->
                            // The entity might correspond to another symbol so we check for symbol text and start ranges as well
                            if InterfaceStubGenerator.isInterface entity && entity.DisplayName = symbol.Text 
                                && hasSameStartPos fsSymbolUse.RangeAlternate interfaceState.InterfaceData.Range then
                                Some (newWord, (interfaceState, fsSymbolUse.DisplayContext, entity, results))
                            else None
                        | _ -> None) |> liftMaybe
                | _ -> return! liftMaybe None
        }
        |> Async.map (fun result -> 
            let changed =
                match state, result, currentWord with
                | None, None, _ -> false
                | _, Some (newWord, _), Some oldWord -> newWord <> oldWord
                | _ -> true
            state <- result
            if changed then
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
        |> Async.StartImmediate

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    200us, updateAtCaretPosition)

    let getLineIdent (lineStr: string) =
        lineStr.Length - lineStr.TrimStart(' ').Length

    let inferStartColumn indentSize state = 
        match InterfaceStubGenerator.getMemberNameAndRanges state.InterfaceData with
        | (_, range) :: _ ->
            let lineStr = buffer.CurrentSnapshot.GetLineFromLineNumber(range.StartLine-1).GetText()
            getLineIdent lineStr
        | [] ->
            match state.InterfaceData with
            | InterfaceData.Interface _ as iface ->
                // 'interface ISomething with' is often in a new line, we use the indentation of that line
                let lineStr = buffer.CurrentSnapshot.GetLineFromLineNumber(iface.Range.StartLine-1).GetText()
                getLineIdent lineStr + indentSize
            | InterfaceData.ObjExpr _ as iface ->
                state.Tokens 
                |> List.tryPick (fun (t: TokenInformation) ->
                            if t.CharClass = TokenCharKind.Keyword && t.TokenName = "NEW" then
                                Some (t.LeftColumn + indentSize)
                            else None)
                // There is no reference point, we indent the content at the start column of the interface
                |> Option.getOrElse iface.Range.StartColumn

    // Check whether the interface is empty
    let shouldImplementInterface _interfaceState (entity: FSharpEntity) =
        not (InterfaceStubGenerator.hasNoInterfaceMember entity)

    let handleImplementInterface (snapshot: ITextSnapshot) state displayContext implementedMemberSignatures entity = 
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let startColumn = inferStartColumn indentSize state
        let typeParams = state.InterfaceData.TypeParameters
        let stub = InterfaceStubGenerator.formatInterface startColumn indentSize typeParams 
                    "x" "raise (System.NotImplementedException())" displayContext implementedMemberSignatures entity
        if String.IsNullOrEmpty stub then
            let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
            statusBar.SetText(Resource.interfaceFilledStatusMessage) |> ignore
        else
            use transaction = textUndoHistory.CreateTransaction("Implement Interface Explicitly")
            match state.EndPosOfWith with
            | Some pos -> 
                let currentPos = snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
                buffer.Insert(currentPos, stub + new String(' ', startColumn)) |> ignore
            | None ->
                let range = state.InterfaceData.Range
                let currentPos = snapshot.GetLineFromLineNumber(range.EndLine-1).Start.Position + range.EndColumn
                buffer.Insert(currentPos, " with") |> ignore
                buffer.Insert(currentPos + 5, stub + new String(' ', startColumn)) |> ignore
            transaction.Complete()

    let implementInterface snapshot (state: InterfaceState, displayContext, entity, results: ParseAndCheckResults) =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = "Implement Interface Explicitly"
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = 
                if shouldImplementInterface state entity then
                    async {
                        let getMemberByLocation(name, range: range) =
                            let lineStr = 
                                match fromFSharpPos snapshot range with
                                | Some span -> span.End.GetContainingLine().GetText()
                                | None -> String.Empty
                            results.GetSymbolUseAtLocation(range.EndLine, range.EndColumn, lineStr, [name])
                        let! implementedMemberSignatures = InterfaceStubGenerator.getImplementedMemberSignatures 
                                                               getMemberByLocation displayContext state.InterfaceData
                        return handleImplementInterface snapshot state displayContext implementedMemberSignatures entity
                    }
                    |> Async.StartImmediate
                else
                    let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
                    statusBar.SetText(Resource.interfaceEmptyStatusMessage) |> ignore }

    member x.GetSmartTagActions(snapshot, interfaceDefinition) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        actionList.Add(implementInterface snapshot interfaceDefinition)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<ImplementInterfaceSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<ImplementInterfaceSmartTag> seq =
            seq {
                match state with
                | Some (word, interfaceDefinition) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<ImplementInterfaceSmartTag>(span, 
                            ImplementInterfaceSmartTag(x.GetSmartTagActions(word.Snapshot, interfaceDefinition)))
                            :> ITagSpan<_>
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish
