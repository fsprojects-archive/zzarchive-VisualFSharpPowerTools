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
open FSharpVSPowerTools.Core.CodeGeneration
open FSharpVSPowerTools.Core.CodeGeneration.RecordStubGenerator
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: edit all comments containing 'implement'
type RecordStubGeneratorSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type RecordStubGeneratorSmartTagger(view: ITextView,
                                    buffer: ITextBuffer,
                                    editorOptionsFactory: IEditorOptionsFactoryService,
                                    textUndoHistory: ITextUndoHistory,
                                    vsLanguageService: VSLanguageService,
                                    serviceProvider: IServiceProvider) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable recordDefinition = None

    let [<Literal>] CommandName = "Generate record stub"

    // Try to:
    // - Identify record expression binding
    // - Try to identify the '{' in 'let x: MyRecord = { }'
    let collectRecordBindingData (point: SnapshotPoint) (doc: EnvDTE.Document) (project: IProjectProvider) =
        async {
            let line = point.Snapshot.GetLineNumberFromPosition point.Position
            let column = point.Position - point.GetContainingLine().Start.Position
            let source = point.Snapshot.GetText()
            let! ast = vsLanguageService.ParseFileInProject(doc.FullName, source, project)
            let pos = Pos.fromZ line column
            let data =
                ast.ParseTree
                |> Option.bind (RecordStubGenerator.tryFindRecordBinding pos)
                |> Option.map (fun recordBinding -> 
                    let lineStr = point.GetContainingLine().GetText()
                    let tokens = vsLanguageService.TokenizeLine(buffer, project.CompilerOptions, line) 
                    // TODO: we want to go after the '{' that' is right after the caret position
                    let endPosOfLBrace =
                        tokens |> List.tryPick (fun (t: TokenInformation) ->
                                    if t.CharClass = TokenCharKind.Delimiter &&
                                       t.LeftColumn >= column &&
                                       t.TokenName = "LBRACE" then
                                        Some (Pos.fromZ line (t.RightColumn + 1))
                                    else None)
                    recordBinding, endPosOfLBrace)
            return data
        }

    let updateAtCaretPosition() =
        match buffer.GetSnapshotPoint view.Caret.Position with
        | Some point ->
            maybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument()
                let! project = ProjectProvider.createForDocument doc
                let! newWord, symbol = vsLanguageService.GetSymbol(point, project)
                return newWord, symbol, doc, project
            }
            |> function
                | Some (newWord, symbol, doc, project) ->
                    async {
                        match symbol.Kind with
                        | SymbolKind.Ident ->
                            let! recordBindingData' = collectRecordBindingData point doc project
                            match recordBindingData' with
                            | Some recordBindingData ->
                                let! results = vsLanguageService.GetFSharpSymbolUse (newWord, symbol, doc.FullName, project, 
                                                    AllowStaleResults.MatchingSource)
                                // Recheck cursor position to ensure it's still in new word
                                match results, buffer.GetSnapshotPoint view.Caret.Position with
                                | Some (fsSymbolUse, _), Some point when (fsSymbolUse.Symbol :? FSharpEntity) && point.InSpan newWord ->
                                    let entity = fsSymbolUse.Symbol :?> FSharpEntity
                                    // The entity might correspond to another symbol 
                                    if entity.IsFSharpRecord && entity.DisplayName = symbol.Text then
                                        recordDefinition <- Some (recordBindingData, fsSymbolUse.DisplayContext, entity)
                                        currentWord <- Some newWord
                                        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                                        return tagsChanged.Trigger(self, SnapshotSpanEventArgs(span))
                                    else
                                        return recordDefinition <- None
                                | _ ->
                                    return recordDefinition <- None
                            | None ->
                                return recordDefinition <- None 
                        | _ ->
                            return recordDefinition <- None 

                    } |> Async.StartImmediate
                | None ->
                    recordDefinition <- None 
        | None -> ()

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    200us, updateAtCaretPosition)

    let getRange = function
        RecordBinding(typ, _) -> typ.Range

    let inferStartColumn = function
        | RecordBinding(_typ, ((field, _, _) :: _)) ->
            let longIdent, _ = field
            let line = buffer.CurrentSnapshot.GetLineFromLineNumber(longIdent.Range.StartLine - 1)
            let str = line.GetText()
            str.Length - str.TrimStart(' ').Length
        | recordBinding ->
            // TODO: this logic may not be adapted to a record
            // There is no assied record field, we indent the content at the start column of the definition
            (getRange recordBinding).StartColumn

    let countFields = function
        RecordBinding(_, fields) -> List.length fields

//    let getTypeParameters = function
//        | InterfaceData.Interface(typ, _)
//        | InterfaceData.ObjExpr(typ, _) ->
//            match typ with
//            | SynType.App(_, _, ts, _, _, _, _)
//            | SynType.LongIdentApp(_, _, _, ts, _, _, _) ->
//                let (|TypeIdent|_|) = function
//                    | SynType.Var(SynTypar.Typar(s, req , _), _) ->
//                        match req with
//                        | NoStaticReq -> 
//                            Some ("'" + s.idText)
//                        | HeadTypeStaticReq -> 
//                            Some ("^" + s.idText)
//                    | SynType.LongIdent(LongIdentWithDots(xs, _)) ->
//                        xs |> Seq.map (fun x -> x.idText) |> String.concat "." |> Some
//                    | _ -> 
//                        None
//                ts |> Seq.choose (|TypeIdent|_|) |> Seq.toArray
//            | _ ->
//                [||]

    // Check whether the record has been fully implemented
    let shouldGenerateRecordStub (recordBindingData, _) (entity: FSharpEntity) =
        // TODO: counting members is not enough.
        // We should match member signatures, 
        // it will be tricky in case of specialized interface implementation
        let count = RecordStubGenerator.countRecordFields entity
        count > 0 && count > countFields recordBindingData

    let handleGenerateRecordStub (span: SnapshotSpan) (recordBindingData, posOpt: pos option) displayContext entity = 
        let startColumn = inferStartColumn recordBindingData
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
//        let typeParams = getTypeParameters recordBindingData

        // TODO: implement format record
//        let stub = InterfaceStubGenerator.formatInterface 
//                        startColumn indentSize typeParams "x" "raise (System.NotImplementedException())" displayContext entity
//
        use transaction = textUndoHistory.CreateTransaction(CommandName)
//        match posOpt with
//        | Some pos -> 
//            let current = span.Snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
//            buffer.Insert(current, stub) |> ignore
//        | None ->
//            let range = getRange interfaceData
//            let current = span.Snapshot.GetLineFromLineNumber(range.EndLine-1).Start.Position + range.EndColumn
//            buffer.Insert(current, " with") |> ignore
//            buffer.Insert(current + 5, stub) |> ignore
        transaction.Complete()

    let generateRecordStub span data displayContext entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = CommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = 
                if shouldGenerateRecordStub data entity then
                    handleGenerateRecordStub span data displayContext entity
                else
                    messageBoxError Resource.implementInterfaceErrorMessage }

    member x.GetSmartTagActions(span: SnapshotSpan, data, displayContext, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        let trackingSpan = span.Snapshot.CreateTrackingSpan(span.Span, SpanTrackingMode.EdgeInclusive)
        let _snapshot = trackingSpan.TextBuffer.CurrentSnapshot

        actionList.Add(generateRecordStub span data displayContext entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<RecordStubGeneratorSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<RecordStubGeneratorSmartTag> seq =
            seq {
                match currentWord, recordDefinition with
                | Some word, Some (data, displayContext, entity) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<RecordStubGeneratorSmartTag>(span, 
                            RecordStubGeneratorSmartTag(x.GetSmartTagActions(span, data, displayContext, entity)))
                            :> ITagSpan<_>
                | _ -> ()
            }

        member x.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member x.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)