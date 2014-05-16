namespace FSharpVSPowerTools.Refactoring

open System
open System.Collections.Generic
open System.Windows
open System.Windows.Threading
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.Core.CodeGeneration
open FSharpVSPowerTools.Core.CodeGeneration.RecordStubGenerator
open FSharpVSPowerTools.ProjectSystem
open FSharp.CompilerBinding
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

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

    let [<Literal>] CommandName = "Generate record stubs"

    // Try to:
    // - Identify record expression binding
    // - Identify the '{' in 'let x: MyRecord = { }'
    let collectRecordBindingData (point: SnapshotPoint) (doc: EnvDTE.Document) (project: IProjectProvider) =
        async {
            let line = point.Snapshot.GetLineNumberFromPosition point.Position
            let caretColumn = point.Position - point.GetContainingLine().Start.Position
            let source = point.Snapshot.GetText()
            let! ast = vsLanguageService.ParseFileInProject(doc.FullName, source, project)
            let pos = Pos.fromZ line caretColumn

            return maybe {
                let! parsedInput = ast.ParseTree
                let! recordBinding = RecordStubGenerator.tryFindRecordBinding pos parsedInput
                let expr, lBraceLeftColumnCondition =
                    match recordBinding with
                    | TypedRecordBinding(_, expr, _) -> expr, (fun (t: TokenInformation) -> t.LeftColumn >= caretColumn)
                    | QualifiedFieldRecordBinding(expr, _)
                    | NonQualifiedFieldRecordBinding(expr, _) -> expr, (fun (t: TokenInformation) -> t.LeftColumn < caretColumn)
                let exprStartLine1 = expr.Range.StartLine
                let exprStartLine0 = exprStartLine1 - 1

                // Tokenize line where the record expression starts
                let tokens = vsLanguageService.TokenizeLine(buffer, project.CompilerOptions, exprStartLine0) 

                // We want to go after the '{' that' is right after the caret position
                let! endPosOfLBrace =
                    tokens
                    |> List.tryPick (fun (t: TokenInformation) ->
                                if t.CharClass = TokenCharKind.Delimiter &&
                                   (pos.Line <> exprStartLine1 || lBraceLeftColumnCondition t) &&
                                   t.TokenName = "LBRACE" then
                                    Some (Pos.fromZ exprStartLine0 (t.RightColumn + 1))
                                else None)

                return recordBinding, endPosOfLBrace
            }
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
                                | Some (fsSymbolUse, _), Some point when point.InSpan newWord ->

                                    let newRecordDefinition =
                                        match fsSymbolUse.Symbol with
                                        // The entity might correspond to another symbol 
                                        | :? FSharpEntity as entity when entity.IsFSharpRecord && entity.DisplayName = symbol.Text ->
                                            Some (recordBindingData, fsSymbolUse.DisplayContext, entity)

                                        // The entity might correspond to another symbol 
                                        | :? FSharpField as field when field.DeclaringEntity.IsFSharpRecord && field.DisplayName = symbol.Text ->
                                            Some (recordBindingData, fsSymbolUse.DisplayContext, field.DeclaringEntity)
                                        | _ -> None

                                    recordDefinition <- newRecordDefinition

                                    match newRecordDefinition with
                                    | Some _ ->
                                        currentWord <- Some newWord
                                        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                                        tagsChanged.Trigger(self, SnapshotSpanEventArgs(span))
                                    | None -> ()

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

    // Check whether the record has been fully defined
    let shouldGenerateRecordStub (recordBindingData, _) (entity: FSharpEntity) =
        let fieldCount = entity.FSharpFields.Count
        let writtenFieldCount =
            match recordBindingData with
            | TypedRecordBinding(_, _, fields)
            | QualifiedFieldRecordBinding(_, fields)
            | NonQualifiedFieldRecordBinding(_, fields) -> List.length fields
        fieldCount > 0 && writtenFieldCount < fieldCount

    let handleGenerateRecordStub (span: SnapshotSpan) (recordBindingData, pos: pos) displayContext entity = 
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let fieldsWritten =
            match recordBindingData with
            | TypedRecordBinding(_, _, fieldsWritten)
            | QualifiedFieldRecordBinding(_, fieldsWritten)
            | NonQualifiedFieldRecordBinding(_, fieldsWritten) -> fieldsWritten

        use transaction = textUndoHistory.CreateTransaction(CommandName)

        let stub = RecordStubGenerator.formatRecord
                       pos.Column
                       indentSize
                       "failwith \"Uninitialized field\""
                       displayContext
                       entity
                       fieldsWritten
        let current = span.Snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
        buffer.Insert(current, stub) |> ignore

        transaction.Complete()

    let generateRecordStub span data displayContext entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = CommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = handleGenerateRecordStub span data displayContext entity }

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
                | Some word, Some (data, displayContext, entity) when shouldGenerateRecordStub data entity ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, 
                                     RecordStubGeneratorSmartTag(x.GetSmartTagActions(span, data, displayContext, entity)))
                          :> ITagSpan<_>
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish