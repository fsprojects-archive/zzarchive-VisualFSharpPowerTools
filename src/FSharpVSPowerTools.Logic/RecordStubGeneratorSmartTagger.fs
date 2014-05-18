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
open FSharpVSPowerTools.AsyncMaybe
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
    let mutable currentWord = None
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
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetActiveDocument() |> liftMaybe
            let! project = ProjectProvider.createForDocument doc |> liftMaybe
            let! newWord, symbol = vsLanguageService.GetSymbol(point, project) |> liftMaybe

            match symbol.Kind with
            | SymbolKind.Ident ->
                let! recordBindingData = collectRecordBindingData point doc project
                let! (fsSymbolUse, _results) = 
                    vsLanguageService.GetFSharpSymbolUse (newWord, symbol, doc.FullName, project, AllowStaleResults.MatchingSource)
                // Recheck cursor position to ensure it's still in new word
                let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
                if point.InSpan newWord then
                    let newRecordDefinition =
                        match fsSymbolUse.Symbol with
                        // The entity might correspond to another symbol 
                        | :? FSharpEntity as entity when entity.IsFSharpRecord && entity.DisplayName = symbol.Text ->
                            Some (newWord, (recordBindingData, fsSymbolUse.DisplayContext, entity))

                        // The entity might correspond to another symbol 
                        | :? FSharpField as field when field.DeclaringEntity.IsFSharpRecord && field.DisplayName = symbol.Text ->
                            Some (newWord, (recordBindingData, fsSymbolUse.DisplayContext, field.DeclaringEntity))
                        | _ -> None

                    return! liftMaybe newRecordDefinition
                else
                    return! liftMaybe None
            | _ -> return! liftMaybe None
        }
        |> Async.map (fun result -> 
            let changed =
                match recordDefinition, result, currentWord with
                | None, None, _ -> false
                | _, Some(newWord, _), Some oldWord -> newWord <> oldWord
                | _ -> true
            recordDefinition <- result
            if changed then
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
        |> Async.StartImmediate

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

    let handleGenerateRecordStub (snapshot: ITextSnapshot) (recordBindingData, pos: pos) displayContext entity = 
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
        let current = snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
        buffer.Insert(current, stub) |> ignore

        transaction.Complete()

    let generateRecordStub snapshot data displayContext entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = CommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = handleGenerateRecordStub snapshot data displayContext entity }

    member x.GetSmartTagActions(snapshot, data, displayContext, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        actionList.Add(generateRecordStub snapshot data displayContext entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<RecordStubGeneratorSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<RecordStubGeneratorSmartTag> seq =
            seq {
                match recordDefinition with
                | Some (word, (data, displayContext, entity)) when shouldGenerateRecordStub data entity ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, 
                                     RecordStubGeneratorSmartTag(x.GetSmartTagActions(word.Snapshot, data, displayContext, entity)))
                          :> ITagSpan<_>
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish