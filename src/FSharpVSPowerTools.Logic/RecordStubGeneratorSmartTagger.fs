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
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.RecordStubGenerator
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
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
                let expr = recordBinding.Expression

                let recordStubsInsertionPos = RecordStubsInsertionPosition.FromRecordExpression recordBinding
                return recordBinding, recordStubsInsertionPos
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
    let shouldGenerateRecordStub (recordBindingData: RecordBinding, _) (entity: FSharpEntity) =
        let fieldCount = entity.FSharpFields.Count
        let writtenFieldCount = recordBindingData.FieldExpressionList.Length
        fieldCount > 0 && writtenFieldCount < fieldCount

    let handleGenerateRecordStub (snapshot: ITextSnapshot) (recordBindingData: RecordBinding, insertionPos: _) displayContext entity = 
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let fieldsWritten = recordBindingData.FieldExpressionList

        use transaction = textUndoHistory.CreateTransaction(CommandName)

        let stub = RecordStubGenerator.formatRecord
                       insertionPos
                       indentSize
                       "failwith \"Uninitialized field\""
                       displayContext
                       entity
                       fieldsWritten
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.Position.Line-1).Start.Position + insertionPos.Position.Column

        buffer.Insert(currentLine, stub) |> ignore

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