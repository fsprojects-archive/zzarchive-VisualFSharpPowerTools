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

type VSSnapshot(doc: EnvDTE.Document, snapshot: ITextSnapshot) =
    interface ISnapshot with
        member x.FullName = doc.FullName
        member x.GetText() = snapshot.GetText()
        member x.GetLineText0(line0) =
            snapshot.GetLineFromLineNumber(int line0).GetText()

        member x.GetLineText1(line1) =
            snapshot.GetLineFromLineNumber(int line1 - 1).GetText()

type CodeGenerationInfra
    (serviceProvider: IServiceProvider,
     languageService: VSLanguageService) =
    interface ICodeGenerationInfra<IProjectProvider, SnapshotPoint, SnapshotSpan> with
        member x.GetSymbolAtPosition(_snapshot, pos) =
            asyncMaybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetActiveDocument() |> liftMaybe
                let! project = ProjectProvider.createForDocument doc |> liftMaybe
                let! range, symbol = languageService.GetSymbol(pos, project) |> liftMaybe
                return range, symbol
            }
        
        member x.GetSymbolAndUseAtPositionOfKind(project, snapshot, pos, kind) =
            asyncMaybe {
                let x = x :> ICodeGenerationInfra<_, _, _>
                let! range, symbol = x.GetSymbolAtPosition(snapshot, pos)

                match symbol.Kind with
                | k when k = kind ->
                    let! symbolUse, _ =
                        languageService.GetFSharpSymbolUse(range, symbol, snapshot.FullName, project, AllowStaleResults.MatchingSource)
                    return range, symbol, symbolUse
                | _ -> return! None |> liftMaybe
            }

        member x.ParseFileInProject(snapshot, project) =
            languageService.ParseFileInProject(snapshot.FullName, snapshot.GetText(), project)
        
        member x.ExtractFSharpPos(pos) =
            let line = pos.Snapshot.GetLineNumberFromPosition pos.Position
            let caretColumn = pos.Position - pos.GetContainingLine().Start.Position
            Pos.fromZ line caretColumn

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

    let codeGenInfra: ICodeGenerationInfra<_, _, _> =
        upcast CodeGenerationInfra(serviceProvider, vsLanguageService)

    // Try to:
    // - Identify record expression binding
    // - Identify the '{' in 'let x: MyRecord = { }'
    let updateAtCaretPosition() =
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetActiveDocument() |> liftMaybe
            let! project = ProjectProvider.createForDocument doc |> liftMaybe
            let snapshot = VSSnapshot(doc, point.Snapshot)
            let! symbolRange, recordExpression, recordDefinition, insertionPos =
                tryGetRecordDefinitionFromPos codeGenInfra project point snapshot
            let newWord = symbolRange

            // Recheck cursor position to ensure it's still in new word
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            if point.InSpan newWord then
                return! Some(newWord, recordExpression, recordDefinition, insertionPos)
                        |> liftMaybe
            else
                return! liftMaybe None
        }
        |> Async.map (fun result -> 
            let changed =
                match recordDefinition, result, currentWord with
                | None, None, _ -> false
                | _, Some(newWord, _, _, _), Some oldWord -> newWord <> oldWord
                | _ -> true
            recordDefinition <- result
            if changed then
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
        |> Async.StartImmediate

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    200us, updateAtCaretPosition)

    // Check whether the record has been fully defined
    let shouldGenerateRecordStub (recordBindingData: RecordExpression) (entity: FSharpEntity) =
        let fieldCount = entity.FSharpFields.Count
        let writtenFieldCount = recordBindingData.FieldExpressionList.Length
        fieldCount > 0 && writtenFieldCount < fieldCount

    let handleGenerateRecordStub (snapshot: ITextSnapshot) (recordBindingData: RecordExpression) (insertionPos: _) entity = 
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let fieldsWritten = recordBindingData.FieldExpressionList

        use transaction = textUndoHistory.CreateTransaction(CommandName)

        let stub = RecordStubGenerator.formatRecord
                       insertionPos
                       indentSize
                       "failwith \"Uninitialized field\""
                       entity
                       fieldsWritten
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.Position.Line-1).Start.Position + insertionPos.Position.Column

        buffer.Insert(currentLine, stub) |> ignore

        transaction.Complete()

    let generateRecordStub snapshot expression insertionPos entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = CommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = handleGenerateRecordStub snapshot expression insertionPos entity }

    member x.GetSmartTagActions(snapshot, expression, insertionPos, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        actionList.Add(generateRecordStub snapshot expression insertionPos entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<RecordStubGeneratorSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<RecordStubGeneratorSmartTag> seq =
            seq {
                match recordDefinition with
                | Some (word, expression, entity, insertionPos) when shouldGenerateRecordStub expression entity ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, 
                                     RecordStubGeneratorSmartTag(x.GetSmartTagActions(word.Snapshot, expression, insertionPos, entity)))
                          :> ITagSpan<_>
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish