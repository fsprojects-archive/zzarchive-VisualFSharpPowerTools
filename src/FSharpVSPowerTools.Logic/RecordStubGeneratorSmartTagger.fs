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
                                    textUndoHistory: ITextUndoHistory,
                                    vsLanguageService: VSLanguageService,
                                    serviceProvider: IServiceProvider,
                                    projectFactory: ProjectFactory) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable recordDefinition = None

    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

    // Try to:
    // - Identify record expression binding
    // - Identify the '{' in 'let x: MyRecord = { }'
    let updateAtCaretPosition() =
        match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
        | Some point, Some word when word.Snapshot = view.TextSnapshot && point.InSpan word -> ()
        | _ ->
            let res =
                maybe {
                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                    let! doc = dte.GetActiveDocument()
                    let! project = projectFactory.CreateForDocument buffer doc
                    let! word, _ = vsLanguageService.GetSymbol(point, project) 
                    return point, doc, project, word
                }
            match res with
            | Some (point, doc, project, newWord) ->
                let wordChanged = 
                    match currentWord with
                    | None -> true
                    | Some oldWord -> newWord <> oldWord
                if wordChanged then
                    asyncMaybe {
                        let vsDocument = VSDocument(doc, point.Snapshot)
                        let! symbolRange, recordExpression, recordDefinition, insertionPos =
                            tryFindRecordDefinitionFromPos codeGenService project point vsDocument
                        let newWord = symbolRange

                        // Recheck cursor position to ensure it's still in new word
                        let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
                        if point.InSpan newWord then
                            return! Some (recordExpression, recordDefinition, insertionPos) |> liftMaybe
                        else
                            return! liftMaybe None
                    }
                    |> Async.map (fun result -> 
                        recordDefinition <- result
                        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                        tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
                    |> Async.StartImmediate
                    currentWord <- Some newWord
            | _ -> ()

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    500us, updateAtCaretPosition)

    // Check whether the record has been fully defined
    let shouldGenerateRecordStub (recordExpr: RecordExpr) (entity: FSharpEntity) =
        let fieldCount = entity.FSharpFields.Count
        let writtenFieldCount = recordExpr.FieldExprList.Length
        fieldCount > 0 && writtenFieldCount < fieldCount

    let handleGenerateRecordStub (snapshot: ITextSnapshot) (recordExpr: RecordExpr) (insertionPos: _) entity = 
        let fieldsWritten = recordExpr.FieldExprList

        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)

        let stub = RecordStubGenerator.formatRecord
                       insertionPos
                       "failwith \"Uninitialized field\""
                       entity
                       fieldsWritten
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.Position.Line-1).Start.Position + insertionPos.Position.Column

        buffer.Insert(currentLine, stub) |> ignore

        transaction.Complete()

    let generateRecordStub snapshot recordExpr insertionPos entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = Resource.recordGenerationCommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = handleGenerateRecordStub snapshot recordExpr insertionPos entity }

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
                match currentWord, recordDefinition with
                | Some word, Some (expression, entity, insertionPos) when shouldGenerateRecordStub expression entity ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, 
                                     RecordStubGeneratorSmartTag(x.GetSmartTagActions(word.Snapshot, expression, insertionPos, entity)))
                          :> ITagSpan<_>
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish