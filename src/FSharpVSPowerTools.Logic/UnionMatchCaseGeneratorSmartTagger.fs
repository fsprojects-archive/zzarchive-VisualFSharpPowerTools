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
open FSharpVSPowerTools.CodeGeneration.UnionMatchCaseGenerator
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type UnionMatchCaseGeneratorSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type UnionMatchCaseGeneratorSmartTagger(view: ITextView,
                                        buffer: ITextBuffer,
                                        editorOptionsFactory: IEditorOptionsFactoryService,
                                        textUndoHistory: ITextUndoHistory,
                                        vsLanguageService: VSLanguageService,
                                        serviceProvider: IServiceProvider) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord = None
    let mutable unionDefinition = None

    let [<Literal>] CommandName = "Generate union type cases"

    let codeGenInfra: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService)

    let updateAtCaretPosition() =
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetActiveDocument() |> liftMaybe
            let! project = ProjectProvider.createForDocument doc |> liftMaybe
            let vsDocument = VSDocument(doc, point.Snapshot)
            let! symbolRange, recordExpression, recordDefinition, insertionPos =
                tryGetRecordDefinitionFromPos codeGenInfra project point vsDocument
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
                match unionDefinition, result, currentWord with
                | None, None, _ -> false
                | _, Some(newWord, _, _, _), Some oldWord -> newWord <> oldWord
                | _ -> true
            unionDefinition <- result
            if changed then
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
        |> Async.StartImmediate

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    200us, updateAtCaretPosition)

    let shouldGenerateUnionTypeCases (matchExpr: MatchExpr) (entity: FSharpEntity) =
        let caseCount = entity.UnionCases.Count
        let writtenCaseCount = matchExpr.FieldExprList.Length
        caseCount > 0 && writtenCaseCount < caseCount

    let handleGenerateUnionTypeCase (snapshot: ITextSnapshot) (matchExpr: MatchExpr) (insertionPos: _) entity = 
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let fieldsWritten = matchExpr.FieldExprList

        use transaction = textUndoHistory.CreateTransaction(CommandName)

        let stub = UnionTypeCaseGenerator.formatRecord
                       insertionPos
                       indentSize
                       "failwith \"Uninitialized field\""
                       entity
                       fieldsWritten
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.Position.Line-1).Start.Position + insertionPos.Position.Column

        buffer.Insert(currentLine, stub) |> ignore

        transaction.Complete()

    let generateUnionTypeCase snapshot recordExpr insertionPos entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = CommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = handleGenerateUnionTypeCase snapshot recordExpr insertionPos entity }

    member x.GetSmartTagActions(snapshot, expression, insertionPos, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        actionList.Add(generateUnionTypeCase snapshot expression insertionPos entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<UnionMatchCaseGeneratorSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<UnionMatchCaseGeneratorSmartTag> seq =
            seq {
                match unionDefinition with
                | Some (word, expression, entity, insertionPos) when shouldGenerateUnionTypeCases expression entity ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, 
                                     UnionMatchCaseGeneratorSmartTag(x.GetSmartTagActions(word.Snapshot, expression, insertionPos, entity)))
                          :> ITagSpan<_>
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish