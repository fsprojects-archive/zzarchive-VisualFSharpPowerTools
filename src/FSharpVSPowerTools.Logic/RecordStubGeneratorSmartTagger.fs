namespace FSharpVSPowerTools.Refactoring

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell.Interop
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.RecordStubGenerator
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Threading

type RecordStubGeneratorSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type RecordStubGenerator(textDocument: ITextDocument,
                         view: ITextView,
                         textUndoHistory: ITextUndoHistory,
                         vsLanguageService: VSLanguageService,
                         serviceProvider: IServiceProvider,
                         projectFactory: ProjectFactory,
                         defaultBody: string) as self =
    let changed = Event<_>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable suggestion: ISuggestion option = None
    
    let buffer = view.TextBuffer
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

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
                       defaultBody
                       entity
                       fieldsWritten
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.InsertionPos.Line-1).Start.Position + insertionPos.InsertionPos.Column

        buffer.Insert(currentLine, stub) |> ignore

        transaction.Complete()

    let getSuggestion snapshot (recordExpr, entity, insertionParams) =
        { new ISuggestion with
              member __.Invoke() = handleGenerateRecordStub snapshot recordExpr insertionParams entity
              member __.NeedsIcon = false
              member __.Text = Resource.recordGenerationCommandName }

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
                    let! doc = dte.GetCurrentDocument(textDocument.FilePath)
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
                    let uiContext = SynchronizationContext.Current
                    asyncMaybe {
                        let vsDocument = VSDocument(doc, point.Snapshot)
                        let! symbolRange, recordExpression, recordDefinition, insertionPos =
                            tryFindRecordDefinitionFromPos codeGenService project point vsDocument
                        // Recheck cursor position to ensure it's still in new word
                        let! point = buffer.GetSnapshotPoint view.Caret.Position
                        if point.InSpan symbolRange && shouldGenerateRecordStub recordExpression recordDefinition then
                            return! Some (recordExpression, recordDefinition, insertionPos)
                        else
                            return! None
                    }
                    |> Async.bind (fun result -> 
                        async {
                            // Switch back to UI thread before firing events
                            do! Async.SwitchToContext uiContext
                            suggestion <- result |> Option.map (getSuggestion newWord.Snapshot)
                            currentWord <- Some newWord
                            changed.Trigger self
                        })
                    |> Async.StartInThreadPoolSafe
            | _ -> 
                currentWord <- None 
                changed.Trigger self

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                                      500us, updateAtCaretPosition)
    member __.Changed = changed.Publish
    member __.CurrentWord = 
        currentWord |> Option.map (fun word ->
            if buffer.CurrentSnapshot = word.Snapshot then word
            else word.TranslateTo(buffer.CurrentSnapshot, SpanTrackingMode.EdgeExclusive))
    member __.Suggestions = suggestion |> Option.map (fun x -> [x])

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()

type RecordStubGeneratorSmartTagger(buffer: ITextBuffer, generator: RecordStubGenerator) as self =
    let tagsChanged = Event<_,_>()
    do generator.Changed.Add (fun _ -> buffer.TriggerTagsChanged self tagsChanged)
    interface ITagger<RecordStubGeneratorSmartTag> with
        member __.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<RecordStubGeneratorSmartTag> seq =
            protectOrDefault (fun _ ->
                seq {
                    match generator.CurrentWord, generator.Suggestions with
                    | Some word, Some suggestions ->
                        let actions =
                            suggestions
                            |> List.map (fun s ->
                                 { new ISmartTagAction with
                                     member __.ActionSets = null
                                     member __.DisplayText = s.Text
                                     member __.Icon = null
                                     member __.IsEnabled = true
                                     member __.Invoke() = s.Invoke() })
                            |> Seq.toReadOnlyCollection
                            |> fun xs -> [ SmartTagActionSet xs ]
                            |> Seq.toReadOnlyCollection
                        yield TagSpan<_>(word, RecordStubGeneratorSmartTag(actions)) :> _
                    | _ -> () })
                Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (generator :> IDisposable).Dispose()
