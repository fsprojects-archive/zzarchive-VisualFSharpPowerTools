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
open FSharpVSPowerTools.CodeGeneration.UnionPatternMatchCaseGenerator
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices

type UnionPatternMatchCaseGeneratorSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type UnionPatternMatchCaseGeneratorSmartTagger
        (textDocument: ITextDocument,
         view: ITextView,
         textUndoHistory: ITextUndoHistory,
         vsLanguageService: VSLanguageService,
         serviceProvider: IServiceProvider,
         projectFactory: ProjectFactory,
         defaultBody: string) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable unionDefinition = None

    let buffer = view.TextBuffer
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

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
                    asyncMaybe {
                        let vsDocument = VSDocument(doc, point.Snapshot)
                        let! symbolRange, patMatchExpr, unionTypeDefinition, insertionPos =
                            tryFindUnionDefinitionFromPos codeGenService project point vsDocument
                        let newWord = symbolRange

                        // Recheck cursor position to ensure it's still in new word
                        let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
                        if point.InSpan newWord then
                            return! Some(patMatchExpr, unionTypeDefinition, insertionPos) |> liftMaybe
                        else
                            return! liftMaybe None
                    }
                    |> Async.map (fun result -> 
                        unionDefinition <- result
                        buffer.TriggerTagsChanged self tagsChanged)
                    |> Async.StartImmediateSafe

                    currentWord <- Some newWord
            | _ -> 
                currentWord <- None
                buffer.TriggerTagsChanged self tagsChanged

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    500us, updateAtCaretPosition)

    let handleGenerateUnionPatternMatchCases
        (snapshot: ITextSnapshot) (patMatchExpr: PatternMatchExpr)
        (insertionParams: _) entity = 
        use transaction = textUndoHistory.CreateTransaction(Resource.unionPatternMatchCaseCommandName)

        let stub = UnionPatternMatchCaseGenerator.formatMatchExpr
                       insertionParams
                       defaultBody
                       patMatchExpr
                       entity
        let insertionPos = insertionParams.InsertionPos
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.Line-1).Start.Position + insertionPos.Column

        buffer.Insert(currentLine, stub) |> ignore

        transaction.Complete()

    let generateUnionPatternMatchCase snapshot patMatchExpr insertionPos entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = Resource.unionPatternMatchCaseCommandName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = handleGenerateUnionPatternMatchCases snapshot patMatchExpr insertionPos entity }

    member x.GetSmartTagActions(snapshot, expression, insertionPos, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        actionList.Add(generateUnionPatternMatchCase snapshot expression insertionPos entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<UnionPatternMatchCaseGeneratorSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<UnionPatternMatchCaseGeneratorSmartTag> seq =
            try
                seq [|
                    match currentWord, unionDefinition with
                    | Some word, Some (expression, entity, insertionPos) when shouldGenerateUnionPatternMatchCases expression entity ->
                        let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                        yield TagSpan<_>(span, 
                                         UnionPatternMatchCaseGeneratorSmartTag(x.GetSmartTagActions(word.Snapshot, expression, insertionPos, entity)))
                              :> ITagSpan<_>
                    | _ -> ()
                |]
            with e ->
                Logging.logException e
                Seq.empty

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member x.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
