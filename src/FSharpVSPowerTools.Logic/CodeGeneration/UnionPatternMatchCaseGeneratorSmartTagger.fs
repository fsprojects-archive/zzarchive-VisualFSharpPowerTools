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
open FSharpVSPowerTools.ProjectSystem
open System.Threading

type UnionPatternMatchCaseGeneratorSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type UnionPatternMatchCaseGenerator
        (textDocument: ITextDocument,
         view: ITextView,
         textUndoHistory: ITextUndoHistory,
         vsLanguageService: VSLanguageService,
         serviceProvider: IServiceProvider,
         projectFactory: ProjectFactory,
         defaultBody: string,
         openDocumentTracker: IOpenDocumentsTracker) as self =
    let changed = Event<_>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable suggestions: ISuggestion list = []

    let buffer = view.TextBuffer
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

    let handleGenerateUnionPatternMatchCases
        (snapshot: ITextSnapshot) (patMatchExpr: PatternMatchExpr)
        insertionParams entity = 
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

    let getSuggestions (patMatchExpr, entity, insertionParams, snapshot) =
        [
            { new ISuggestion with
                  member __.Invoke() = handleGenerateUnionPatternMatchCases snapshot patMatchExpr insertionParams entity
                  member __.NeedsIcon = false
                  member __.Text = Resource.unionPatternMatchCaseCommandName }
        ]

    let updateAtCaretPosition (CallInUIContext callInUIContext) =
        async {
            match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
            | Some point, Some word when word.Snapshot = view.TextSnapshot && point.InSpan word -> return ()
            | (Some _ | None), _ ->
                let! result = asyncMaybe {
                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                    let dte = serviceProvider.GetService<SDTE,EnvDTE.DTE>()
                    let! doc = dte.GetCurrentDocument textDocument.FilePath
                    let! project = projectFactory.CreateForDocument buffer doc
                    let! word, _ = vsLanguageService.GetSymbol (point, doc.FullName, project) 
                    
                    do! match currentWord with
                        | None -> Some()
                        | Some oldWord -> 
                            if word <> oldWord then Some()
                            else None

                    currentWord <- Some word
                    suggestions <- []
                    let! source = openDocumentTracker.TryGetDocumentText textDocument.FilePath
                    let vsDocument = VSDocument(source, doc, point.Snapshot)
                    let! symbolRange, patMatchExpr, unionTypeDefinition, insertionPos =
                        tryFindUnionDefinitionFromPos codeGenService project point vsDocument
                    // Recheck cursor position to ensure it's still in new word
                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                    if point.InSpan symbolRange && shouldGenerateUnionPatternMatchCases patMatchExpr unionTypeDefinition then
                        return! Some(patMatchExpr, unionTypeDefinition, insertionPos, word.Snapshot)
                    else
                        return! None
                } 
                suggestions <- result |> Option.map getSuggestions |> Option.getOrElse []
                do! callInUIContext <| fun _ -> changed.Trigger self
        }

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                                      500us, updateAtCaretPosition)

    member __.Changed = changed.Publish
    member __.CurrentWord = 
        currentWord |> Option.map (fun word ->
            if buffer.CurrentSnapshot = word.Snapshot then word
            else word.TranslateTo(buffer.CurrentSnapshot, SpanTrackingMode.EdgeExclusive))
    member __.Suggestions = suggestions

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()

type UnionPatternMatchCaseGeneratorSmartTagger(buffer: ITextBuffer, generator: UnionPatternMatchCaseGenerator) as self =
    let tagsChanged = Event<_,_>()
    do generator.Changed.Add (fun _ -> buffer.TriggerTagsChanged self tagsChanged)
    interface ITagger<UnionPatternMatchCaseGeneratorSmartTag> with
        member __.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<UnionPatternMatchCaseGeneratorSmartTag> seq =
            protectOrDefault (fun _ ->
                seq {
                    match generator.CurrentWord, generator.Suggestions with
                    | None, _
                    | _, [] -> ()
                    | Some word, suggestions ->
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

                        yield TagSpan<_>(word, UnionPatternMatchCaseGeneratorSmartTag actions) :> _
                }) 
                Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (generator :> IDisposable).Dispose()
    