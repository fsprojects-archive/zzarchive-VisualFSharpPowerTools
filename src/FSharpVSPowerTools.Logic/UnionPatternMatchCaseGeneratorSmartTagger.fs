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
open FSharpVSPowerTools.CodeGeneration.UnionPatternMatchCaseGenerator
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type UnionPatternMatchCaseGeneratorSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type UnionPatternMatchCaseGeneratorSmartTagger(view: ITextView,
                                               buffer: ITextBuffer,
                                               textUndoHistory: ITextUndoHistory,
                                               vsLanguageService: VSLanguageService,
                                               serviceProvider: IServiceProvider,
                                               projectFactory: ProjectFactory) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord = None
    let mutable unionDefinition = None

    let [<Literal>] CommandName = "Generate union pattern match cases"

    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

    let updateAtCaretPosition() =
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetActiveDocument() |> liftMaybe
            let! project = projectFactory.CreateForDocument buffer doc |> liftMaybe
            let vsDocument = VSDocument(doc, point.Snapshot)
            let! symbolRange, patMatchExpr, unionTypeDefinition, insertionPos =
                tryFindUnionDefinitionFromPos codeGenService project point vsDocument
            let newWord = symbolRange

            // Recheck cursor position to ensure it's still in new word
            let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
            if point.InSpan newWord then
                return! Some(newWord, patMatchExpr, unionTypeDefinition, insertionPos)
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
                                    500us, updateAtCaretPosition)

    let handleGenerateUnionPatternMatchCases
        (snapshot: ITextSnapshot) (patMatchExpr: PatternMatchExpr)
        (insertionParams: _) entity = 
        use transaction = textUndoHistory.CreateTransaction(CommandName)

        let stub = UnionPatternMatchCaseGenerator.formatMatchExpr
                       insertionParams
                       "failwith \"Unhandled case\""
                       patMatchExpr
                       entity
        let insertionPos = insertionParams.InsertionPos
        let currentLine = snapshot.GetLineFromLineNumber(insertionPos.Line-1).Start.Position + insertionPos.Column

        buffer.Insert(currentLine, stub) |> ignore

        transaction.Complete()

    let generateUnionPatternMatchCase snapshot patMatchExpr insertionPos entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = CommandName
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
                [|
                    match unionDefinition with
                    | Some (word, expression, entity, insertionPos) when shouldGenerateUnionPatternMatchCases expression entity ->
                        let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                        yield TagSpan<_>(span, 
                                         UnionPatternMatchCaseGeneratorSmartTag(x.GetSmartTagActions(word.Snapshot, expression, insertionPos, entity)))
                              :> ITagSpan<_>
                    | _ -> ()
                |]
                |> Seq.ofArray
            with e ->
                Logging.logException e
                Seq.empty

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish