namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.IO
open System.Threading
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.AsyncMaybe
open Microsoft.VisualStudio.Text.Tagging 
open FSharpVSPowerTools.UntypedAstUtils
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler

[<NoComparison>]
type private CheckingProject =
    { Options: FSharpProjectOptions
      Checked: bool }

[<NoComparison>]
type private SnapshotSpanWithLine =
    { Span: SnapshotSpan 
      Line: int }

[<Sealed>]
type private CategorizedSnapshotSpan (columnSpan: CategorizedColumnSpan<ITextSnapshot>, originalSnapshot: ITextSnapshot) =
    let snapshotSpan: SnapshotSpanWithLine option Atom = Atom None 
    member __.ColumnSpan = columnSpan
    member __.CachedSnapshotSpan = snapshotSpan.Value
    member __.GetSnapshotSpan targetSnapshot =
        snapshotSpan.Swap (fun oldSpan ->
            oldSpan
            |> Option.orTry (fun _ -> 
                fromRange originalSnapshot (columnSpan.WordSpan.ToRange())
                |> Option.map (fun span -> 
                    { Span = span
                      Line = span.Start.GetContainingLine().LineNumber }))
            |> Option.map (fun span ->
                if span.Span.Snapshot <> targetSnapshot then
                    let newSpan = span.Span.TranslateTo(targetSnapshot, SpanTrackingMode.EdgeExclusive)
                    { Span = newSpan; Line = newSpan.Start.GetContainingLine().LineNumber }
                else span)) 
        |> ignore
        snapshotSpan.Value

[<NoComparison>]
type private FastStageData =
    { Snapshot: ITextSnapshot
      Spans: CategorizedSnapshotSpan[]
      SingleSymbolsProjects: CheckingProject list } 

[<NoComparison>]
type private FastStage =
    | NoData
    | Updating of oldData:FastStageData option * currentSnapshot: ITextSnapshot
    | Data of FastStageData

[<NoComparison>]
type private SlowStageData =
    { Snapshot: ITextSnapshot
      UnusedSpans: Map<WordSpan, CategorizedSnapshotSpan>
      IsUpdating: bool }

[<NoComparison>]
type private SlowStage =
    | NoData of isUpdating: bool
    | Data of SlowStageData

type UnusedDeclarationTag() =
    interface ITag

type SyntaxConstructClassifier
    (
        textDocument: ITextDocument,
        buffer: ITextBuffer,
        classificationRegistry: IClassificationTypeRegistryService,
        vsLanguageService: VSLanguageService,
        serviceProvider: IServiceProvider,
        projectFactory: ProjectFactory,
        includeUnusedReferences: bool,
        includeUnusedOpens: bool
    ) as self =

    let getClassificationType = memoize <| fun cat ->
        match cat with
        | Category.ReferenceType -> Some Constants.fsharpReferenceType
        | Category.ValueType -> Some Constants.fsharpValueType
        | Category.PatternCase -> Some Constants.fsharpPatternCase
        | Category.Function -> Some Constants.fsharpFunction
        | Category.MutableVar -> Some Constants.fsharpMutableVar
        | Category.Quotation -> Some Constants.fsharpQuotation
        | Category.Module -> Some Constants.fsharpModule
        | Category.Unused -> Some Constants.fsharpUnused
        | Category.Printf -> Some Constants.fsharpPrintf
        | Category.Escaped -> Some Constants.fsharpEscaped
        | Category.Operator -> Some Constants.fsharpOperator
        | _ -> None
        |> Option.map classificationRegistry.GetClassificationType

    let classificationChanged = Event<_,_>()
    let fastState = Atom FastStage.NoData
    let slowState = Atom (SlowStage.NoData false)
    let fastStageCancellationToken = Atom None
    let slowStageCancellationToken = Atom None

    let unusedDeclarationChanged = Event<_,_>()
    let unusedDeclarationState = Atom None

    let newCancellationToken (currentToken: Atom<CancellationTokenSource option>) =
        let newToken = new CancellationTokenSource()
        currentToken.Swap (fun _ -> Some newToken)
        |> Option.iter (fun oldToken ->
            oldToken.Cancel()
            oldToken.Dispose())
        |> ignore
        newToken

    let disposeCancellationToken (currentToken: Atom<CancellationTokenSource option>) =
        currentToken.Value
        |> Option.iter (fun token ->
            token.Cancel()
            token.Dispose())

    let getCurrentProject() =
        maybe {
            // If there is no backing document, an ITextDocument instance might be null
            let! _ = Option.ofNull textDocument
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            return! projectFactory.CreateForDocument buffer doc }

    let isCurrentProjectForStandaloneScript() =
        getCurrentProject() |> Option.map (fun p -> p.IsForStandaloneScript) |> Option.getOrElse false

    let includeUnusedOpens() =
        includeUnusedOpens
        // Don't check for unused opens on generated signatures
        && not (isSignatureExtension(Path.GetExtension textDocument.FilePath)
                && isCurrentProjectForStandaloneScript())

    let includeUnusedReferences() =
        includeUnusedReferences
        // Don't check for unused declarations on generated signatures
        && not (isSignatureExtension(Path.GetExtension textDocument.FilePath)
                && isCurrentProjectForStandaloneScript())

    let isSlowStageEnabled() = includeUnusedOpens() || includeUnusedReferences()
    let getCurrentSnapshot() =
        maybe {
            let! doc = Option.ofNull textDocument
            let! buffer = Option.ofNull doc.TextBuffer
            return buffer.CurrentSnapshot }

    let triggerClassificationChanged snapshot reason =
        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
        classificationChanged.Trigger(self, ClassificationChangedEventArgs span)
        debug "[SyntaxConstructClassifier] ClassificationChanged event has been triggered by %s" reason

    let triggerUnusedDeclarationChanged snapshot =
        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
        unusedDeclarationChanged.Trigger(self, SnapshotSpanEventArgs span)

    let getOpenDeclarations filePath project ast getTextLineOneBased (pf: Profiler) = 
        async {
            let! entities = pf.TimeAsync "GetAllEntities" <| fun _ ->
                vsLanguageService.GetAllEntities(filePath, project)
            
            return! pf.TimeAsync "getOpenDeclarations" <| fun _ -> 
                async {
                  let qualifyOpenDeclarations line endCol idents = async {
                      let lineStr = getTextLineOneBased (line - 1)
                      let! tooltip =
                          vsLanguageService.GetOpenDeclarationTooltip(
                              line, endCol, lineStr, Array.toList idents, project, textDocument.FilePath)
                      return
                          match tooltip with
                          | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
                          | None -> []
                  }
                  
                  let! openDecls = OpenDeclarationGetter.getOpenDeclarations ast entities qualifyOpenDeclarations
                  return
                      (entities
                       |> Option.map
                           (Seq.groupBy (fun e -> e.FullName)
                            >> Seq.map (fun (key, es) -> key, es |> Seq.map (fun e -> e.CleanedIdents) |> Seq.toList)
                            >> Dict.ofSeq),
                      openDecls)
                }
        }

    let uiContext = SynchronizationContext.Current

    let checkAst message (ast: ParsedInput) =
        if ast.Range.IsEmpty then
            debug "[SyntaxConstructClassifier] %s Empty AST" message
            None
        else Some()

    let isTypeCheckerCategory = function
        | Category.ReferenceType
        | Category.ValueType
        | Category.PatternCase
        | Category.Function
        | Category.MutableVar
        | Category.Module -> true
        | Category.Quotation
        | Category.Unused
        | Category.Printf
        | Category.Escaped
        | Category.Operator
        | Category.Other -> false

    let mergeSpans (oldSpans: CategorizedSnapshotSpan[]) (newSpans: CategorizedSnapshotSpan[]) =
        let getLineRange includingUnused (spans: CategorizedSnapshotSpan[]) =
            let typeCheckerSpans = 
                spans 
                |> Array.filter (fun x -> 
                    isTypeCheckerCategory x.ColumnSpan.Category
                    || (includingUnused && x.ColumnSpan.Category = Category.Unused))
            typeCheckerSpans,
            typeCheckerSpans
            |> Array.map (fun x -> x.ColumnSpan.WordSpan.Line)
            |> function [||] -> -1, -1 | lines -> Array.min lines, Array.max lines

        // we take into account new Unused spans, but do not old ones.
        let newTcSpans, (newStartLine, newEndLine) = getLineRange true newSpans
        let oldTcSpans, (oldStartLine, oldEndLine) = getLineRange false oldSpans
        let isNewRangeLarger = newStartLine <= oldStartLine && newEndLine >= oldEndLine

        // returns `true` if both first and last spans are still here, which means
        // that new spans are not produced from partially valid source file.
        let haveFirstAndLastSpansNotChanged() =
            let sameWordSpan x y =
                x.SymbolKind = y.SymbolKind
                && x.StartCol = y.StartCol
                && x.EndCol = y.EndCol
            match newTcSpans, oldTcSpans with
            | [||], [||] -> true
            | _, [||] | [||], _ -> false
            | x, y ->
                sameWordSpan x.[0].ColumnSpan.WordSpan y.[0].ColumnSpan.WordSpan
                && sameWordSpan x.[x.Length - 1].ColumnSpan.WordSpan y.[y.Length - 1].ColumnSpan.WordSpan

        if isNewRangeLarger then
            debug "[SyntaxConstructClassifier] Replace spans entirely because new span range are wider than old one (old lines = %d..%d, new lines = %d..%d)" 
                  oldStartLine oldEndLine newStartLine newEndLine
            Logging.logInfo (fun _ -> 
                sprintf "[SyntaxConstructClassifier] Replace spans entirely because new span range are wider than old one (old lines = %d..%d, new lines = %d..%d)." 
                        oldStartLine oldEndLine newStartLine newEndLine)
            newSpans
        elif haveFirstAndLastSpansNotChanged() then
            debug "[SyntaxConstructClassifier] Replace spans entirely because first and last spans have not changed."
            Logging.logInfo (fun _ -> "[SyntaxConstructClassifier] Replace spans entirely because first and last spans have not changed.")
            newSpans
        else
            debug "[SyntaxConstructClassifier] Merging spans (new range %A < old range %A)."
                  (newStartLine, newEndLine) (oldStartLine, oldEndLine)
            Logging.logInfo (fun _ ->
                sprintf "[SyntaxConstructClassifier] Merging spans (new range %A < old range %A)."
                        (newStartLine, newEndLine) (oldStartLine, oldEndLine))
            seq { 
                yield! oldSpans |> Seq.takeWhile (fun x -> x.ColumnSpan.WordSpan.Line < newStartLine)
                yield! oldSpans |> Seq.skipWhile (fun x -> x.ColumnSpan.WordSpan.Line <= newEndLine) 
                yield! newSpans
            }
            |> Seq.sortBy (fun x -> x.ColumnSpan.WordSpan.Line)
            |> Seq.toArray
            
    let updateUnusedDeclarations() =
        let worker (project, snapshot) =
            asyncMaybe {
                let pf = Profiler()
                debug "[SyntaxConstructClassifier] -> UpdateUnusedDeclarations"

                let! symbolsUses = pf.TimeAsync "GetAllUsesOfAllSymbolsInFile" <| fun _ ->
                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(
                        snapshot, textDocument.FilePath, project, AllowStaleResults.No, includeUnusedOpens(), pf)

                let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol textDocument.FilePath project

                let! symbolsUses =
                    if includeUnusedReferences() then
                        vsLanguageService.GetUnusedDeclarations(symbolsUses, project, getSymbolDeclLocation, pf)
                    else async { return symbolsUses }
                    |> liftAsync

                let! lexer = vsLanguageService.CreateLexer(textDocument.FilePath, snapshot, project.CompilerOptions)
                let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()

                let! checkResults = pf.Time "parseFileInProject" <| fun _ ->
                    vsLanguageService.ParseAndCheckFileInProject(textDocument.FilePath, project)
                     
                let! ast = checkResults.GetUntypedAst()
                do! checkAst "Slow stage" ast

                let! entities, openDecls =
                    if includeUnusedOpens() then
                        getOpenDeclarations textDocument.FilePath project (checkResults.GetUntypedAst()) getTextLineOneBased pf
                    else async { return None, [] }
                    |> liftAsync

                let spans = pf.Time "getCategoriesAndLocations" <| fun _ ->
                    getCategoriesAndLocations (symbolsUses, checkResults, lexer, getTextLineOneBased, openDecls, entities)
                    |> Array.sortBy (fun x -> x.WordSpan.Line)
                    |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))

                let notUsedSpans =
                    spans
                    |> Array.filterMap
                        (fun x -> x.ColumnSpan.Category = Category.Unused)
                        (fun x -> x.ColumnSpan.WordSpan, x)
                    |> Map.ofArray

                fastState.Swap (function
                    | FastStage.Data data ->
                        FastStage.Data { data with Snapshot = snapshot
                                                   Spans = mergeSpans data.Spans spans
                                                   SingleSymbolsProjects = [] }
                    | state -> state)
                    |> ignore

                debug "[SyntaxConstructClassifier] UpdateUnusedDeclarations: fastState swapped"
                slowState.Swap (fun _ -> SlowStage.Data { Snapshot = snapshot; UnusedSpans = notUsedSpans; IsUpdating = false }) |> ignore
                debug "[SyntaxConstructClassifier] UpdateUnusedDeclarations: slowState swapped"
                pf.Stop()
                Logging.logInfo (fun _ -> sprintf "[SyntaxConstructClassifier] [Slow stage] %s" pf.Result)
                triggerClassificationChanged snapshot "UpdateUnusedDeclarations"

                // Switch back to UI thread before firing events
                do! Async.SwitchToContext(uiContext) |> liftAsync
                unusedDeclarationState.Swap(fun _ -> Some (snapshot, notUsedSpans |> Map.toArray |> Array.map fst)) |> ignore
                triggerUnusedDeclarationChanged snapshot
            } 
            |> Async.map (fun _ ->
                // no matter what's happend in `worker`, we should reset `IsUpdating` flag to `false`
                // in order to prevent Slow stage to stop working as it would think that a previous 
                // `worker` is still running.
                 slowState.Swap (function
                    | SlowStage.NoData _ -> SlowStage.NoData false
                    | SlowStage.Data x -> SlowStage.Data { x with IsUpdating = false })
                |> ignore)

        match getCurrentProject(), getCurrentSnapshot() with
        | Some project, Some snapshot ->
            match fastState.Value, slowState.Value with
            | (FastStage.NoData | FastStage.Updating _), _ -> ()
            | _, SlowStage.NoData (isUpdating = true) -> ()
            | FastStage.Data _, slowStage ->
                match slowStage with
                | SlowStage.Data { IsUpdating = true } -> ()
                | SlowStage.Data { Snapshot = oldSnapshot } when oldSnapshot = snapshot -> ()
                | SlowStage.NoData (isUpdating = true) -> ()
                | _ ->
                    let cancelToken = newCancellationToken slowStageCancellationToken
                    slowState.Swap (function
                        | SlowStage.Data data -> SlowStage.Data { data with IsUpdating = true }
                        | SlowStage.NoData _ -> SlowStage.NoData true) |> ignore
                    Async.StartInThreadPoolSafe(worker (project, snapshot), cancelToken.Token)
        | _ -> ()

    let updateSyntaxConstructClassifiers force = 
        let cancelToken = newCancellationToken fastStageCancellationToken
        let snapshot = getCurrentSnapshot()
        let needUpdate =
            match snapshot, force, fastState.Value with
            | None, _, _ -> false
            | _, true, _ -> true
            | _, _, FastStage.NoData -> true
            | Some snapshot, _, FastStage.Updating (_, oldSnapshot) -> oldSnapshot <> snapshot
            | Some snapshot, _, FastStage.Data { Snapshot = oldSnapshot } -> oldSnapshot <> snapshot

        snapshot |> Option.iter (fun snapshot ->
            fastState.Swap (fun oldState ->
                let oldData =
                    match oldState with
                    | FastStage.Data data -> Some data
                    | FastStage.Updating (data, _) -> data
                    | _ -> None
                Updating (oldData, snapshot)) |> ignore)

        if needUpdate then
            let worker = asyncMaybe {
                let! currentProject = getCurrentProject()
                let! snapshot = snapshot
                debug "[SyntaxConstructClassifier] - Effective update"
                let pf = Profiler()

                let! checkResults = pf.TimeAsync "ParseFileInProject" <| fun _ ->
                    vsLanguageService.ParseAndCheckFileInProject(textDocument.FilePath, currentProject)

                let! ast = checkResults.GetUntypedAst()
                do! checkAst "Fast stage" ast
                let! lexer = vsLanguageService.CreateLexer(textDocument.FilePath, snapshot, currentProject.CompilerOptions)

                let! allSymbolsUses = pf.TimeAsync "GetAllUsesOfAllSymbolsInFile" <| fun _ ->
                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(
                        snapshot, textDocument.FilePath, currentProject, AllowStaleResults.No, false, pf)

                let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()

                let spans = pf.Time "getCategoriesAndLocations" <| fun _ ->
                    getCategoriesAndLocations (allSymbolsUses, checkResults, lexer, getTextLineOneBased, [], None)
                    |> Array.sortBy (fun x -> x.WordSpan.Line)
                    |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))

                let spans =
                    match slowState.Value with
                    | SlowStage.Data { UnusedSpans = oldUnusedSpans } ->
                        spans
                        |> Array.filter (fun s ->
                            not (oldUnusedSpans |> Map.containsKey s.ColumnSpan.WordSpan))
                        |> Array.append (oldUnusedSpans |> Map.toArray |> Array.map snd)
                        |> Array.sortBy (fun x -> x.ColumnSpan.WordSpan.Line)
                    | _ -> spans

                let! singleSymbolsProjects =
                    async {
                        if includeUnusedReferences() then
                            let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol textDocument.FilePath currentProject
                            let singleDefs = UnusedDeclarations.getSingleDeclarations allSymbolsUses
                            return!
                                singleDefs
                                |> Async.Array.map (fun symbol ->
                                     vsLanguageService.GetSymbolDeclProjects getSymbolDeclLocation currentProject symbol)
                                |> Async.map (
                                       Array.choose id
                                    >> Array.concat
                                    >> Array.distinct
                                    >> Array.map (fun opts ->
                                        { Options = opts
                                          // we mark standalone FSX's fake project as already checked
                                          // because otherwise the slow stage never completes
                                          Checked = currentProject.IsForStandaloneScript })
                                    >> Array.toList)
                        else return [] } |> liftAsync

                fastState.Swap (fun oldState ->
                    let spans =
                        match oldState with
                        | FastStage.Data oldData
                        | FastStage.Updating (Some oldData, _) -> mergeSpans oldData.Spans spans
                        | _ -> spans

                    FastStage.Data
                        { Snapshot = snapshot
                          Spans = spans
                          SingleSymbolsProjects = singleSymbolsProjects }) |> ignore

                triggerClassificationChanged snapshot "UpdateSyntaxConstructClassifiers"

                if isSlowStageEnabled() then
                    if currentProject.IsForStandaloneScript || not (includeUnusedReferences()) then
                        updateUnusedDeclarations()
                    else
                        let! currentProjectOpts = vsLanguageService.GetProjectCheckerOptions currentProject |> liftAsync
                        vsLanguageService.CheckProjectInBackground currentProjectOpts

                pf.Stop()
                Logging.logInfo (fun _ -> sprintf "[SyntaxConstructClassifier] [Fast stage] %s" pf.Result)
            }
            Async.StartInThreadPoolSafe (Async.Ignore worker, cancelToken.Token)

    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun project _ _ _ _ ->
        maybe {
            let! selfProject = getCurrentProject()
            let builtProjectFileName = Path.GetFileName project
            let referencedProjectFileNames = selfProject.GetAllReferencedProjectFileNames()
            if referencedProjectFileNames |> List.exists ((=) builtProjectFileName) then
                debug "[SyntaxConstructClassifier] Referenced project %s has been built, updating classifiers..."
                        builtProjectFileName
                updateSyntaxConstructClassifiers true
        } |> ignore)

    do events |> Option.iter (fun e -> e.BuildEvents.add_OnBuildProjConfigDone onBuildDoneHandler)

    let docEventListener =
        new DocumentEventListener ([ViewChange.bufferEvent textDocument.TextBuffer], 200us, fun _ -> updateSyntaxConstructClassifiers false)

    let projectCheckedSubscription =
        // project check results needed for Unused Declarations only.
        if includeUnusedReferences() then
            Some (vsLanguageService.RawChecker.ProjectChecked.Subscribe (fun projectFileName ->
                match isSlowStageEnabled(), fastState.Value with
                | true, FastStage.Data ({ SingleSymbolsProjects = projects } as fastData) ->
                    let projects =
                        match projects |> List.partition (fun p -> p.Options.ProjectFileName = projectFileName) with
                        | [], rest -> rest
                        | matched, rest ->
                            (matched |> List.map (fun p -> { p with Checked = true })) @ rest
                    fastState.Swap (fun _ -> FastStage.Data { fastData with SingleSymbolsProjects = projects }) |> ignore

                    match projects |> List.tryFind (fun p -> not p.Checked) with
                    | Some { Options = opts } ->
                        // there is at least one yet unchecked project, start compilation on it
                        vsLanguageService.CheckProjectInBackground opts
                    | None ->
                        // all the needed projects have been checked in background, let's calculate
                        // Slow Stage (unused symbols and opens)
                        updateUnusedDeclarations()
                | _ -> ()))
        else None

    let getClassificationSpans (targetSnapshotSpan: SnapshotSpan) =
        match fastState.Value with
        | FastStage.Data { FastStageData.Spans = spans }
        | FastStage.Updating (Some { FastStageData.Spans = spans }, _) ->
            let spanStartLine = targetSnapshotSpan.Start.GetContainingLine().LineNumber
            let widenSpanStartLine = max 0 (spanStartLine - 10)
            let spanEndLine = targetSnapshotSpan.End.GetContainingLine().LineNumber
            spans
            // Locations are sorted, so we can safely filter them efficiently.
            // Skip spans that's not are potential candidates for return (we widen the range 
            // because spans may shift to up after translation).
            |> Seq.skipWhile (fun span -> span.ColumnSpan.WordSpan.Line < widenSpanStartLine)
            |> Seq.choose (fun snapshotSpan ->
                maybe {
                    let! clType = getClassificationType snapshotSpan.ColumnSpan.Category
                    let! span = snapshotSpan.GetSnapshotSpan targetSnapshotSpan.Snapshot
                    return clType, span
                })
            |> Seq.takeWhile (fun (_, span) -> span.Line <= spanEndLine)
            // Because we may translate spans above, some of them may not be contained in the requested `SnapshotSpan`.
            |> Seq.filter (fun (_, span) -> targetSnapshotSpan.Contains span.Span)
            |> Seq.map (fun (clType, span) -> ClassificationSpan (span.Span, clType))
            |> Seq.toArray
        | FastStage.NoData ->
            // Only schedule an update on signature files
            if isSignatureExtension(Path.GetExtension(textDocument.FilePath)) then
                // If not yet schedule an action, do it now.
                updateSyntaxConstructClassifiers false
            [||]
        | FastStage.Updating _ -> [||]

    interface IClassifier with
        // It's called for each visible line of code
        member __.GetClassificationSpans span =
            upcast (protectOrDefault (fun _ -> getClassificationSpans span) [||])

        [<CLIEvent>]
        member __.ClassificationChanged = classificationChanged.Publish

    interface ITagger<UnusedDeclarationTag> with
        member __.GetTags spans =
            let getTags (_spans: NormalizedSnapshotSpanCollection) =
                unusedDeclarationState.Value
                |> Option.map (fun (snapshot, data) ->
                    data
                    |> Array.choose (fun wordSpan ->
                        fromRange snapshot (wordSpan.ToRange())
                        |> Option.map (fun span -> TagSpan(span, UnusedDeclarationTag()) :> ITagSpan<_>)))
                |> Option.getOrElse [||]
            protectOrDefault (fun _ -> getTags spans :> _) Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = unusedDeclarationChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            projectCheckedSubscription |> Option.iter (fun sub -> sub.Dispose())
            events |> Option.iter (fun e -> e.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler)
            disposeCancellationToken fastStageCancellationToken
            disposeCancellationToken slowStageCancellationToken
            (docEventListener :> IDisposable).Dispose()
