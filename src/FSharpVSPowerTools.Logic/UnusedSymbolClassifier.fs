namespace FSharpVSPowerTools.SyntaxColoring.UnusedSymbols

open System
open System.IO
open System.Threading
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.AsyncMaybe
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.UntypedAstUtils
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler
open System.Diagnostics
open FSharpVSPowerTools.SyntaxColoring

[<NoComparison>]
type private CheckingProject =
    { Options: FSharpProjectOptions
      Checked: bool }

[<NoComparison>]
type private Data =
    { Snapshot: ITextSnapshot
      Spans: CategorizedSnapshotSpans }

[<NoComparison>]
type private State =
    | NoData
    | Updating of oldData: Data option * currentSnapshot: ITextSnapshot
    | Data of Data

type UnusedDeclarationTag() = interface ITag

type UnusedSymbolClassifier
    (
        doc: ITextDocument,
        buffer: ITextBuffer,
        classificationRegistry: IClassificationTypeRegistryService,
        vsLanguageService: VSLanguageService,
        serviceProvider: IServiceProvider,
        projectFactory: ProjectFactory,
        includeUnusedReferences: bool,
        includeUnusedOpens: bool
    ) as self =

    let typeName = self.GetType().Name
    let log (f: unit -> string) = Logging.logInfo (fun _ -> "[" + typeName + "] " + f()) 
    let debug msg = Printf.kprintf (fun x -> Debug.WriteLine ("[" + typeName + "] " + x)) msg
    let classificationChanged = Event<_,_>()
    let state = Atom State.NoData
    let stageCancellationToken = Atom None
    let singleSymbolsProjects: Atom<CheckingProject list> = Atom []
    let unusedTasgChanged = Event<_,_>()
    let dte = serviceProvider.GetDte()

    let disposeCancellationToken (currentToken: Atom<CancellationTokenSource option>) =
        currentToken.Value
        |> Option.iter (fun token ->
            token.Cancel()
            token.Dispose())

    let getCurrentProject() =
        maybe {
            // If there is no backing document, an ITextDocument instance might be null
            let! _ = Option.ofNull doc
            let! item = dte.GetProjectItem doc.FilePath
            return! projectFactory.CreateForProjectItem buffer doc.FilePath item }

    let isCurrentProjectForStandaloneScript() =
        getCurrentProject() |> Option.map (fun p -> p.IsForStandaloneScript) |> Option.getOrElse false

    let includeUnusedOpens() =
        includeUnusedOpens
        // Don't check for unused opens on generated signatures
        && not (isSignatureExtension(Path.GetExtension doc.FilePath)
                && isCurrentProjectForStandaloneScript())

    let includeUnusedReferences() =
        includeUnusedReferences
        // Don't check for unused declarations on generated signatures
        && not (isSignatureExtension(Path.GetExtension doc.FilePath)
                && isCurrentProjectForStandaloneScript())

    let getCurrentSnapshot() =
        maybe {
            let! doc = Option.ofNull doc
            let! buffer = Option.ofNull doc.TextBuffer
            return buffer.CurrentSnapshot }

    let triggerClassificationChanged snapshot reason =
        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
        classificationChanged.Trigger(self, ClassificationChangedEventArgs span)
        debug "ClassificationChanged event has been triggered by %s" reason

    let triggerUnusedDeclarationChanged snapshot =
        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
        unusedTasgChanged.Trigger(self, SnapshotSpanEventArgs span)

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
                              line, endCol, lineStr, Array.toList idents, project, doc.FilePath)
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
            debug "%s Empty AST" message
            None
        else Some()

    let updateUnusedDeclarations (CallInUIContext callInUIContext) =
        let worker (project, snapshot) =
            asyncMaybe {
                let pf = Profiler()
                debug "UpdateUnusedDeclarations"

                let! symbolsUses = pf.TimeAsync "GetAllUsesOfAllSymbolsInFile" <| fun _ ->
                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(
                        snapshot, doc.FilePath, project, AllowStaleResults.No, includeUnusedOpens(), pf)

                let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath project

                let! symbolsUses =
                    if includeUnusedReferences() then
                        vsLanguageService.GetUnusedDeclarations(symbolsUses, project, getSymbolDeclLocation, pf)
                    else async { return symbolsUses }
                    |> liftAsync

                let! lexer = vsLanguageService.CreateLexer(doc.FilePath, snapshot, project.CompilerOptions)
                let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()

                let! checkResults = pf.Time "ParseAndCheckFileInProject" <| fun _ ->
                    vsLanguageService.ParseAndCheckFileInProject(doc.FilePath, project)
                     
                let! ast = checkResults.ParseTree
                do! checkAst "Slow stage" ast

                let! entities, openDecls =
                    if includeUnusedOpens() then
                        getOpenDeclarations doc.FilePath project checkResults.ParseTree getTextLineOneBased pf
                    else async { return None, [] }
                    |> liftAsync

                let spans = pf.Time "getCategoriesAndLocations" <| fun _ ->
                    getCategoriesAndLocations (symbolsUses, checkResults, lexer, getTextLineOneBased, openDecls, entities)
                    |> Array.sortBy (fun x -> x.WordSpan.Line)
                    |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))
                    |> Array.filter (fun x -> x.ColumnSpan.Category = Category.Unused)

                let spans = { Spans = spans; Errors = checkResults.Errors }

                state.Swap (fun _ ->
                    State.Data 
                        { Snapshot = snapshot
                          Spans = spans })
                |> ignore

                pf.Stop()
                log (fun _ -> sprintf "[Unused symbols and opens stage] %O" pf.Elapsed)
                do! callInUIContext <| fun _ -> triggerClassificationChanged snapshot "UpdateUnusedDeclarations" 
                    |> liftAsync

                // Switch back to UI thread before firing events
                do! Async.SwitchToContext(uiContext) |> liftAsync
                triggerUnusedDeclarationChanged snapshot
            } |> Async.Ignore

        match getCurrentProject(), getCurrentSnapshot() with
        | Some project, Some snapshot ->
            match state.Value with
            | State.Updating _ -> async.Return()
            | State.Data data ->
                match data with
                | { Snapshot = oldSnapshot } when oldSnapshot = snapshot -> async.Return()
                | _ ->
                    state.Swap (function
                        | State.Data oldData -> State.Updating (Some oldData, snapshot)
                        | State.NoData -> State.Updating (None, snapshot)
                        | x -> x) |> ignore
                    async {
                        try do! worker (project, snapshot)
                        finally
                            // no matter what's happend in `worker`, we should reset `IsUpdating` flag to `false`
                            // in order to prevent Slow stage to stop working as it would think that a previous 
                            // `worker` is still running.
                            state.Swap (function
                               | State.NoData -> State.NoData
                               | State.Updating _ -> State.Data data
                               | x -> x)
                            |> ignore
                    }
            | State.NoData -> 
                state.Swap (function
                    | State.Data oldData -> State.Updating (Some oldData, snapshot)
                    | State.NoData -> State.Updating (None, snapshot)
                    | x -> x) |> ignore
                async {
                    try do! worker (project, snapshot)
                    finally
                        // no matter what's happend in `worker`, we should reset `IsUpdating` flag to `false`
                        // in order to prevent Slow stage to stop working as it would think that a previous 
                        // `worker` is still running.
                        state.Swap (function
                           | State.NoData -> State.NoData
                           | State.Updating _ -> State.NoData
                           | x -> x)
                        |> ignore
                }
        | _ -> async.Return()

    let onBufferChanged force ((CallInUIContext callInUIContext) as ciuc) = 
        let snapshot = getCurrentSnapshot()
        let needUpdate =
            match snapshot, force, state.Value with
            | None, _, _ -> false
            | _, true, _ -> true
            | _, _, State.NoData -> true
            | Some snapshot, _, State.Updating (_, oldSnapshot) -> oldSnapshot <> snapshot
            | Some snapshot, _, State.Data { Snapshot = oldSnapshot } -> oldSnapshot <> snapshot

        snapshot |> Option.iter (fun snapshot ->
            state.Swap (fun oldState ->
                let oldData =
                    match oldState with
                    | State.Data data -> Some data
                    | State.Updating (data, _) -> data
                    | _ -> None
                Updating (oldData, snapshot)) |> ignore)

        if needUpdate then
            asyncMaybe {
                let! currentProject = getCurrentProject()
                let! snapshot = snapshot
                debug "Effective update"
                let pf = Profiler()

                let! allSymbolsUses =
                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(
                        snapshot, doc.FilePath, currentProject, AllowStaleResults.No, false, pf)

                let! singleSymbolsProjs =
                    async {
                        let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath currentProject
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
                    } |> liftAsync

                singleSymbolsProjects.Swap (fun _ -> singleSymbolsProjs) |> ignore

                do! callInUIContext <| fun _ -> triggerClassificationChanged snapshot "UpdateSyntaxConstructClassifiers" 
                    |> liftAsync

                if currentProject.IsForStandaloneScript || not (includeUnusedReferences()) then
                    do! updateUnusedDeclarations ciuc |> liftAsync
                else
                    let! currentProjectOpts = vsLanguageService.GetProjectCheckerOptions currentProject |> liftAsync
                    vsLanguageService.CheckProjectInBackground currentProjectOpts

                pf.Stop()
                log (fun _ -> sprintf "[Normal stage] %O elapsed" pf.Elapsed)
            } |> Async.Ignore
        else async.Return ()

    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun project _ _ _ _ ->
        maybe {
            let! selfProject = getCurrentProject()
            let builtProjectFileName = Path.GetFileName project
            let referencedProjectFileNames = selfProject.GetAllReferencedProjectFileNames()
            if referencedProjectFileNames |> List.exists ((=) builtProjectFileName) then
                debug "Referenced project %s has been built, updating classifiers..." builtProjectFileName
                let callInUIContext = CallInUIContext.FromCurrentThread()
                onBufferChanged true callInUIContext |> Async.StartInThreadPoolSafe
        } |> ignore)

    do events |> Option.iter (fun e -> e.BuildEvents.add_OnBuildProjConfigDone onBuildDoneHandler)

    let docEventListener =
        new DocumentEventListener ([ViewChange.bufferEvent doc.TextBuffer], 200us, onBufferChanged false)

    let projectCheckedSubscription =
        // project check results needed for Unused Declarations only.
        if includeUnusedReferences() then
            Some (vsLanguageService.RawChecker.ProjectChecked.Subscribe (fun projectFileName ->
                let projects = singleSymbolsProjects.Value
                let projects =
                    match projects |> List.partition (fun p -> p.Options.ProjectFileName = projectFileName) with
                    | [], rest -> rest
                    | matched, rest ->
                        (matched |> List.map (fun p -> { p with Checked = true })) @ rest
                singleSymbolsProjects.Swap (fun _ -> projects) |> ignore

                match projects |> List.tryFind (fun p -> not p.Checked) with
                | Some { Options = opts } ->
                    // there is at least one yet unchecked project, start compilation on it
                    vsLanguageService.CheckProjectInBackground opts
                | None ->
                    // all the needed projects have been checked in background, let's calculate
                    // Slow Stage (unused symbols and opens)
                    let ctx = CallInUIContext.FromCurrentThread()
                    updateUnusedDeclarations ctx |> Async.StartInThreadPoolSafe
            ))
        else None

    let getClassificationSpans (targetSnapshotSpan: SnapshotSpan) =
        match state.Value with
        | State.Data { Data.Spans = spans }
        | State.Updating (Some { Data.Spans = spans }, _) ->
            let spanStartLine = targetSnapshotSpan.Start.GetContainingLine().LineNumber
            let widenSpanStartLine = max 0 (spanStartLine - 10)
            let spanEndLine = targetSnapshotSpan.End.GetContainingLine().LineNumber
            spans.Spans
            // Locations are sorted, so we can safely filter them efficiently.
            // Skip spans that's not are potential candidates for return (we widen the range 
            // because spans may shift to up after translation).
            |> Seq.skipWhile (fun span -> span.ColumnSpan.WordSpan.Line < widenSpanStartLine)
            |> Seq.choose (fun snapshotSpan ->
                maybe {
                    let! clType = getClassificationType classificationRegistry snapshotSpan.ColumnSpan.Category
                    let! span = snapshotSpan.GetSnapshotSpan targetSnapshotSpan.Snapshot
                    return clType, span
                })
            |> Seq.takeWhile (fun (_, span) -> span.Line <= spanEndLine)
            // Because we may translate spans above, some of them may not be contained in the requested `SnapshotSpan`.
            |> Seq.filter (fun (_, span) -> targetSnapshotSpan.Contains span.Span)
            |> Seq.map (fun (clType, span) -> ClassificationSpan (span.Span, clType))
            |> Seq.toArray
        | State.NoData ->
            // Only schedule an update on signature files
            if isSignatureExtension(Path.GetExtension(doc.FilePath)) then
                // If not yet schedule an action, do it now.
                let callInUIContext = CallInUIContext.FromCurrentThread()
                onBufferChanged false callInUIContext |> Async.StartInThreadPoolSafe
            [||]
        | State.Updating _ -> [||]

    interface IClassifier with
        // It's called for each visible line of code
        member __.GetClassificationSpans span =
            upcast (protectOrDefault (fun _ -> getClassificationSpans span) [||])

        [<CLIEvent>]
        member __.ClassificationChanged = classificationChanged.Publish

    interface ITagger<UnusedDeclarationTag> with
        member __.GetTags spans =
            let getTags (_spans: NormalizedSnapshotSpanCollection) =
                let data = 
                    match state.Value with
                    | Data data -> Some data
                    | Updating (data, _) -> data
                    | NoData -> None

                data
                |> Option.map (fun data ->
                    data.Spans.Spans
                    |> Array.choose (fun wordSpan ->
                        fromRange data.Snapshot (wordSpan.ColumnSpan.WordSpan.ToRange())
                        |> Option.map (fun span -> TagSpan(span, UnusedDeclarationTag()) :> ITagSpan<_>)))
                |> Option.getOrElse [||]

            protectOrDefault (fun _ -> getTags spans :> _) Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = unusedTasgChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            projectCheckedSubscription |> Option.iter (fun sub -> sub.Dispose())
            events |> Option.iter (fun e -> e.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler)
            disposeCancellationToken stageCancellationToken
            (docEventListener :> IDisposable).Dispose()
