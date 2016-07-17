namespace FSharp.Editing.VisualStudio.Coloring.UnusedSymbols

open System
open System.IO
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open FSharp.Editing
open FSharp.Editing.AsyncMaybe
open FSharp.Editing.Features
open FSharp.Editing.UntypedAstUtils
open FSharp.Editing.VisualStudio
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler
open System.Diagnostics
open FSharp.Editing.VisualStudio.Coloring
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.Editing.Features.SourceCodeClassifier

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
    member x.MaybeData = 
        match x with
        | Data data -> Some data
        | Updating (data, _) -> data
        | NoData -> None
    override x.ToString() =
        let formatData data =
            sprintf "(snapshot version = %d, spans count = %d)" data.Snapshot.Version.VersionNumber data.Spans.Spans.Length
        match x with
        | Data data -> sprintf "Data %s" (formatData data)
        | Updating (data, currentSnapshot) -> 
            sprintf "Updating (data = %s, currentSnapshot = %d)" 
                    (match data with Some data -> formatData data | None -> "None") 
                    currentSnapshot.Version.VersionNumber
        | NoData -> "NoData"

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
    let singleSymbolsProjects: Atom<CheckingProject list> = Atom []
    let unusedTasgChanged = Event<_,_>()
    let dte = serviceProvider.GetDte()
    let project() = projectFactory.CreateForDocument buffer doc.FilePath
    
    let isCurrentProjectForStandaloneScript = 
        lazy (project() |> Option.map (fun p -> p.Project.IsForStandaloneScript) |> Option.getOrElse false)

    let includeUnusedOpens() =
        includeUnusedOpens
        // Don't check for unused opens on generated signatures
        && not (isSignatureFile doc.FilePath && isCurrentProjectForStandaloneScript.Value)

    let includeUnusedReferences() =
        includeUnusedReferences
        // Don't check for unused declarations on generated signatures
        && not (isSignatureFile doc.FilePath && isCurrentProjectForStandaloneScript.Value)

    let getCurrentSnapshot() =
        maybe {
            let! doc = Option.ofNull doc
            let! buffer = Option.ofNull doc.TextBuffer
            return buffer.CurrentSnapshot }

    let triggerClassificationChanged snapshot reason =
        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
        classificationChanged.Trigger(self, ClassificationChangedEventArgs span)
        unusedTasgChanged.Trigger(self, SnapshotSpanEventArgs span)
        debug "ClassificationChanged and UnusedTasgChanged events have been triggered by %s" reason

    let getOpenDeclarations filePath project ast getTextLineOneBased = 
        async {
            let! entities = vsLanguageService.GetAllEntities(filePath, project)

            let qualifyOpenDeclarations line endCol idents = 
                async {
                    let lineStr = getTextLineOneBased (line - 1)
                    let! tooltip =
                        vsLanguageService.GetOpenDeclarationTooltip(
                            line, endCol, lineStr, Array.toList idents, project, doc.FilePath)
                    return
                        match tooltip with
                        | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
                        | None -> []
                }
            
            let! openDeclarations = OpenDeclarationGetter.getOpenDeclarations ast entities qualifyOpenDeclarations
            return
                entities
                |> Option.map (fun entities ->
                    entities
                    |> Seq.groupBy (fun e -> e.FullName)
                    |> Seq.map (fun (fullName, entities) -> 
                         fullName,
                         entities
                         |> Seq.map (fun e -> e.CleanedIdents) 
                         |> Seq.toList)
                    |> Dict.ofSeq),
                openDeclarations
        }

    let checkAstIsNotEmpty (ast: ParsedInput) =
        if ast.Range.IsEmpty then
            debug "Empty AST"
            None
        else Some()

    let updateUnusedSymbols (project, snapshot, callInUIContext) =
        asyncMaybe {
            debug "updateUnusedSymbols"

            let! symbolsUses =
                vsLanguageService.GetAllUsesOfAllSymbolsInFile(snapshot, doc.FilePath, project, AllowStaleResults.No, includeUnusedOpens())

            let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath project

            let! symbolsUses =
                if includeUnusedReferences() then
                    vsLanguageService.GetUnusedDeclarations(symbolsUses, project, getSymbolDeclLocation)
                else async.Return symbolsUses
                |> liftAsync

            let! lexer = vsLanguageService.CreateLexer(doc.FilePath, snapshot, project.Project.CompilerOptions)
            let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()
            let! checkResults = vsLanguageService.ParseAndCheckFileInProject(doc.FilePath, project)
            let! ast = checkResults.ParseTree
            do! checkAstIsNotEmpty ast

            let! entities, openDecls =
                if includeUnusedOpens() then
                    getOpenDeclarations doc.FilePath project checkResults.ParseTree getTextLineOneBased
                else async.Return (None, [])
                |> liftAsync

            let spans =
                getCategorizedSpansForUnusedSymbols (symbolsUses, checkResults, lexer, openDecls, entities)
                |> Array.sortBy (fun x -> x.WordSpan.Line)
                |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))

            let spans = { Spans = spans; Errors = checkResults.Errors }
            state.Swap (fun _ -> State.Data { Snapshot = snapshot; Spans = spans }) |> ignore
            do! liftAsync (callInUIContext (fun _ -> triggerClassificationChanged snapshot "UpdateUnusedDeclarations"))
        } |> Async.Ignore

    let updateUnusedSymbolsIfNeeded (CallInUIContext callInUIContext) =
        match project(), getCurrentSnapshot() with
        | Some project, Some snapshot ->
            match state.Value with
            | State.Updating (_, oldSnapshot) when oldSnapshot = snapshot -> async.Return()
            | State.Data data when data.Snapshot = snapshot -> async.Return()
            | _ ->
                let oldState = state.Swap (fun x -> State.Updating (x.MaybeData, snapshot))
                log <| fun _ -> sprintf "UpdateUnusedSymbolsIfNeeded, old state = %O" oldState
                async {
                    try 
                        do! updateUnusedSymbols (project, snapshot, callInUIContext)
                    finally
                        // if async updating failed, we should revert state to the previous
                        state.Swap (function Updating _ -> oldState | x -> x) |> ignore
                }
        | _ -> async.Return()

    let onBufferChanged force callInUIContext = 
        let snapshot = getCurrentSnapshot()
        
        let needUpdate =
            match snapshot, force, state.Value with
            | None, _, _ -> false
            | _, true, _ -> true
            | _, _, State.NoData -> true
            | Some snapshot, _, State.Updating (_, oldSnapshot) -> oldSnapshot <> snapshot
            | Some snapshot, _, State.Data { Snapshot = oldSnapshot } -> oldSnapshot <> snapshot

        log <| fun _ -> sprintf "OnBufferChanged, needUpdate = %b" needUpdate

        if needUpdate then
            asyncMaybe {
                let! currentProject = project()
                
                if currentProject.Project.IsForStandaloneScript || not (includeUnusedReferences()) then
                    do! updateUnusedSymbolsIfNeeded callInUIContext |> liftAsync
                else
                    let! snapshot = snapshot
                    let! allSymbolsUses =
                        vsLanguageService.GetAllUsesOfAllSymbolsInFile(snapshot, doc.FilePath, currentProject, AllowStaleResults.No, false)
                    
                    let! singleSymbolsProjs =
                        async {
                            let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath currentProject
                            
                            let! singleDeclarationProjectOpts =
                                UnusedDeclarations.getSingleDeclarations allSymbolsUses
                                |> Async.Array.map (vsLanguageService.GetSymbolDeclProjects getSymbolDeclLocation currentProject)
                                |> Async.map (Array.choose id >> Array.concat >> Array.distinct)
                                
                            return
                                singleDeclarationProjectOpts
                                |> Array.map (fun opts ->
                                   { Options = opts
                                     // We mark standalone FSX's fake project as already checked because otherwise we never complete.
                                     Checked = currentProject.Project.IsForStandaloneScript })
                                |> Array.toList
                        } |> liftAsync
                    
                    singleSymbolsProjects.Swap (fun _ -> singleSymbolsProjs) |> ignore
                    let! currentProjectOpts = vsLanguageService.GetProjectCheckerOptions currentProject |> liftAsync
                    vsLanguageService.CheckProjectInBackground currentProjectOpts
            } |> Async.Ignore
        else async.Return ()

    let events: EnvDTE80.Events2 option = tryCast dte.Events
    
    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun p _ _ _ _ ->
        maybe {
            let! selfProject = project()
            let builtProjectFileName = Path.GetFileName p
            let referencedProjectFileNames = selfProject.GetAllReferencedProjectFileNames()
            if referencedProjectFileNames |> List.exists ((=) builtProjectFileName) then
                debug "Referenced project %s has been built, updating classifiers..." builtProjectFileName
                onBufferChanged true (CallInUIContext.FromCurrentThread()) |> Async.StartInThreadPoolSafe
        } |> ignore)

    do events |> Option.iter (fun e -> e.BuildEvents.add_OnBuildProjConfigDone onBuildDoneHandler)
    let docEventListener = new DocumentEventListener ([ViewChange.bufferEvent doc.TextBuffer], 100us, onBufferChanged false)

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
                    // there is at least one yet unchecked project, start compiling it
                    vsLanguageService.CheckProjectInBackground opts
                | None ->
                    // all the needed projects have been checked in background, let's calculate unused symbols
                    updateUnusedSymbolsIfNeeded (CallInUIContext.FromCurrentThread()) |> Async.StartInThreadPoolSafe
            ))
        else None

    let getClassificationSpans (targetSnapshotSpan: SnapshotSpan) =
        match state.Value with
        | State.Data { Data.Spans = spans }
        | State.Updating (Some { Data.Spans = spans }, _) ->
            getClassificationSpans spans targetSnapshotSpan classificationRegistry
        | State.NoData ->
            // Only schedule an update on signature files
            if isSignatureFile doc.FilePath then
                // If not yet schedule an action, do it now.
                onBufferChanged false (CallInUIContext.FromCurrentThread()) |> Async.StartInThreadPoolSafe
            [||]
        | State.Updating (None, _) -> [||]

    interface IClassifier with
        // It's called for each visible line of code
        member __.GetClassificationSpans span =
            upcast (protectOrDefault (fun _ -> getClassificationSpans span) [||])

        [<CLIEvent>] member __.ClassificationChanged = classificationChanged.Publish

    interface ITagger<UnusedDeclarationTag> with
        member __.GetTags spans =
            let getTags (_spans: NormalizedSnapshotSpanCollection) =
                state.Value.MaybeData
                |> Option.map (fun data ->
                    data.Spans.Spans
                    |> Array.choose (fun wordSpan ->
                        fromRange data.Snapshot wordSpan.ColumnSpan.WordSpan.Range.Value
                        |> Option.map (fun span -> TagSpan(span, UnusedDeclarationTag()) :> ITagSpan<_>)))
                |> Option.getOrElse [||]

            protectOrDefault (fun _ -> getTags spans :> _) Seq.empty

        [<CLIEvent>] member __.TagsChanged = unusedTasgChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            projectCheckedSubscription |> Option.iter (fun sub -> sub.Dispose())
            events |> Option.iter (fun e -> e.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler)
            dispose docEventListener