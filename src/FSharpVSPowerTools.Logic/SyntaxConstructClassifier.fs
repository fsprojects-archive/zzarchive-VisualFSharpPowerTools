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

[<NoComparison>]
type private CheckingProject =
    { Options: FSharpProjectOptions
      Checked: bool }

[<NoComparison>]
type private FastStageData =
    { Snapshot: ITextSnapshot
      OpenDeclarations: OpenDeclaration list  
      AllEntities: Map<string, Idents list> option
      Spans: CategorizedColumnSpan[]
      SymbolUses: SymbolUse[]
      SingleSymbolsProjects: CheckingProject list }

[<NoComparison>]
type private FastStage =
    | NoData
    | Updating of snapshot: ITextSnapshot
    | Data of FastStageData 

[<NoComparison>]
type private SlowStageData =
    { Snapshot: ITextSnapshot 
      UnusedSpans: Map<WordSpan, CategorizedColumnSpan> }

[<NoComparison>]
type private SlowStage =
    | NoData
    | Data of SlowStageData

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
    
    let getClassificationType cat =
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
        | _ -> None
        |> Option.map classificationRegistry.GetClassificationType

    let classificationChanged = Event<_,_>()
    let fastState = Atom FastStage.NoData
    let slowState = Atom SlowStage.NoData
    let cancellationToken = Atom None
    
    let getProject() =
        maybe {
            // If there is no backing document, an ITextDocument instance might be null
            let! _ = Option.ofNull textDocument
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            return! projectFactory.CreateForDocument buffer doc }

    // Don't check for unused declarations on generated signatures
    let includeUnusedReferences() = 
        includeUnusedReferences 
        && not (isSignatureExtension(Path.GetExtension(textDocument.FilePath)) 
                && getProject() |> Option.map (fun p -> p.IsForStandaloneScript) |> function Some x -> x | None -> false)

    let getCurrentSnapshot() = if textDocument <> null then Some textDocument.TextBuffer.CurrentSnapshot else None

    let triggerClassificationChanged reason =
        // TextBuffer is null if a solution is closed at this moment
        if textDocument.TextBuffer <> null then
            let currentSnapshot = textDocument.TextBuffer.CurrentSnapshot
            let span = SnapshotSpan(currentSnapshot, 0, currentSnapshot.Length)
            classificationChanged.Trigger(self, ClassificationChangedEventArgs(span))
            debug "[SyntaxConstructClassifier] ClassificationChanged event has been triggered by %s" reason

    let getOpenDeclarations source project ast getTextLineOneBased (pf: Profiler) = async {
        let! entities = pf.TimeAsync "GetAllEntities" <| fun _ ->
            vsLanguageService.GetAllEntities(textDocument.FilePath, source, project)    

        return pf.Time "getOpenDeclarations" <| fun _ ->
            let qualifyOpenDeclarations line endCol idents = 
                let lineStr = getTextLineOneBased (line - 1)
                let tooltip =
                    vsLanguageService.GetOpenDeclarationTooltip(
                        line, endCol, lineStr, Array.toList idents, project, textDocument.FilePath, source)
                    |> Async.RunSynchronously
                match tooltip with
                | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
                | None -> []

            entities
            |> Option.map (fun entities -> 
                 entities 
                 |> Seq.groupBy (fun e -> e.FullName)
                 |> Seq.map (fun (key, es) -> key, es |> Seq.map (fun e -> e.CleanedIdents) |> Seq.toList)
                 |> Map.ofSeq),

            OpenDeclarationGetter.getOpenDeclarations ast entities qualifyOpenDeclarations
    }

    let updateSyntaxConstructClassifiers force =
        let cancelToken = new CancellationTokenSource() 
        cancellationToken.Swap (fun _ -> Some (cancelToken))
        |> Option.iter (fun oldToken -> 
            oldToken.Cancel()
            oldToken.Dispose())

        let snapshot = getCurrentSnapshot()
        let needUpdate =
            match snapshot, force, fastState.Value with
            | None, _, _ -> false
            | _, true, _ -> true
            | _, _, FastStage.NoData -> true
            | Some snapshot, _, FastStage.Updating oldSnapshot -> oldSnapshot <> snapshot
            | Some snapshot, _, FastStage.Data { Snapshot = oldSnapshot } -> oldSnapshot <> snapshot

        snapshot |> Option.iter (fun snapshot -> 
            fastState.Swap (fun _ -> Updating snapshot) |> ignore)
                
        if needUpdate then
            let worker = async {
                match getProject(), getCurrentSnapshot() with
                | Some project, Some snapshot ->
                    debug "[SyntaxConstructClassifier] - Effective update"
                    let pf = Profiler()
                                        
                    let! parseResults = pf.TimeAsync "ParseFileInProject" <| fun _ ->
                        vsLanguageService.ParseFileInProject(textDocument.FilePath, snapshot.GetText(), project)

                    let includeUnusedOpens = 
                        includeUnusedOpens 
                        && not (isSignatureExtension(Path.GetExtension(textDocument.FilePath)) 
                        && project.IsForStandaloneScript)

                    let lexer = vsLanguageService.CreateLexer(snapshot, project.CompilerOptions)

                    let! allSymbolsUses = pf.TimeAsync "GetAllUsesOfAllSymbolsInFile [NO UNUSED]" <| fun _ ->
                        vsLanguageService.GetAllUsesOfAllSymbolsInFile(
                            snapshot, textDocument.FilePath, project, AllowStaleResults.No, includeUnusedOpens, pf)

                    let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()

                    let! entitiesMap, openDeclarations = 
                        if includeUnusedOpens then
                            getOpenDeclarations (snapshot.GetText()) project parseResults.ParseTree getTextLineOneBased pf
                        else async { return None, [] }
                      
                    let spans = pf.Time "getCategoriesAndLocations [FAST STAGE]" <| fun _ ->
                        getCategoriesAndLocations (allSymbolsUses, parseResults.ParseTree, lexer, getTextLineOneBased, openDeclarations, entitiesMap)
                        |> Array.sortBy (fun { WordSpan = { Line = line }} -> line)

                    let spans = 
                        match slowState.Value with
                        | SlowStage.Data { UnusedSpans = oldUnusedSpans } ->
                            spans
                            |> Array.filter (fun s ->
                                not (oldUnusedSpans |> Map.containsKey s.WordSpan))
                            |> Array.append (oldUnusedSpans |> Map.toArray |> Array.map snd)
                        | _ -> spans

                    let spans = spans |> Array.sortBy (fun { WordSpan = { Line = line }} -> line)
                    
                    let! singleSymbolsProjects = async {
                        if includeUnusedReferences() then
                            let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol textDocument.FilePath project
                            let singleDefs = UnusedDeclarations.getSingleDeclarations allSymbolsUses
                            return!
                                singleDefs
                                |> Async.Array.map (fun symbol ->
                                    vsLanguageService.GetSymbolDeclProjects getSymbolDeclLocation project symbol)
                                |> Async.map (
                                       Array.choose id 
                                    >> Array.concat 
                                    >> Seq.distinct 
                                    >> Seq.map (fun opts -> { Options = opts; Checked = false })
                                    >> Seq.toList)
                        else return [] }
                    
                    fastState.Swap (fun _ -> 
                        FastStage.Data 
                            { Snapshot = snapshot
                              Spans = spans
                              SymbolUses = allSymbolsUses
                              SingleSymbolsProjects = singleSymbolsProjects
                              OpenDeclarations = openDeclarations
                              AllEntities = entitiesMap }) |> ignore  

                    triggerClassificationChanged "UpdateSyntaxConstructClassifiers" 
                        
                    singleSymbolsProjects |> List.iter (fun p -> vsLanguageService.StartBackgroundCompile p.Options)
                    pf.Stop()
                    Logging.logInfo "[SyntaxConstructClassifier] %s" pf.Result
                    
                | _ -> () } 
            Async.StartInThreadPoolSafe (worker, cancelToken.Token) 
            
    let updateUnusedDeclarations = 
        asyncMaybe {
            let! project = getProject() |> liftMaybe
            let! snapshot = getCurrentSnapshot() |> liftMaybe
            let! symbolsUses, openDecls, entities =
                match fastState.Value, slowState.Value with
                | (FastStage.NoData | FastStage.Updating _), _ -> None
                | FastStage.Data fastData, slowStage ->
                    match slowStage with
                    | SlowStage.Data { Snapshot = oldSnapshot } when oldSnapshot = snapshot -> None
                    | _ -> Some (fastData.SymbolUses, fastData.OpenDeclarations, fastData.AllEntities)
                |> liftMaybe
            
            debug "[SyntaxConstructClassifier] -> UpdateUnusedDeclarations"
            let pf = Profiler()
            let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol textDocument.FilePath project
            let! symbolsUses = vsLanguageService.GetUnusedDeclarations(symbolsUses, project, getSymbolDeclLocation, pf) |> liftAsync
            let lexer = vsLanguageService.CreateLexer(snapshot, project.CompilerOptions)     
            let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()
            let! parseResults = vsLanguageService.ParseFileInProject(textDocument.FilePath, snapshot.GetText(), project) |> liftAsync
            let spans =
                getCategoriesAndLocations (symbolsUses, parseResults.ParseTree, lexer, getTextLineOneBased, openDecls, entities)
                |> Array.sortBy (fun { WordSpan = { Line = line }} -> line)
            
            let notUsedSpans =
                spans
                |> Array.filter (fun s -> s.Category = Category.Unused)
                |> Array.map (fun s -> s.WordSpan, s)
                |> Map.ofArray
                           
            fastState.Swap (function
                | FastStage.Data data -> FastStage.Data { data with Spans = spans; SingleSymbolsProjects = [] }
                | state -> state) 
                |> ignore
            
            debug "[SyntaxConstructClassifier] UpdateUnusedDeclarations: fastState swapped"

            slowState.Swap (fun _ -> SlowStage.Data { Snapshot = snapshot; UnusedSpans = notUsedSpans }) |> ignore
            debug "[SyntaxConstructClassifier] UpdateUnusedDeclarations: slowState swapped"

            triggerClassificationChanged "UpdateUnusedDeclarations"
        } |> Async.map ignore

    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
    let events: EnvDTE80.Events2 option = tryCast dte.Events
    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun project _ _ _ _ ->
        maybe {
            let! selfProject = getProject()
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

    do if includeUnusedReferences() then
        vsLanguageService.Checker.ProjectChecked.Add(fun projectFileName ->
            match includeUnusedReferences(), fastState.Value with
            | true, FastStage.Data { SingleSymbolsProjects = [] } -> ()
            | true, FastStage.Data ({ SingleSymbolsProjects = projects } as fastData) ->
                let projects =
                    match projects |> List.partition (fun p -> p.Options.ProjectFileName = projectFileName) with
                    | [], rest -> rest
                    | matched, rest ->
                        (matched |> List.map (fun p -> { p with Checked = true })) @ rest
                fastState.Swap (fun _ -> FastStage.Data { fastData with SingleSymbolsProjects = projects }) |> ignore
                if projects |> List.forall (fun p -> p.Checked) then
                    Async.StartInThreadPoolSafe updateUnusedDeclarations
            | _ -> ()
        )

    let getClassificationSpans (newSnapshotSpan: SnapshotSpan) =
        match fastState.Value with
        | FastStage.Data { FastStageData.Snapshot = snapshot; Spans = spans } ->
            // We get additional 10 lines above the current snapshot in case the user inserts some line
            // while we were getting locations from FCS. It's not as reliable though. 
            let spanStartLine = max 0 (newSnapshotSpan.Start.GetContainingLine().LineNumber + 1 - 10)
            let spanEndLine = (newSnapshotSpan.End - 1).GetContainingLine().LineNumber + 1
            let spans =
                spans
                // Locations are sorted, so we can safely filter them efficiently
                |> Seq.skipWhile (fun { WordSpan = { Line = line }} -> line < spanStartLine)
                |> Seq.choose (fun loc -> 
                    maybe {
                        let! clType = getClassificationType loc.Category
                        // Create a span on the original snapshot
                        let! span = fromRange snapshot (loc.WordSpan.ToRange())
                        // Translate the span to the new snapshot
                        return clType, span.TranslateTo(newSnapshotSpan.Snapshot, SpanTrackingMode.EdgeExclusive) 
                    })
                |> Seq.takeWhile (fun (_, span) -> span.Start.GetContainingLine().LineNumber <= spanEndLine)
                |> Seq.map (fun (clType, span) -> ClassificationSpan(span, clType))
                |> Seq.toArray
            spans
        | FastStage.NoData -> 
            // Only schedule an update on signature files
            if isSignatureExtension(Path.GetExtension(textDocument.FilePath)) then
                // If not yet schedule an action, do it now.
                updateSyntaxConstructClassifiers false
            [||]
        | FastStage.Updating _ -> [||]

    interface IClassifier with
        // It's called for each visible line of code
        member __.GetClassificationSpans(snapshotSpan: SnapshotSpan) =
            upcast (protectOrDefault (fun _ -> getClassificationSpans snapshotSpan) [||])

        [<CLIEvent>]
        member __.ClassificationChanged = classificationChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            events |> Option.iter (fun e -> e.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler)
            cancellationToken.Value
            |> Option.iter (fun token -> 
                token.Cancel()
                token.Dispose())
            (docEventListener :> IDisposable).Dispose()
         
