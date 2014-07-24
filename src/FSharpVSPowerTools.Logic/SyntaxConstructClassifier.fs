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

[<NoComparison>]
type private ClassifierState =
    { SnapshotSpan: SnapshotSpan
      Spans: CategorizedColumnSpan [] }

type SyntaxConstructClassifier (doc: ITextDocument, classificationRegistry: IClassificationTypeRegistryService,
                                vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider,
                                projectFactory: ProjectFactory, includeUnusedDeclarations: bool) as self =
    
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
        | _ -> None
        |> Option.map classificationRegistry.GetClassificationType

    let classificationChanged = Event<_,_>()
    let state = Atom None
    let cancellationToken = Atom None
    
    let getProject() =
        maybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! projectItem = Option.attempt (fun _ -> dte.Solution.FindProjectItem doc.FilePath) |> Option.bind Option.ofNull
            return! projectFactory.CreateForFileInProject doc.TextBuffer doc.FilePath projectItem.ContainingProject }

    let updateSyntaxConstructClassifiers force =
        let cancelToken = new CancellationTokenSource() 
        cancellationToken.Swap (fun _ -> Some (cancelToken))
        |> Option.iter (fun oldToken -> 
            oldToken.Cancel()
            oldToken.Dispose())

        let snapshot = doc.TextBuffer.CurrentSnapshot
        let currentState = state.Value
        let needUpdate =
            match force, currentState with
            | true, _ -> true
            | _, None -> true
            | _, Some state -> state.SnapshotSpan.Snapshot <> snapshot
                 
        if needUpdate then
            match getProject() with
            | Some project ->
                debug "[SyntaxConstructClassifier] - Effective update"
                let worker = 
                    async {
                        try
                            let getSymbolDeclLocation fsSymbol =
                                projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath project                                  

                            let! symbolsUses, lexer =
                                vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, doc.FilePath, project, AllowStaleResults.No,
                                                                                includeUnusedDeclarations, getSymbolDeclLocation)

                            let! parseResults = vsLanguageService.ParseFileInProject(doc.FilePath, snapshot.GetText(), project)
                            let! entities = vsLanguageService.GetAllEntities(doc.FilePath, snapshot.GetText(), project)
                            
                            let openDeclarations = 
                                if includeUnusedDeclarations then
                                    let qualifyOpenDeclarations line endCol idents = 
                                        let lineStr = snapshot.GetLineFromLineNumber(line - 1).GetText()
                                        let tooltip =
                                            vsLanguageService.GetOpenDeclarationTooltip(
                                                            line, endCol, lineStr, Array.toList idents, project, doc.FilePath, snapshot.GetText())
                                            |> Async.RunSynchronously
                                        match tooltip with
                                        | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
                                        | None -> []

                                    OpenDeclarationGetter.getOpenDeclarations parseResults.ParseTree entities qualifyOpenDeclarations
                                else [] 

                            let spans = 
                                getCategoriesAndLocations (symbolsUses, parseResults.ParseTree, lexer, openDeclarations)
                                |> Array.sortBy (fun { WordSpan = { Line = line }} -> line)
                        
                            state.Swap (fun _ -> 
                                Some { SnapshotSpan = SnapshotSpan (snapshot, 0, snapshot.Length)
                                       Spans = spans }) |> ignore

                            // TextBuffer is null if a solution is closed at this moment
                            if doc.TextBuffer <> null then
                                let currentSnapshot = doc.TextBuffer.CurrentSnapshot
                                let span = SnapshotSpan(currentSnapshot, 0, currentSnapshot.Length)
                                classificationChanged.Trigger(self, ClassificationChangedEventArgs(span))
                        with e -> Logging.logException e
                    } 
                Async.Start (worker, cancelToken.Token) 
            | None -> ()

    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
    let events = dte.Events :?> EnvDTE80.Events2 
    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun project _ _ _ _ ->
        maybe {
            let! selfProject = getProject()
            let builtProjectFileName = Path.GetFileName project
            let referencedProjectFileNames = selfProject.GetAllReferencedProjectFileNames()
            if referencedProjectFileNames |> List.exists ((=) builtProjectFileName) then
                debug "[SyntaxConstructClassifier] Referenced project %s has been built, updating classifiers." 
                        builtProjectFileName
                updateSyntaxConstructClassifiers true
        } |> ignore)

    do events.BuildEvents.add_OnBuildProjConfigDone onBuildDoneHandler
    
    let docEventListener = new DocumentEventListener ([ViewChange.bufferEvent doc.TextBuffer], 200us, 
                                    fun() -> updateSyntaxConstructClassifiers false)

    let getClassificationSpans (snapshotSpan: SnapshotSpan) =
        match state.Value with
        | Some state ->
            // we get additional 10 lines above the current snapshot in case the user inserts some line
            // while we were getting locations from FCS. It's not as reliable though. 
            let spanStartLine = max 0 (snapshotSpan.Start.GetContainingLine().LineNumber + 1 - 10)
            let spanEndLine = (snapshotSpan.End - 1).GetContainingLine().LineNumber + 1
            let spans =
                state.Spans
                // locations are sorted, so we can safely filter them efficently
                |> Seq.skipWhile (fun { WordSpan = { Line = line }} -> line < spanStartLine)
                |> Seq.choose (fun loc -> 
                    maybe {
                        let! clType = getClassificationType loc.Category
                        let! span = fromRange state.SnapshotSpan.Snapshot (loc.WordSpan.ToRange())
                        return clType, span.TranslateTo(snapshotSpan.Snapshot, SpanTrackingMode.EdgeExclusive) 
                    })
                |> Seq.takeWhile (fun (_, span) -> span.Start.GetContainingLine().LineNumber <= spanEndLine)
                |> Seq.map (fun (clType, span) -> ClassificationSpan(span, clType))
                |> Seq.toArray
            spans
        | None -> [||]

    interface IClassifier with
        // it's called for each visible line of code
        member x.GetClassificationSpans(snapshotSpan: SnapshotSpan) =
            try getClassificationSpans snapshotSpan :> _
            with e -> 
                Logging.logException e
                upcast [||]

        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish

    interface IDisposable with
        member x.Dispose() = 
            events.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler
            (docEventListener :> IDisposable).Dispose()
         
