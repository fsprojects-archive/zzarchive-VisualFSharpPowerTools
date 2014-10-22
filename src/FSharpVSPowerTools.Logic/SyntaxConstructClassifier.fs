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

type SyntaxConstructClassifier (textDocument: ITextDocument, 
                                buffer: ITextBuffer, 
                                classificationRegistry: IClassificationTypeRegistryService,
                                vsLanguageService: VSLanguageService, 
                                serviceProvider: IServiceProvider,
                                projectFactory: ProjectFactory, 
                                includeUnusedDeclarations: bool) as self =
    
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
        | _ -> None
        |> Option.map classificationRegistry.GetClassificationType

    let classificationChanged = Event<_,_>()
    let state = Atom None
    let cancellationToken = Atom None
    
    let getProject() =
        maybe {
            // If there is no backing document, an ITextDocument instance might be null
            let! _ = Option.ofNull textDocument
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            return! projectFactory.CreateForDocument buffer doc }

    let updateSyntaxConstructClassifiers force =
        let cancelToken = new CancellationTokenSource() 
        cancellationToken.Swap (fun _ -> Some (cancelToken))
        |> Option.iter (fun oldToken -> 
            oldToken.Cancel()
            oldToken.Dispose())

        let snapshot = if textDocument <> null then textDocument.TextBuffer.CurrentSnapshot else null
        let currentState = state.Value
        let needUpdate =
            match force, currentState with
            | true, _ -> true
            | _, None -> true
            | _, Some state -> state.SnapshotSpan.Snapshot <> snapshot
                 
        if needUpdate then
            let worker = 
                async {
                    match getProject() with
                    | Some project ->
                        debug "[SyntaxConstructClassifier] - Effective update"
                        let getSymbolDeclLocation fsSymbol =
                            projectFactory.GetSymbolDeclarationLocation fsSymbol textDocument.FilePath project                                  
                        
                        let getTextLine i = snapshot.GetLineFromLineNumber(i).GetText()
                        
                        let includeUnusedDeclarations = 
                            // Don't check for unused declarations on generated signatures
                            includeUnusedDeclarations && not (String.Equals(Path.GetExtension(textDocument.FilePath), ".fsi", StringComparison.OrdinalIgnoreCase) && project.IsForStandaloneScript)
                        let! symbolsUses, lexer =
                            vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, textDocument.FilePath, project, AllowStaleResults.No,
                                                                            includeUnusedDeclarations, getSymbolDeclLocation)

                        let! parseResults = vsLanguageService.ParseFileInProject(textDocument.FilePath, snapshot.GetText(), project)
                        let! entities = vsLanguageService.GetAllEntities(textDocument.FilePath, snapshot.GetText(), project)
                            
                        let entitiesMap, openDeclarations = 
                            if includeUnusedDeclarations then
                                let qualifyOpenDeclarations line endCol idents = 
                                    let lineStr = snapshot.GetLineFromLineNumber(line - 1).GetText()
                                    let tooltip =
                                        vsLanguageService.GetOpenDeclarationTooltip(
                                                        line, endCol, lineStr, Array.toList idents, project, textDocument.FilePath, snapshot.GetText())
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
                                OpenDeclarationGetter.getOpenDeclarations parseResults.ParseTree entities qualifyOpenDeclarations
                            else None, []
                             
                        let spans = 
                            getCategoriesAndLocations (symbolsUses, parseResults.ParseTree, lexer, getTextLine, openDeclarations, entitiesMap)
                            |> Array.sortBy (fun { WordSpan = { Line = line }} -> line)
                        
                        state.Swap (fun _ -> 
                            Some { SnapshotSpan = SnapshotSpan (snapshot, 0, snapshot.Length);
                                   Spans = spans }) |> ignore

                        // TextBuffer is null if a solution is closed at this moment
                        if textDocument.TextBuffer <> null then
                            let currentSnapshot = textDocument.TextBuffer.CurrentSnapshot
                            let span = SnapshotSpan(currentSnapshot, 0, currentSnapshot.Length)
                            classificationChanged.Trigger(self, ClassificationChangedEventArgs(span))
                    | None -> ()
                } 
            Async.StartInThreadPoolSafe (worker, cancelToken.Token) 
            

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
    
    let docEventListener = new DocumentEventListener ([ViewChange.bufferEvent textDocument.TextBuffer], 200us, 
                                    fun() -> updateSyntaxConstructClassifiers false)

    let getClassificationSpans (snapshotSpan: SnapshotSpan) =
        match state.Value with
        | Some state ->
            // We get additional 10 lines above the current snapshot in case the user inserts some line
            // while we were getting locations from FCS. It's not as reliable though. 
            let spanStartLine = max 0 (snapshotSpan.Start.GetContainingLine().LineNumber + 1 - 10)
            let spanEndLine = (snapshotSpan.End - 1).GetContainingLine().LineNumber + 1
            let spans =
                state.Spans
                // Locations are sorted, so we can safely filter them efficiently
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
        | None -> 
            // Only schedule an update on signature files
            if String.Equals(Path.GetExtension(textDocument.FilePath), ".fsi", StringComparison.OrdinalIgnoreCase) then
                // If not yet schedule an action, do it now.
                updateSyntaxConstructClassifiers false
            [||]

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
         
