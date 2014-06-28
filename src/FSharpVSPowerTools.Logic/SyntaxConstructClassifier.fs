namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.IO
open System.Threading
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices

[<NoComparison>]
type private ClassifierState =
    { SnapshotSpan: SnapshotSpan
      Spans: CategorizedColumnSpan [] }

type SyntaxConstructClassifier (doc: ITextDocument, classificationRegistry: IClassificationTypeRegistryService,
                                vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider,
                                projectFactory: ProjectFactory) as self =
    
    let getClassficationType cat =
        match cat with
        | ReferenceType -> Some "FSharp.ReferenceType"
        | ValueType -> Some "FSharp.ValueType"
        | PatternCase -> Some "FSharp.PatternCase"
        | Function -> Some "FSharp.Function"
        | MutableVar -> Some "FSharp.MutableVar"
        | Quotation -> Some "FSharp.Quotation"
        | Module -> Some "FSharp.Module"
        | Unused -> Some "FSharp.Unused"
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

    let isSymbolLocalForProject (symbol: FSharpSymbol) = 
        match symbol with 
        | :? FSharpParameter -> true
        | :? FSharpMemberFunctionOrValue as m -> not m.IsModuleValueOrMember || not m.Accessibility.IsPublic
        | :? FSharpEntity as m -> not m.Accessibility.IsPublic
        | :? FSharpGenericParameter -> true
        | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
        | :? FSharpField as m -> not m.Accessibility.IsPublic
        | _ -> false

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
                            let! allSymbolsUses, lexer =
                                vsLanguageService.GetAllUsesOfAllSymbolsInFile (snapshot, doc.FilePath, project, AllowStaleResults.No)
                            let! parseResults = vsLanguageService.ParseFileInProject(doc.FilePath, snapshot.GetText(), project)

                            let singleDefs = 
                                allSymbolsUses
                                |> Seq.groupBy (fun su -> su.Symbol)
                                |> Seq.choose (fun (sym, uses) -> 
                                    if Seq.length uses = 1 
                                        && (Seq.head uses).IsFromDefinition 
                                        && isSymbolLocalForProject sym then Some sym 
                                    else None)
                                |> Seq.toList

                            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                                    
                            let notUsedSymbols =
                                singleDefs 
                                |> List.choose (fun sym ->
                                    match projectFactory.GetSymbolDeclarationLocation project.IsForStandaloneScript sym dte doc.FilePath with
                                    | Some SymbolDeclarationLocation.File -> Some sym
                                    | Some (SymbolDeclarationLocation.Projects declProjects) ->
                                        //let projectsToCheck = projectFactory.GetDependentProjects dte declProjects
                                        if vsLanguageService.IsSymbolUsedInProjects (sym, project.ProjectFileName, declProjects) 
                                            |> Async.RunSynchronously then None
                                        else Some sym
                                    | _ -> None)
                                    
                            let usedSymbolUses =
                                match notUsedSymbols with
                                | [] -> allSymbolsUses |> Array.map (fun su -> su, true)
                                | _ ->
                                    allSymbolsUses 
                                    |> Array.map (fun su -> 
                                        su, not (notUsedSymbols |> List.exists (fun s -> s = su.Symbol)))

                            let spans = 
                                getCategoriesAndLocations (usedSymbolUses, parseResults.ParseTree, lexer)
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
    
    let docEventListener = new DocumentEventListener ([ViewChange.bufferChangedEvent doc.TextBuffer], 200us, 
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
                        let! clType = getClassficationType loc.Category
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
         
