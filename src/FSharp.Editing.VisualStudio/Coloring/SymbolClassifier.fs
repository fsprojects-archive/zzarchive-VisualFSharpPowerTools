namespace FSharp.Editing.VisualStudio.Coloring.Symbols

open System
open System.IO
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler
open System.Diagnostics
open FSharp.Editing
open FSharp.Editing.ProjectSystem
open FSharp.Editing.UntypedAstUtils
open FSharp.Editing.Features
open FSharp.Editing.AsyncMaybe
open FSharp.Editing.VisualStudio.Coloring
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.Editing.VisualStudio
open FSharp.Editing.SourceCodeClassifier

[<NoComparison>]
type private Data =
    { Snapshot: ITextSnapshot
      Spans: CategorizedSnapshotSpans } 

[<NoComparison>]
type private State =
    | NoData
    | Updating of oldData: Data option * currentSnapshot: ITextSnapshot
    | Data of Data

type SymbolClassifier
    (
        doc: ITextDocument,
        buffer: ITextBuffer,
        classificationRegistry: IClassificationTypeRegistryService,
        vsLanguageService: VSLanguageService,
        serviceProvider: IServiceProvider,
        projectFactory: ProjectFactory
    ) as self =

    let typeName = self.GetType().Name
    let debug msg = Printf.kprintf (fun x -> Debug.WriteLine ("[" + typeName + "] " + x)) msg
    let classificationChanged = Event<_,_>()
    let state = Atom State.NoData
    let dte = serviceProvider.GetDte()
        
    let triggerClassificationChanged snapshot reason =
        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
        classificationChanged.Trigger(self, ClassificationChangedEventArgs span)
        debug "ClassificationChanged event has been triggered by %s" reason

    let checkAstIsNotEmpty (ast: ParsedInput) =
        if ast.Range.IsEmpty then
            debug "Empty AST"
            None
        else Some()

    let getCurrentSnapshot() =
        maybe {
            let! doc = Option.ofNull doc
            let! buffer = Option.ofNull doc.TextBuffer
            return buffer.CurrentSnapshot }
    
    let project() = projectFactory.CreateForDocument buffer doc.FilePath

    let updateSyntaxConstructClassifiers force (CallInUIContext callInUIContext) = 
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
                let! currentProject = project()
                let! snapshot = snapshot
                debug "Effective update"
                let! checkResults = vsLanguageService.ParseAndCheckFileInProject(doc.FilePath, currentProject)
                let! ast = checkResults.ParseTree
                do! checkAstIsNotEmpty ast
                let! lexer = vsLanguageService.CreateLexer(doc.FilePath, snapshot, currentProject.Project.CompilerOptions)

                let! allSymbolsUses =
                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(snapshot, doc.FilePath, currentProject, AllowStaleResults.No, false)

                let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()

                let spans =
                    getCategorizedSpans (allSymbolsUses, checkResults, lexer, getTextLineOneBased)
                    |> Array.sortBy (fun x -> x.WordSpan.Line)
                    |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))

                let spans = { Spans = spans; Errors = checkResults.Errors }

                state.Swap (fun oldState ->
                    let spans =
                        match oldState with
                        | State.Data oldData
                        | State.Updating (Some oldData, _) -> mergeSpans oldData.Spans spans
                        | _ -> spans
                    State.Data { Snapshot = snapshot; Spans = spans }) |> ignore

                do! liftAsync (callInUIContext (fun _ -> triggerClassificationChanged snapshot "UpdateSyntaxConstructClassifiers"))
            } |> Async.Ignore
        else async.Return ()

    let events: EnvDTE80.Events2 option = tryCast dte.Events

    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun p _ _ _ _ ->
        maybe {
            let! selfProject = project()
            let builtProjectFileName = Path.GetFileName p
            let referencedProjectFileNames = selfProject.GetAllReferencedProjectFileNames()

            if referencedProjectFileNames |> List.contains builtProjectFileName then
                debug "Referenced project %s has been built, updating classifiers..." builtProjectFileName
                let callInUIContext = CallInUIContext.FromCurrentThread()
                updateSyntaxConstructClassifiers true callInUIContext |> Async.StartInThreadPoolSafe
        } |> ignore)

    do events |> Option.iter (fun e -> e.BuildEvents.add_OnBuildProjConfigDone onBuildDoneHandler)

    let docEventListener =
        new DocumentEventListener ([ViewChange.bufferEvent doc.TextBuffer], 100us, updateSyntaxConstructClassifiers false)

    let getClassificationSpans (targetSnapshotSpan: SnapshotSpan) =
        match state.Value with
        | State.Data { Data.Spans = spans }
        | State.Updating (Some { Data.Spans = spans }, _) ->
            getClassificationSpans spans targetSnapshotSpan classificationRegistry
        | State.NoData ->
            // Only schedule an update on signature files
            if isSignatureFile doc.FilePath then
                // If not yet schedule an action, do it now.
                let callInUIContext = CallInUIContext.FromCurrentThread()
                updateSyntaxConstructClassifiers false callInUIContext |> Async.StartInThreadPoolSafe
            [||]
        | State.Updating _ -> [||]

    interface IClassifier with
        // It's called for each visible line of code
        member __.GetClassificationSpans span =
            upcast (protectOrDefault (fun _ -> getClassificationSpans span) [||])

        [<CLIEvent>]
        member __.ClassificationChanged = classificationChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            events |> Option.iter (fun e -> e.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler)
            (docEventListener :> IDisposable).Dispose()