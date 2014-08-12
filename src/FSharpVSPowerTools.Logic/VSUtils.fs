[<AutoOpen>]
module FSharpVSPowerTools.ProjectSystem.VSUtils

open System
open System.Diagnostics
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools

let fromRange (snapshot: ITextSnapshot) (startLine, startColumn, endLine, endColumn) =
    Debug.Assert(startLine <= endLine, sprintf "startLine = %d, endLine = %d" startLine endLine)
    Debug.Assert(startLine <> endLine || startColumn < endColumn, 
                 sprintf "Single-line pos, but startCol = %d, endCol = %d" startColumn endColumn)
    try 
        let startPos = snapshot.GetLineFromLineNumber(startLine - 1).Start.Position + startColumn
        let endPos = snapshot.GetLineFromLineNumber(endLine - 1).Start.Position + endColumn
        Debug.Assert(startPos < endPos, sprintf "startPos = %d, endPos = %d" startPos endPos)
        let length = endPos - startPos
        Some (SnapshotSpan(snapshot, startPos, length))
    with e ->
        fail "Attempting to create a SnapshotSpan (StartLine = %d, StartColumn = %d, EndLine = %d, EndColumn = %d) in a snapshot length = %d results in: %O"
             startLine startColumn endLine endColumn snapshot.Length e
        Logging.logException e
        None
    
/// Retrieve snapshot span from VS zero-based positions
let fromFSharpRange (snapshot: ITextSnapshot) (r: range) = 
    fromRange snapshot (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

let FSharpProjectKind = "{F2A71F9B-5D33-465A-A702-920D77279786}"
let isFSharpProject (project: EnvDTE.Project) = 
    project <> null && project.Kind <> null && project.Kind.Equals(FSharpProjectKind, StringComparison.OrdinalIgnoreCase)

let isPhysicalFolderKind (kind: string) =
    kind.Equals(EnvDTE.Constants.vsProjectItemKindPhysicalFolder, StringComparison.OrdinalIgnoreCase)

let isPhysicalFileKind (kind: string) =
    kind.Equals(EnvDTE.Constants.vsProjectItemKindPhysicalFile, StringComparison.OrdinalIgnoreCase)

let isPhysicalFileOrFolderKind kind =
    kind <> null && (isPhysicalFolderKind kind) || (isPhysicalFileKind kind)

let isPhysicalFolder (item: EnvDTE.ProjectItem) =
    item <> null && item.Kind <> null && (isPhysicalFolderKind item.Kind)

let isPhysicalFile (item: EnvDTE.ProjectItem) =
    item <> null && item.Kind <> null && (isPhysicalFileKind item.Kind)

let isPhysicalFileOrFolder (item: EnvDTE.ProjectItem) =
    item <> null && isPhysicalFileOrFolderKind item.Kind

let inline private isTypeParameter (prefix: char) (s: string) =
    match s.Length with
    | 0 | 1 -> false
    | _ -> s.[0] = prefix && IdentifierUtils.isIdentifier (s.Substring(1))

let isGenericTypeParameter = isTypeParameter '''
let isStaticallyResolvedTypeParameter = isTypeParameter '^'

type SnapshotPoint with
    member x.InSpan (span: SnapshotSpan) = 
        // The old snapshot might not be available anymore, we compare on updated snapshot
        let point = x.TranslateTo(span.Snapshot, PointTrackingMode.Positive)
        point.CompareTo span.Start >= 0 && point.CompareTo span.End <= 0

type SnapshotSpan with
    /// Return corresponding zero-based FCS range
    member x.ToRange() =
        let lineStart = x.Snapshot.GetLineNumberFromPosition(x.Start.Position)
        let lineEnd = x.Snapshot.GetLineNumberFromPosition(x.End.Position)
        let startLine = x.Snapshot.GetLineFromPosition(x.Start.Position)
        let endLine = x.Snapshot.GetLineFromPosition(x.End.Position)
        let colStart = x.Start.Position - startLine.Start.Position
        let colEnd = x.End.Position - endLine.Start.Position
        (lineStart, colStart, lineEnd, colEnd - 1)

type ITextBuffer with
    member x.GetSnapshotPoint (position: CaretPosition) = 
        Option.ofNullable <| position.Point.GetPoint(x, position.Affinity)
    
    member x.TriggerTagsChanged (sender: obj) (event: Event<_,_>) =
        let span = SnapshotSpan(x.CurrentSnapshot, 0, x.CurrentSnapshot.Length)
        event.Trigger(sender, SnapshotSpanEventArgs(span))
        
type IServiceProvider with
    member x.GetService<'T>() = x.GetService(typeof<'T>) :?> 'T
    member x.GetService<'T, 'S>() = x.GetService(typeof<'S>) :?> 'T

open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.ComponentModelHost

// This is for updating documents after refactoring
// Reference at https://pytools.codeplex.com/SourceControl/latest#Python/Product/PythonTools/PythonToolsPackage.cs

type DocumentUpdater(serviceProvider: IServiceProvider) = 
    member x.OpenDocument(fileName: string, [<Out>] viewAdapter: byref<IVsTextView>, pWindowFrame: byref<IVsWindowFrame>) = 
        let hierarchy = ref null
        let itemId = ref 0u
        VsShellUtilities.OpenDocument(serviceProvider, fileName, Guid.Empty, hierarchy, itemId, &pWindowFrame, &viewAdapter)

    member x.GetBufferForDocument(fileName: string) = 
        let viewAdapter = ref null
        let frame = ref null
        x.OpenDocument(fileName, viewAdapter, frame)

        let lines = ref null
        ErrorHandler.ThrowOnFailure((!viewAdapter).GetBuffer(lines)) |> ignore

        let componentModel = serviceProvider.GetService<IComponentModel, SComponentModel>()
        let adapter = componentModel.GetService<IVsEditorAdaptersFactoryService>()
        adapter.GetDocumentBuffer(!lines)

    member x.BeginGlobalUndo(key: string) = 
        let linkedUndo = serviceProvider.GetService<IVsLinkedUndoTransactionManager, SVsLinkedUndoTransactionManager>()
        ErrorHandler.ThrowOnFailure(linkedUndo.OpenLinkedUndo(uint32 LinkedTransactionFlags2.mdtGlobal, key)) |> ignore
        linkedUndo

    member x.EndGlobalUndo(linkedUndo: IVsLinkedUndoTransactionManager) = 
        ErrorHandler.ThrowOnFailure(linkedUndo.CloseLinkedUndo()) |> ignore

open EnvDTE
open VSLangProj

type DTE with
    member x.GetActiveDocument() =
        let doc =
            maybe {
                let! doc = Option.ofNull x.ActiveDocument 
                let! _ = Option.ofNull doc.ProjectItem
                return doc }
        match doc with
        | None -> debug "Should be able to find an active document."
        | _ -> ()
        doc

type ProjectItem with
    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun item ->
            try Option.ofNull (item.ContainingProject.Object :?> VSProject) with _ -> None)

    member x.TryGetProperty name = 
        let property = x.Properties |> Seq.cast<Property> |> Seq.tryFind (fun p -> p.Name = name)
        match property with
        | Some p -> Some (p.Value :?> string)
        | None -> None

    member x.GetProperty name = 
        let property = x.TryGetProperty name
        match property with
        | Some p -> p
        | None -> raise(new ArgumentException("name"))

type Project with
    member x.GetReferencedProjects() = 
        (x.Object :?> VSProject).References
        |> Seq.cast<Reference>
        |> Seq.choose (fun reference ->
            maybe {
                let! reference = Option.ofNull reference
                let! project = Option.attempt (fun _ -> reference.SourceProject)
                return! Option.ofNull project
            })
        |> Seq.toList

    member x.GetReferencedFSharpProjects() = x.GetReferencedProjects() |> List.filter isFSharpProject

    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun project ->
            try Option.ofNull (project.Object :?> VSProject) with _ -> None)

let getProject (hierarchy: IVsHierarchy) =
    match hierarchy.GetProperty(VSConstants.VSITEMID_ROOT,
                                int __VSHPROPID.VSHPROPID_ExtObject) with
    | VSConstants.S_OK, p ->
        tryCast<Project> p
    | _ -> 
        None

let inline ensureSucceeded hr = 
    ErrorHandler.ThrowOnFailure hr
    |> ignore

let private getSelectedFromSolutionExplorer<'T> (dte:EnvDTE80.DTE2) =
    let items = dte.ToolWindows.SolutionExplorer.SelectedItems :?> UIHierarchyItem[]
    items
    |> Seq.choose (fun x ->
            match x.Object with
            | :? 'T as p -> Some p
            | _ -> None)

let getSelectedItemsFromSolutionExplorer dte =
    getSelectedFromSolutionExplorer<ProjectItem> dte

let getSelectedProjectsFromSolutionExplorer dte =
    getSelectedFromSolutionExplorer<Project> dte

open System.Threading
open System.Windows.Threading
open Microsoft.VisualStudio.Text.Classification
open System.Windows.Input

[<Literal>]
let private UnassignedThreadId = -1

type ForegroundThreadGuard private() = 
    static let mutable threadId = UnassignedThreadId
    static member BindThread() =
        if threadId <> UnassignedThreadId then 
            () // fail "Thread is already set"
        threadId <- Thread.CurrentThread.ManagedThreadId
    static member CheckThread() =
        if threadId = UnassignedThreadId then 
            fail "Thread not set"
        if threadId <> Thread.CurrentThread.ManagedThreadId then
            fail "Accessed from the wrong thread"

[<RequireQualifiedAccess>]
module ViewChange =    
    let layoutEvent (view: ITextView) = 
        view.LayoutChanged |> Event.choose (fun e -> if e.NewSnapshot <> e.OldSnapshot then Some() else None)
    
    let viewportHeightEvent (view: ITextView) = 
        view.ViewportHeightChanged |> Event.map (fun _ -> ())

    let caretEvent (view: ITextView) = 
        view.Caret.PositionChanged |> Event.map (fun _ -> ())

    let bufferEvent (buffer: ITextBuffer) = 
        buffer.Changed |> Event.map (fun _ -> ())

    let classificationEvent (classifier: IClassifier) = 
        classifier.ClassificationChanged |> Event.map (fun _ -> ())

type DocumentEventListener (events: IEvent<unit> list, delayMillis: uint16, update: unit -> unit) =
    // Start an async loop on the UI thread that will re-parse the file and compute tags after idle time after a source change
    do if List.isEmpty events then invalidArg "changes" "Changes must be a non-empty list"
    let events = events |> List.reduce Event.merge
    let timer = DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                Interval = TimeSpan.FromMilliseconds (float delayMillis))
    let tokenSource = new CancellationTokenSource()

    // This is none or forall option for unit testing purpose only
    static let mutable skipTimerDelay = false

    let startNewTimer() = 
        timer.Stop()
        timer.Start()
        
    let rec awaitPauseAfterChange() =
        async { 
            let! e = Async.EitherEvent(events, timer.Tick)
            match e with
            | Choice1Of2 _ -> 
                startNewTimer()
                do! awaitPauseAfterChange()
            | _ -> ()
        }
        
    do 
       let computation =
           async { 
            while true do
                do! Async.AwaitEvent events
                if not skipTimerDelay then
                    startNewTimer()
                    do! awaitPauseAfterChange()
                update() }
       Async.StartImmediate(computation, tokenSource.Token)
       // Go ahead and synchronously get the first bit of info for the original rendering
       update()

    /// Skip all timer events in order to test events instantineously
    static member internal SkipTimerDelay 
        with get () = skipTimerDelay
        and set v = skipTimerDelay <- v

    interface IDisposable with
        member x.Dispose() =
            tokenSource.Cancel()
            tokenSource.Dispose()
            timer.Stop()
         
type Async with
    /// An equivalence of Async.StartImmediate which catches and logs raised exceptions
    static member StartImmediateSafe(computation, ?cancellationToken) =
        let comp =
            async {
                try
                    return! computation
                with e ->
                    fail "The following exception occurs inside async blocks: %O" e
                    Logging.logException e
            }
        Async.StartImmediate(comp, ?cancellationToken = cancellationToken)

    /// An equivalence of Async.Start which catches and logs raised exceptions
    static member StartInThreadPoolSafe(computation, ?cancellationToken) =
        let comp =
            async {
                try
                    return! computation
                with e ->
                    fail "The following exception occurs inside async blocks: %O" e
                    Logging.logException e
            }
        Async.Start(comp, ?cancellationToken = cancellationToken)       

/// Provides an IDisposable handle which allows us to override the cursor cleanly as well as restore whenever needed
type CursorOverrideHandle(newCursor) =
    let mutable disposed = false

    let originalCursor = Mouse.OverrideCursor
    let restore () = 
        if not disposed then
            Mouse.OverrideCursor <- originalCursor
            disposed <- true

    do Mouse.OverrideCursor <- newCursor

    member x.Restore() = restore()

    interface IDisposable with 
        member x.Dispose() = restore()

module internal Cursor =
    let wait() = new CursorOverrideHandle(System.Windows.Input.Cursors.Wait)
