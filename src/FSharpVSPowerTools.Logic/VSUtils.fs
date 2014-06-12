[<AutoOpen>]
module FSharpVSPowerTools.ProjectSystem.VSUtils

open System
open System.Diagnostics
open System.Text.RegularExpressions
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
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
        fail "Attempt to create a SnapshotSpan with wrong arguments (StartLine = %d, StartColumn = %d, EndLine = %d, EndColumn = %d)"
             startLine startColumn endLine endColumn
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

open Microsoft.FSharp.Compiler.PrettyNaming

let private isDoubleBacktickIdent (s: string) =
    if s.StartsWith("``") && s.EndsWith("``") && s.Length > 4 then
        let inner = s.Substring("``".Length, s.Length - "````".Length)
        not (inner.Contains("``"))
    else
        false

let isIdentifier (s: string) =
    if isDoubleBacktickIdent s then
        true
    else
        s |> Seq.mapi (fun i c -> i, c)
          |> Seq.forall (fun (i, c) -> 
                if i = 0 then IsIdentifierFirstCharacter c else IsIdentifierPartCharacter c) 

let isOperator (s: string) = 
    let allowedChars = Set.ofList ['!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~']
    (IsPrefixOperator s || IsInfixOperator s || IsTernaryOperator s)
    && (s.ToCharArray() |> Array.forall (fun c -> Set.contains c allowedChars))

let inline private isTypeParameter (prefix: char) (s: string) =
    match s.Length with
    | 0 | 1 -> false
    | _ -> s.[0] = prefix && isIdentifier (s.Substring(1))

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

open Microsoft.VisualStudio.Shell
open EnvDTE
open VSLangProj
open System.Diagnostics

type DTE with
    member x.GetActiveDocument() =
        let doc =
            maybe {
                let! doc = Option.ofNull x.ActiveDocument 
                let! item = Option.ofNull doc.ProjectItem 
                let! _ = Option.ofNull item.ContainingProject 
                return doc }
        match doc with
        | None -> debug "Should be able to find active document and active project."
        | _ -> ()
        doc

    member x.ListFSharpProjectsInSolution() = 
        let rec handleProject (p: Project) = 
            if p === null then []
            elif isFSharpProject p then [ p ]
            elif p.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then handleProjectItems p.ProjectItems
            else []
        
        and handleProjectItems (items: ProjectItems) = 
            [ for pi in items do
                  yield! handleProject pi.SubProject ]
        
        [ for p in x.Solution.Projects do
              yield! handleProject p ]
    
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

open System.ComponentModel.Composition
open System.Threading

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

module ViewChange =
    let layoutEvent (view: ITextView) = 
        view.LayoutChanged |> Event.choose (fun e -> if e.NewSnapshot <> e.OldSnapshot then Some() else None)
    let caretEvent (view: ITextView) = view.Caret.PositionChanged |> Event.map (fun _ -> ())
    let bufferChangedEvent (buffer: ITextBuffer) = buffer.Changed |> Event.map (fun _ -> ())

open System.Windows.Threading

type DocumentEventsListener (events: IEvent<unit> list, delayMillis: uint16, update: unit -> unit) =
    // start an async loop on the UI thread that will re-parse the file and compute tags after idle time after a source change
    do if List.isEmpty events then invalidArg "changes" "Changes must be a non-empty list"
    let events = events |> List.reduce Event.merge
    let timer = new DispatcherTimer(DispatcherPriority.ApplicationIdle,      
                                    Interval = TimeSpan.FromMilliseconds (float delayMillis))

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
        
    do async { 
        while true do
            do! Async.AwaitEvent events
            startNewTimer()
            do! awaitPauseAfterChange()
            update() }
       |> Async.StartImmediate
       // Go ahead and synchronously get the first bit of info for the original rendering
       update()

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
        
