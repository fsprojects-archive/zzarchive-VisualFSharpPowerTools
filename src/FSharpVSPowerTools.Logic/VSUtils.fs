[<AutoOpen>]
module FSharpVSPowerTools.ProjectSystem.VSUtils

open System
open System.Text.RegularExpressions
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools

/// Retrieve snapshot from VS zero-based positions
let fromFSharpPos (snapshot: ITextSnapshot) (r: range) =
    let startPos = snapshot.GetLineFromLineNumber(r.StartLine - 1).Start.Position + r.StartColumn
    let endPos = snapshot.GetLineFromLineNumber(r.EndLine - 1).Start.Position + r.EndColumn
    SnapshotSpan(snapshot, startPos, endPos - startPos)

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
    member x.FromRange (lineStart, colStart, lineEnd, colEnd) =
        let startPos = x.Snapshot.GetLineFromLineNumber(lineStart).Start.Position + colStart
        let endPos = x.Snapshot.GetLineFromLineNumber(lineEnd).Start.Position + colEnd
        SnapshotSpan(x.Snapshot, startPos, endPos - startPos)
    member x.InSpan (span: SnapshotSpan) = x.CompareTo span.Start >= 0 && x.CompareTo span.End <= 0

type SnapshotSpan with
    /// Return corresponding zero-based range
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
        let _ = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
        let _ = serviceProvider.GetService<IVsUIShellOpenDocument, SVsUIShellOpenDocument>()
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
    
type ProjectItem with
    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun item ->
            try Option.ofNull (item.ContainingProject.Object :?> VSProject) with _ -> None)

type SolutionEvents (serviceProvider: IServiceProvider) =
    let projectChanged = Event<_>()
    // we must keep a reference to the events in order to prevent GC to collect it
    let dte = serviceProvider.GetService<DTE, SDTE>()
    let events: EnvDTE80.Events2 option = tryCast dte.Events
    
    let onProjectChanged (projectItem: ProjectItem) =
        projectItem.VSProject
        |> Option.iter (fun item ->
            debug "[ProjectsCache] %s changed." projectItem.Name
            item.Project.Save()
            projectChanged.Trigger item)

    do match events with
       | Some events ->
           events.ProjectItemsEvents.add_ItemRenamed (fun p _ -> onProjectChanged p)
           events.ProjectItemsEvents.add_ItemRemoved (fun p -> onProjectChanged p)
           events.ProjectItemsEvents.add_ItemAdded (fun p -> onProjectChanged p)
           debug "[SolutionEvents] Subscribed for ProjectItemsEvents"
       | _ -> fail "[SolutionEvents] Cannot subscribe for ProjectItemsEvents"
    
    /// Raised when any project in solution has changed.
    member x.ProjectChanged = projectChanged.Publish

let inline ensureSucceded hr = 
    ErrorHandler.ThrowOnFailure hr
    |> ignore        

open System.ComponentModel.Composition

[<Literal>]
let private UnassignedThreadId = -1

type ForegroundThreadGuard private() = 
    static let mutable threadId = UnassignedThreadId
    static member BindThread() =
        if threadId <> UnassignedThreadId then 
            fail "Thread is already set"
        threadId <- System.Threading.Thread.CurrentThread.ManagedThreadId
    static member CheckThread() =
        if threadId = UnassignedThreadId then 
            fail "Thread not set"
        if threadId <> System.Threading.Thread.CurrentThread.ManagedThreadId then
            fail "Accessed from the wrong thread"

open System.Windows.Threading

type DocumentEventsListener (view: ITextView, update: unit -> unit) =
    // start an async loop on the UI thread that will re-parse the file and compute tags after idle time after a source change
    let timeSpan = TimeSpan.FromMilliseconds 200.

    let events =
        view.LayoutChanged
        |> Event.choose (fun e -> if e.NewSnapshot <> e.OldSnapshot then Some() else None)
        |> Event.merge (view.Caret.PositionChanged |> Event.map (fun _ -> ()))
        
    let startNewTimer() = 
        let timer = new DispatcherTimer(DispatcherPriority.ApplicationIdle, Interval = timeSpan)
        timer.Start()
        timer
        
    let rec awaitPauseAfterChange (timer: DispatcherTimer) = 
        async { 
            let! e = Async.EitherEvent(events, timer.Tick)
            match e with
            | Choice1Of2 _ -> 
                timer.Stop()
                do! awaitPauseAfterChange (startNewTimer())
            | _ -> ()
        }
        
    do async { 
        while true do
            do! Async.AwaitEvent events
            do! awaitPauseAfterChange (startNewTimer())
            update() }
       |> Async.StartImmediate
       // go ahead and synchronously get the first bit of info for the original rendering
       update() 
