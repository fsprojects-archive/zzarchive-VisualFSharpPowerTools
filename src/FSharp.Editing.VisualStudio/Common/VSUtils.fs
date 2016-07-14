[<AutoOpen>]
module FSharp.Editing.VisualStudio.VSUtils

open System
open FSharpPowerTools.Core
open FSharpVSPowerTools

type String with
    /// Splits a string into lines for all platform's linebreaks.
    /// If the string mixes windows, mac, and linux linebreaks, all will be respected
    member self.ToLineArray () = String.getLines self

    /// Return substring starting at index, returns the same string if given a negative index
    /// if given an index > string.Length returns empty string
    member self.SubstringSafe index =
        if index < 0 then self elif index > self.Length then "" else self.Substring index

open System.Diagnostics
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.FSharp.Compiler.Range

let fromRange (snapshot: ITextSnapshot) (startLine, startColumn, endLine, endColumn) =
    Debug.Assert(startLine <= endLine, sprintf "startLine = %d, endLine = %d" startLine endLine)
    Debug.Assert(startLine <> endLine || startColumn <= endColumn, 
                 sprintf "Single-line pos, but startCol = %d, endCol = %d" startColumn endColumn)
    try 
        let startPos = snapshot.GetLineFromLineNumber(startLine - 1).Start.Position + startColumn
        let endPos = snapshot.GetLineFromLineNumber(endLine - 1).Start.Position + endColumn
        Debug.Assert(startPos <= endPos, sprintf "startPos = %d, endPos = %d" startPos endPos)
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

let [<Literal>] FSharpProjectKind = "{F2A71F9B-5D33-465A-A702-920D77279786}"

let isFSharpProject (project: EnvDTE.Project) = 
    isNotNull project && isNotNull project.Kind && project.Kind.Equals(FSharpProjectKind, StringComparison.OrdinalIgnoreCase)

let isPhysicalFolderKind (kind: string) =
    kind.Equals(EnvDTE.Constants.vsProjectItemKindPhysicalFolder, StringComparison.OrdinalIgnoreCase)

let isPhysicalFileKind (kind: string) =
    kind.Equals(EnvDTE.Constants.vsProjectItemKindPhysicalFile, StringComparison.OrdinalIgnoreCase)

let isPhysicalFileOrFolderKind kind =
    isNotNull kind && (isPhysicalFolderKind kind) || isPhysicalFileKind kind

let isPhysicalFolder (item: EnvDTE.ProjectItem) =
    isNotNull item && isNotNull item.Kind && isPhysicalFolderKind item.Kind

let isPhysicalFile (item: EnvDTE.ProjectItem) =
    isNotNull item && isNotNull item.Kind && isPhysicalFileKind item.Kind

let isPhysicalFileOrFolder (item: EnvDTE.ProjectItem) =
    isNotNull item && isPhysicalFileOrFolderKind item.Kind

let filePath (item: EnvDTE.ProjectItem) =
    Debug.Assert(item.FileCount = 1s, "Item should be unique.")
    item.FileNames(1s) //1 based indexing

let inline private isTypeParameter (prefix: char) (s: string) =
    match s.Length with
    | 0 | 1 -> false
    | _ -> s.[0] = prefix && IdentifierUtils.isIdentifier s.[1..]

let isGenericTypeParameter = isTypeParameter '''
let isStaticallyResolvedTypeParameter = isTypeParameter '^'

type SnapshotPoint with
    member x.InSpan (span: SnapshotSpan) = 
        // The old snapshot might not be available anymore, we compare on updated snapshot
        let point = x.TranslateTo(span.Snapshot, PointTrackingMode.Positive)
        point.CompareTo span.Start >= 0 && point.CompareTo span.End <= 0
    member x.ToPoint =
        x.Snapshot.GetLineNumberFromPosition x.Position, x.Position - x.GetContainingLine().Start.Position
    
    member x.MakePointInDocument filename source : PointInDocument<FCS> =
        let line = x.Snapshot.GetLineNumberFromPosition x.Position
        let col = x.Position - x.GetContainingLine().Start.Position
        let lineStr = x.GetContainingLine().GetText()
        { Point = Point.make line col; Line = lineStr; File = filename; Document = source }

type ITextSnapshot with
    /// SnapshotSpan of the entirety of this TextSnapshot
    member x.FullSpan =
        SnapshotSpan(x, 0, x.Length)

    /// Get the start and end line numbers of a snapshotSpan based on this textSnapshot
    /// returns a tuple of (startLineNumber, endLineNumber)
    member inline x.LineBounds (snapshotSpan:SnapshotSpan) =
        let startLineNumber = x.GetLineNumberFromPosition (snapshotSpan.Span.Start)
        let endLineNumber = x.GetLineNumberFromPosition (snapshotSpan.Span.End)
        (startLineNumber, endLineNumber)

    /// Get the text at line `num`
    member inline x.LineText num =  x.GetLineFromLineNumber(num).GetText()

type SnapshotSpan with
    member inline x.StartLine = x.Snapshot.GetLineFromPosition (x.Start.Position)
    member inline x.StartLineNum = x.Snapshot.GetLineNumberFromPosition x.Start.Position
    member inline x.StartColumn = x.Start.Position - x.StartLine.Start.Position 
    member inline x.EndLine = x.Snapshot.GetLineFromPosition (x.End.Position)
    member inline x.EndLineNum  = x.Snapshot.GetLineNumberFromPosition x.End.Position
    member inline x.EndColumn = x.End.Position - x.EndLine.Start.Position

    member x.ModStart (num) = SnapshotSpan(SnapshotPoint (x.Snapshot, x.Start.Position + num), x.End)
    member x.ModEnd (num) = SnapshotSpan(x.Start, (SnapshotPoint (x.Snapshot,x.End.Position + num)))

    member x.ModBoth m1 m2 =
        SnapshotSpan(SnapshotPoint (x.Snapshot, x.Start.Position + m1),
                     SnapshotPoint (x.Snapshot, x.End.Position + m2))

    /// get the position of the token found at (line,.col) if token was not found then -1,-1
    member x.PositionOf (token:string) =
        let firstLine = x.StartLineNum
        let lastLine = x.EndLineNum
        let lines = [| for idx in firstLine .. lastLine -> x.Snapshot.LineText idx |]

        let withinBounds (line, col) =
            match line, col with
            | -1,-1 -> -1,-1 // fast terminate if token wasn't found
            |  l, c when c < x.StartColumn &&  l = firstLine -> -1,-1
            |  l, c when c > x.EndColumn &&  l = lastLine -> -1,-1
            | _ -> line,col

        let rec loop idx =
            if idx > lines.Length then -1,-1 else
            match lines.[idx].IndexOf(token) with
            | -1 -> loop (idx+1)
            | toki -> (firstLine+idx,toki)
        
        loop 0 |> withinBounds

    /// Return corresponding zero-based FCS range
    /// (lineStart, colStart, lineEnd, colEnd)
    member inline x.ToRange () : Range<FCS> =
        Range.make x.StartLineNum x.StartColumn x.EndLineNum (x.EndColumn - 1)

    member inline x.MakeCurrentLine (filename) =
        { Line = x.Start.GetContainingLine().GetText(); Range = x.ToRange(); File = filename }
    
    static member MakeFromRange (snapshot: ITextSnapshot) (range:Range<FCS>) =
        let startPos = snapshot.GetLineFromLineNumber(range.Start.Line).Start.Position + range.Start.Column
        let endPos = snapshot.GetLineFromLineNumber(range.End.Line).Start.Position + range.End.Column
        SnapshotSpan(snapshot, startPos, endPos - startPos)

type ITextBuffer with
    member x.GetSnapshotPoint (position: CaretPosition) = 
        Option.ofNullable <| position.Point.GetPoint(x, position.Affinity)
    
    member x.TriggerTagsChanged (sender: obj) (event: Event<_,_>) =
        let span = x.CurrentSnapshot.FullSpan
        event.Trigger(sender, SnapshotSpanEventArgs(span))

type ITextView with
    /// Return a simple zero-based (line, column) tuple from 
    /// actual caret position in this ITextView
    member x.GetCaretPosition () =
        maybe {
          let! point = x.TextBuffer.GetSnapshotPoint x.Caret.Position
          let line = point.Snapshot.GetLineNumberFromPosition point.Position
          let col = point.Position - point.GetContainingLine().Start.Position
          return (line, col)
        }

    /// Return Microsoft.FSharp.Compiler.Range.pos from actual 
    /// caret position in this ITextView taking care of off by 
    /// 1 indexing between VS and FCS
    member x.PosAtCaretPosition () =
        maybe {
          let! line, col = x.GetCaretPosition()
          return Microsoft.FSharp.Compiler.Range.mkPos (line + 1) (col + 1)
        }

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
    member __.OpenDocument(fileName: string, [<Out>] viewAdapter: byref<IVsTextView>, pWindowFrame: byref<IVsWindowFrame>) = 
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

    member __.BeginGlobalUndo(key: string) = 
        let linkedUndo = serviceProvider.GetService<IVsLinkedUndoTransactionManager, SVsLinkedUndoTransactionManager>()
        ErrorHandler.ThrowOnFailure(linkedUndo.OpenLinkedUndo(uint32 LinkedTransactionFlags2.mdtGlobal, key)) |> ignore
        linkedUndo

    member __.EndGlobalUndo(linkedUndo: IVsLinkedUndoTransactionManager) = 
        ErrorHandler.ThrowOnFailure(linkedUndo.CloseLinkedUndo()) |> ignore

/// Fix invalid symbols if they appear to have redundant suffix and prefix. 
/// All symbol uses are assumed to belong to a single snapshot.
let fixInvalidSymbolSpans (snapshot: ITextSnapshot) (lastIdent: string) spans =
    spans
    |> Seq.choose (fun (isDef, span: SnapshotSpan) -> 
        let newLastIdent = span.GetText()
        let index = newLastIdent.LastIndexOf(lastIdent, StringComparison.Ordinal)
        if index > 0 then 
            // Sometimes FCS returns a composite identifier for a short symbol, so we truncate the prefix
            // Example: newLastIdent --> "x.Length", lastIdent --> "Length"
            Some (isDef, SnapshotSpan(snapshot, span.Start.Position + index, span.Length - index))
        elif index = 0 && newLastIdent.Length > lastIdent.Length then
            // The returned symbol use is too long; we truncate its redundant suffix
            // Example: newLastIdent --> "Length<'T>", lastIdent --> "Length"
            Some (isDef, SnapshotSpan(snapshot, span.Start.Position, lastIdent.Length))
        elif index = 0 then
            Some (isDef, span)
        else
            // In the case of attributes, a returned symbol use may be a part of original text
            // Example: newLastIdent --> "Sample", lastIdent --> "SampleAttribute"
            let index = lastIdent.LastIndexOf(newLastIdent, StringComparison.Ordinal)
            if index >= 0 then
                Some (isDef, span)
            else None)
    |> Seq.distinctBy (fun (_, span) -> span.Start.Position)
    |> Seq.toList

open EnvDTE
open VSLangProj

let tryGetProject (hierarchy: IVsHierarchy) =
    if isNull hierarchy then
        None
    else
        match hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, int __VSHPROPID.VSHPROPID_ExtObject) with
        | VSConstants.S_OK, p ->
            tryCast<Project> p
        | _ -> 
            None

let tryFindProject (rdt: IVsRunningDocumentTable) fileName =
    match rdt.FindAndLockDocument(uint32 _VSRDTFLAGS.RDT_NoLock, fileName) with
    | VSConstants.S_OK, hier, _, _, _ ->
        tryGetProject hier            
    | _ -> None

type DTE with
    member x.GetActiveDocument() =
        let doc =
            maybe {
                let! doc = Option.attempt (fun _ -> x.ActiveDocument) |> Option.bind Option.ofNull
                let! _ = Option.ofNull doc.ProjectItem
                return doc }
        match doc with
        | None -> debug "There is no active document or its project item is null."
        | _ -> ()
        doc

    member x.TryGetProperty(category, page, name) = 
        x.Properties(category, page)
        |> Seq.cast
        |> Seq.tryPick (fun (prop: Property) ->
            if prop.Name = name then Some (prop.Value)
            else None)

type ProjectItem with
    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun item ->
            Option.attempt (fun _ -> item.ContainingProject.Object :?> VSProject)
            |> Option.bind Option.ofNull)

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
            Option.attempt (fun _ -> project.Object :?> VSProject)
            |> Option.bind Option.ofNull)

let inline ensureSucceeded hr = 
    ErrorHandler.ThrowOnFailure hr
    |> ignore

let getSelectedFromSolutionExplorer<'T> (dte: EnvDTE80.DTE2) =
    Option.attempt (fun _ -> dte.ToolWindows.SolutionExplorer)
    |> function Some x -> x.SelectedItems :?> UIHierarchyItem[] | None -> [||]
    |> Seq.choose (fun x ->
         match x.Object with
         | :? 'T as p -> Some p
         | _ -> None)
    |> Seq.toList

open System.Windows
open System.Windows.Interop

/// Display a modal dialog and set VS window as the owner
let showDialog (wnd: Window) (shell: IVsUIShell) = 
    match shell.GetDialogOwnerHwnd() with
    | VSConstants.S_OK, hwnd ->
        let helper = WindowInteropHelper(wnd)
        helper.Owner <- hwnd
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        try
            if ErrorHandler.Failed(shell.EnableModeless(0)) then Some false
            else wnd.ShowDialog() |> Option.ofNullable
        finally
            shell.EnableModeless(1) |> ignore
    | _ -> 
        None

open System.Threading
open System.Windows.Input

[<Literal>]
let private UnassignedThreadId = -1

type ForegroundThreadGuard private() = 
    static let mutable threadId = UnassignedThreadId

    static member BindThread() =
        if threadId <> UnassignedThreadId then 
            fail "Thread is already set"
        threadId <- Thread.CurrentThread.ManagedThreadId

    static member CheckThread() =
        if threadId = UnassignedThreadId then 
            fail "Thread not set"
        if threadId <> Thread.CurrentThread.ManagedThreadId then
            fail "Accessed from the wrong thread"

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

    member __.Restore() = restore()

    interface IDisposable with 
        member __.Dispose() = restore()

module internal Cursor =
    let wait() = new CursorOverrideHandle(System.Windows.Input.Cursors.Wait)

/// Try to run a given function, resorting to a default value if it throws exceptions
let protectOrDefault f defaultVal =
    try
        f()
    with e ->
        Logging.logException e
        defaultVal

/// Try to run a given async computation, catch and log its exceptions
let protectAsync a =
    async {
        let! res = Async.Catch a
        return 
            match res with 
            | Choice1Of2 () -> ()
            | Choice2Of2 e ->
                Logging.logException e
                ()
    }

/// Try to run a given function and catch its exceptions
let protect f = protectOrDefault f ()

/// Execute a function and record execution time
let time label f =
    let sw = Stopwatch.StartNew()
    let result = f()
    sw.Stop()
    debug "%s took: %i ms" label sw.ElapsedMilliseconds
    result

type IServiceProvider with
    /// Go to exact location in a given file.
    member serviceProvider.NavigateTo (fileName, startRow, startCol, endRow, endCol) =
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>
        let isOpened = 
            VsShellUtilities.IsDocumentOpen(
                serviceProvider, 
                fileName, 
                Constants.guidLogicalTextView,
                &hierarchy,
                &itemId,
                &windowFrame)
        let canShow = 
            if isOpened then true
            else
                // TODO: track the project that contains document and open document in project context
                try
                    VsShellUtilities.OpenDocument(
                        serviceProvider, 
                        fileName, 
                        Constants.guidLogicalTextView, 
                        &hierarchy,
                        &itemId,
                        &windowFrame)
                    true
                with _ -> false
        if canShow then
            windowFrame.Show()
            |> ensureSucceeded

            let vsTextView = VsShellUtilities.GetTextView windowFrame
            let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
            let mutable vsTextBuffer = Unchecked.defaultof<_>
            vsTextView.GetBuffer (&vsTextBuffer)
            |> ensureSucceeded

            vsTextManager.NavigateToLineAndColumn (vsTextBuffer, ref Constants.guidLogicalTextView, startRow, startCol, endRow, endCol)
            |> ensureSucceeded

    /// Get the IWPFTextView of a document if it is open
    member serviceProvider.GetWPFTextViewOfDocument fileName =
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>
        if VsShellUtilities.IsDocumentOpen
               (serviceProvider, fileName, Constants.guidLogicalTextView, &hierarchy, &itemId, &windowFrame) then
            let vsTextView = VsShellUtilities.GetTextView windowFrame 
            let componentModel = serviceProvider.GetService<IComponentModel, SComponentModel>()
            let vsEditorAdapterFactoryService =  componentModel.GetService<IVsEditorAdaptersFactoryService>()
            Some (vsEditorAdapterFactoryService.GetWpfTextView vsTextView)
        else None

    member serviceProvider.GetDte() = serviceProvider.GetService<EnvDTE.DTE, SDTE>()

open System.IO

let isSourceFile path =
    let ext = Path.GetExtension path
    String.Equals (ext, ".fsx", StringComparison.OrdinalIgnoreCase) 
    || String.Equals (ext, ".fsscript", StringComparison.OrdinalIgnoreCase) 
    || String.Equals (ext, ".fs", StringComparison.OrdinalIgnoreCase)

let isSignatureFile path =
    let ext = Path.GetExtension path
    String.Equals(ext, ".fsi", StringComparison.OrdinalIgnoreCase)

let listFSharpProjectsInSolution (dte: DTE) =
    let rec handleProject (p: Project) = 
        if p === null then []
        elif isFSharpProject p then [ p ]
        elif p.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then 
            handleProjectItems p.ProjectItems
        else []
        
    and handleProjectItems (items: ProjectItems) =
        [ for pi in items do
            yield! handleProject pi.SubProject ]

    [ for p in dte.Solution.Projects do
        yield! handleProject p ]

open Microsoft.VisualStudio.OLE.Interop

type IMenuCommand =
    inherit IOleCommandTarget
    abstract IsAdded: bool with get, set
    abstract NextTarget: IOleCommandTarget with get, set
