[<AutoOpen>]
module FSharpVSPowerTools.VSUtils

open System
open System.Text.RegularExpressions
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open FSharpVSPowerTools

/// Retrieve snapshot from VS zero-based positions
let fromVSPos (snapshot : ITextSnapshot) ((startLine, startCol), (endLine, endCol)) =
    let startPos = snapshot.GetLineFromLineNumber(startLine).Start.Position + startCol
    let endPos = snapshot.GetLineFromLineNumber(endLine).Start.Position + endCol
    SnapshotSpan(snapshot, startPos, endPos - startPos)

open Microsoft.FSharp.Compiler.PrettyNaming

let isDoubleBacktickIdent (s : string) =
    if s.StartsWith("``") && s.EndsWith("``") then
        let inner = s.Substring("``".Length, s.Length - "````".Length)
        not (inner.Contains("``"))
    else
        false

let isIdentifier (s : string) =
    if isDoubleBacktickIdent s then
        true
    else
        s |> Seq.mapi (fun i c -> i, c)
          |> Seq.forall (fun (i, c) -> 
                if i = 0 then IsIdentifierFirstCharacter c else IsIdentifierPartCharacter c) 

let isOperator (s : string) = 
    IsPrefixOperator s || IsInfixOperator s || IsTernaryOperator s

type SnapshotPoint with
    member this.FromRange(lineStart, colStart, lineEnd, colEnd) =
        fromVSPos this.Snapshot ((lineStart, colStart), (lineEnd, colEnd))

type SnapshotSpan with
    /// Return corresponding zero-based range
    member this.ToRange() =
        let lineStart = this.Snapshot.GetLineNumberFromPosition(this.Start.Position)
        let lineEnd = this.Snapshot.GetLineNumberFromPosition(this.End.Position)
        let startLine = this.Snapshot.GetLineFromPosition(this.Start.Position)
        let endLine = this.Snapshot.GetLineFromPosition(this.End.Position)
        let colStart = this.Start.Position - startLine.Start.Position
        let colEnd = this.End.Position - endLine.Start.Position
        (lineStart, colStart, lineEnd, colEnd - 1)

type ITextBuffer with
    member x.GetSnapshotPoint (position: CaretPosition) = 
        Option.ofNullable <| position.Point.GetPoint(x, position.Affinity)

open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.ComponentModelHost

// This is for updating documents after refactoring
// Reference at https://pytools.codeplex.com/SourceControl/latest#Python/Product/PythonTools/PythonToolsPackage.cs

type DocumentUpdater(serviceProvider : IServiceProvider) = 
    member __.OpenDocument(fileName : string, [<Out>] viewAdapter : byref<IVsTextView>, pWindowFrame : byref<IVsWindowFrame>) = 
        let _textMgr = Package.GetGlobalService(typedefof<SVsTextManager>) :?> IVsTextManager
        let _uiShellOpenDocument = Package.GetGlobalService(typedefof<SVsUIShellOpenDocument>) :?> IVsUIShellOpenDocument
        let hierarchy = ref null
        let itemid = ref 0u
        VsShellUtilities.OpenDocument(serviceProvider, fileName, Guid.Empty, hierarchy, itemid, &pWindowFrame, &viewAdapter)

    member this.GetBufferForDocument(fileName : string) = 
        let viewAdapter = ref null
        let frame = ref null
        this.OpenDocument(fileName, viewAdapter, frame)

        let lines = ref null
        ErrorHandler.ThrowOnFailure((!viewAdapter).GetBuffer(lines)) |> ignore

        let componentModel = Package.GetGlobalService(typedefof<SComponentModel>) :?> IComponentModel
        let adapter = componentModel.GetService<IVsEditorAdaptersFactoryService>()
        adapter.GetDocumentBuffer(!lines)

    member __.BeginGlobalUndo(key : string) = 
        let linkedUndo = Package.GetGlobalService(typedefof<SVsLinkedUndoTransactionManager>) :?> IVsLinkedUndoTransactionManager
        ErrorHandler.ThrowOnFailure(linkedUndo.OpenLinkedUndo(uint32 LinkedTransactionFlags2.mdtGlobal, key)) |> ignore
        linkedUndo

    member __.EndGlobalUndo(linkedUndo : IVsLinkedUndoTransactionManager) = 
        ErrorHandler.ThrowOnFailure(linkedUndo.CloseLinkedUndo()) |> ignore

open Microsoft.VisualStudio.Shell
open EnvDTE
open VSLangProj
open System.Diagnostics

module Dte =
    let instance(): DTE option = tryCast (Package.GetGlobalService typedefof<DTE>)
    
    let getActiveDocument() =
        let doc =
            maybe {
                let! dte = instance()
                let! doc = Option.ofNull dte.ActiveDocument
                let! item = Option.ofNull doc.ProjectItem 
                let! _ = Option.ofNull item.ContainingProject 
                return doc }
        match doc with
        | None -> fail "Should be able to find active document and active project."
        | _ -> ()
        doc

type ProjectItem with
    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun item ->
            try Option.ofNull (item.ContainingProject.Object :?> VSProject) with _ -> None)

type SolutionEvents() =
    let projectChanged = Event<_>()
    // we must keep a reference to the events in order to prevent GC to collect it
    let dte = Dte.instance()
    let events: EnvDTE80.Events2 option = dte |> Option.bind (fun dte -> tryCast dte.Events)
    
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

    static let instance = lazy (SolutionEvents())
    static member Initialize() = () // instance.Force()
    static member Instance = instance.Value
    /// Raised when any project in solution has changed.
    member x.ProjectChanged = projectChanged.Publish