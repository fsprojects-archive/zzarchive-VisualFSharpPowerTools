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

let isIdentifier (s : string) =
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

module Dte =
    let getActiveDocument() =
        let dte = Package.GetGlobalService(typedefof<DTE>) :?> DTE
        let doc = dte.ActiveDocument
        System.Diagnostics.Debug.Assert(doc <> null && doc.ProjectItem.ContainingProject <> null, 
                                        "Should be able to find active document and active project.")
        doc
