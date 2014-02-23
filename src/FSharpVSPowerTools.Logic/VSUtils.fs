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
