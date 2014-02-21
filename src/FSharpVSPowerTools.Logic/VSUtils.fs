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

/// Retrieve snapshot from VS zero-based positions
let fromVSPos (snapshot : ITextSnapshot) ((startLine, startCol), (endLine, endCol)) =
    let startPos = snapshot.GetLineFromLineNumber(startLine).Start.Position + startCol
    let endPos = snapshot.GetLineFromLineNumber(endLine).Start.Position + endCol
    SnapshotSpan(snapshot, startPos, endPos - startPos)

open Microsoft.FSharp.Compiler.PrettyNaming

let (|Identifier|Operator|Other|) (s: string) =
    if s |> Seq.mapi (fun i c -> i, c)
         |> Seq.forall (fun (i, c) -> 
              if i = 0 then IsIdentifierFirstCharacter c
              else IsIdentifierPartCharacter c)
    then Identifier
    elif IsOpName s && s <> "."
    then Operator
    else Other

let commonInsignificantChars = set [','; '('; ')'; ']']
let identInsignificantChars = commonInsignificantChars |> Set.union (set ['.'; '<'; '>'])
let operatorInsignificantChars = commonInsignificantChars

let isInsignificant (text: string) (insignificantChars: Set<char>) =
    not (String.IsNullOrEmpty text) && insignificantChars |> Set.contains text.[0] |> not

type SnapshotPoint with
    member this.WordExtentIsValid(word : TextExtent) =
        let text = word.Span.GetText()
        match word.IsSignificant, text with
        | true, Identifier -> isInsignificant text identInsignificantChars
        | true, Operator -> isInsignificant text operatorInsignificantChars
        | _ -> false
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
