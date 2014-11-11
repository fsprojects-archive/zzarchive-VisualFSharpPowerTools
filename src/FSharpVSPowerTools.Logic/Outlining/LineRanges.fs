namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools
open OutliningExtensions


[<Struct>]
type LineRange =
    val StartLineNumber   : int
    val Count             : int

    new ( startline, count ) =
        { StartLineNumber   = startline 
          Count             = count     }

    member x.LastLineNumber 
        with get() = x.StartLineNumber + x.Count - 1

    member x.LineNumbers 
        with get() = seq { x.StartLineNumber .. x.Count }

    member x.ContainsLineNumber (lineNumber:int) =
        lineNumber >= x.StartLineNumber &&
        lineNumber <= x.LastLineNumber

    member x.Contains (lineRange:LineRange) =
        x.StartLineNumber <= lineRange.StartLineNumber &&
        x.LastLineNumber  >= lineRange.LastLineNumber
    
    member x.Intersects (lineRange:LineRange) =
        (x.ContainsLineNumber(lineRange.StartLineNumber))||
        (x.ContainsLineNumber(lineRange.LastLineNumber ))||
        (x.LastLineNumber+1 = lineRange.StartLineNumber )||
        (x.StartLineNumber  = lineRange.LastLineNumber+1)

    override x.ToString() = sprintf "[{%A}-{%A}]" x.StartLineNumber x.LastLineNumber

    static member CreateFromBounds startLineNumber lastLineNumber =
        if lastLineNumber < startLineNumber then 
            failwithf "LastLineNumber %A cannot be lower than StartLineNumber %A" 
                          startLineNumber                          lastLineNumber
        else
            let count = (lastLineNumber - startLineNumber) + 1
            LineRange( startLineNumber, count )

    static member CreateOverarching (left:LineRange) (right:LineRange) =
            let startLineNumber = Math.Min(left.StartLineNumber, right.StartLineNumber);
            let lastLineNumber  = Math.Max(left.LastLineNumber, right.LastLineNumber);
            LineRange.CreateFromBounds startLineNumber lastLineNumber



[<Struct; CustomEquality; NoComparison >]
type SnapshotLineRange =
    val Snapshot        : ITextSnapshot
    val StartLineNumber : int
    val Count           : int

    new ( snapshot, startline, count ) =
        {   Snapshot        = snapshot
            StartLineNumber = startline 
            Count           = count      }
        then 
            if startline >= snapshot.LineCount then 
                failwithf "The startline %A cannot be greater than the lenght of the Snapshot %A" 
                                    startline                                 snapshot.LineCount

    member x.Startline 
        with get() = x.Snapshot.GetLineFromLineNumber x.StartLineNumber

    member x.Start 
        with get() = x.Startline.Start

    member x.LastLineNumber 
        with get() = x.StartLineNumber + x.Count - 1 

    member x.LastLine
        with get() = x.Snapshot.GetLineFromLineNumber x.LastLineNumber

    member x.LineRange 
        with get() = LineRange()

    member x.End 
        with get() = x.LastLine.End

    member x.EndIncludingLineBreak 
        with get() = x.LastLine.EndIncludingLineBreak

    member x.Extent
        with get() = SnapshotSpan( x.Start, x.End )

    member x.ExtentIncludingLineBreak
        with get() = SnapshotSpan( x.Start, x.EndIncludingLineBreak)

    member x.Lines 
        with get() = seq{ x.StartLineNumber .. x.Count } |> Seq.map x.Snapshot.GetLineFromLineNumber

    member x.GetText() = x.Extent.GetText()

    member x.GetTextIncludingLineBreak() = x.ExtentIncludingLineBreak.GetText()

    override x.GetHashCode() = pown x.StartLineNumber x.Count
    
    override x.Equals other =
        match other with 
        | :? SnapshotLineRange as o ->  
                o.Snapshot          = x.Snapshot        &&
                o.StartLineNumber   = x.StartLineNumber &&
                o.Count             = x.Count
        | _ -> false
    
    override x.ToString() = sprintf "[{%A}-{%A}] %A" x.StartLineNumber x.LastLineNumber x.Snapshot
 

    static member CreateForLine (snapshotLine:ITextSnapshotLine)  =
        SnapshotLineRange( snapshotLine.Snapshot, snapshotLine.LineNumber, 1 )
 
                                      
    static member CreateForExtent (snapshot:ITextSnapshot) =
        SnapshotLineRange( snapshot, 0, snapshot.LineCount )                                     
 

    static member CreateForLineRange (startLine:ITextSnapshotLine) (lastLine:ITextSnapshotLine) =
        if startLine.Snapshot <> lastLine.Snapshot then
            failwithf "The snapshot of %A does not equal the snapshot of %A" 
                startLine.Snapshot  lastLine.Snapshot
        else
            let count = lastLine.LineNumber - startLine.LineNumber + 1
            SnapshotLineRange( startLine.Snapshot, startLine.LineNumber, count )


    static member CreateForSpan (span:SnapshotSpan) =
        let startLine = span.GetStartLine() 
        let lastLine  = span.GetLastLine()
        SnapshotLineRange.CreateForLineRange startLine lastLine 


    static member CreateForLineAndMaxCount (snapshotLine:ITextSnapshotLine) (count:int) =
        let maxCount = snapshotLine.Snapshot.LineCount - snapshotLine.LineNumber
        let count' = Math.Min(count,maxCount)
        SnapshotLineRange(snapshotLine.Snapshot, snapshotLine.LineNumber, count')


