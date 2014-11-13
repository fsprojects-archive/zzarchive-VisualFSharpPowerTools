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

[<AutoOpen>]
module SpanExtensions =
    type Span with
        
        static member CreateOverarching ( left:Span) (right:Span) =
            let start   = Math.Min( left.Start, right.Start )
            let finish  = Math.Max( left.End  , right.End   )
            Span.FromBounds ( start, finish )


    type SnapshotSpan with 
        
        member x.GetStartLine() =  x.Start.GetContainingLine()
        member x.GetLastLine () =  x.End.GetContainingLine()

        static member CreateOverarching (left:SnapshotSpan)(right:SnapshotSpan) =
            if left.Snapshot <> right.Snapshot then
                failwithf "left Snapshot %A does not equal right Snapshot %A"
                            left                        right
            else
                let span = Span.CreateOverarching (left.Span) (right.Span)
                SnapshotSpan(left.Snapshot, span)

/// <summary>
/// A simple line range 
/// </summary>
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


/// <summary>
/// Represents a range of lines in an ITextSnapshot.  Different from a SnapshotSpan
/// because it declaratively supports lines instead of a position range
/// </summary>
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
 
    /// <summary>
    /// Create for a single ITextSnapshotLine
    /// </summary>
    static member CreateForLine (snapshotLine:ITextSnapshotLine)  =
        SnapshotLineRange( snapshotLine.Snapshot, snapshotLine.LineNumber, 1 )
 
    /// <summary>
    /// Create for the entire ITextSnapshot
    /// </summary>                                    
    static member CreateForExtent (snapshot:ITextSnapshot) =
        SnapshotLineRange( snapshot, 0, snapshot.LineCount )                                     
 
    /// <summary>
    /// Create a SnapshotLineRange which includes the 2 lines
    /// </summary>
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

    /// <summary>
    /// Create a range for the provided ITextSnapshotLine and with at most count 
    /// length.  If count pushes the range past the end of the buffer then the 
    /// span will go to the end of the buffer
    /// </summary>
    static member CreateForLineAndMaxCount (snapshotLine:ITextSnapshotLine) (count:int) =
        let maxCount = snapshotLine.Snapshot.LineCount - snapshotLine.LineNumber
        let count' = Math.Min(count,maxCount)
        SnapshotLineRange(snapshotLine.Snapshot, snapshotLine.LineNumber, count')



    /// <summary>
    /// Create a SnapshotLineRange which includes the 2 lines
    /// </summary>
    static member CreateForLineNumberRange (snapshot:ITextSnapshot) (startLine:int) (lastLine:int) =
        if startLine >= lastLine then
            failwithf "The startline[%A] cannot be greater than the endline[%A]" 
                            startLine                               lastLine
        elif startLine >= snapshot.LineCount || lastLine >= snapshot.LineCount then None else
        Some <| SnapshotLineRange(snapshot, startLine, (lastLine - startLine) + 1)





/// <summary>
/// The goal of this collection is to efficiently track the set of LineRange values that have 
/// been visited for a given larger LineRange.  The order in which, or original granualarity
/// of visits is less important than the overall range which is visited.  
/// 
/// For example if both ranges 1-3 and 2-5 are visited then the collection will only record
/// that 1-5 is visited. 
/// </summary>
type NormalizedLineRangeCollection( visited:IEnumerable<LineRange>) as self =
    
    let list = List<LineRange>()

    do  
        visited |> Seq.iter ( fun lineRange -> list.Add lineRange )

    new() = NormalizedLineRangeCollection(Enumerable.Empty())

    member __.List with get() = list

    member __.FindInsertionPoint (startLineNumber:int) =
        let rec loop cnt =
            if cnt < list.Count then 
                match startLineNumber with
                | x when x <= list.[cnt].StartLineNumber -> cnt
                | x  -> loop cnt+1
            else -1
        loop 0

    /// <summary>
    /// This is the helper method for Add which will now collapse elements that intersect.   We only have
    /// to look at the item before the insert and all items after and not the entire collection.
    /// </summary>
    member __.CollapseIntersecting (index:int) =
            // It's possible this new LineRange actually intersects with the LineRange before
            // the insertion point.  LineRange values are ordered by start line.  Hence the LineRange
            // before could have an extent which intersects the new LineRange but not the previous 
            // LineRange at this index.  Do a quick check for this and if it's true just start the
            // collapse one index backwards
            let lineRange = list.[index]
            if index > 0 && list.[index - 1].Intersects(lineRange) then
                self.CollapseIntersecting(index - 1)

            let current = index + 1
            let removeCount = 0

            let rec loop current removeCount =
                if current < list.Count then
                    let currentLineRange = list.[current]
                    if not <| lineRange.Intersects currentLineRange then ()
                    else 
                        let lineRange' = LineRange.CreateOverarching lineRange currentLineRange
                        list.[index] <- lineRange'
                        loop (current+1) (removeCount+1)
            loop current removeCount

            if (removeCount > 0) then list.RemoveRange(index + 1, removeCount)


    member __.Add(lineRange:LineRange) =
        let index = self.FindInsertionPoint lineRange.StartLineNumber
        if index = 1 then 
            // Just insert at the end and let the collapse code do the work in this case 
            list.Add lineRange 
            self.CollapseIntersecting (list.Count-1)
        else
            // Quick optimization check to avoid copying the contents of the List
            // structure down on insert
            let item = list.[index]

            if item.StartLineNumber = lineRange.StartLineNumber     ||
               lineRange.ContainsLineNumber(item.StartLineNumber) then
                    list.[index] <- LineRange.CreateOverarching item lineRange
            else
                list.Insert(index, lineRange)
                self.CollapseIntersecting(index)


    member __.OverarchingLineRange 
        with get() = 
            if   list.Count = 0 then None
            elif list.Count = 1 then Some list.[0]
            else 
                let startLine = list.[0].StartLineNumber
                let lastLine = list.[list.Count-1].LastLineNumber
                Some <| LineRange.CreateFromBounds startLine lastLine
    
    member __.Count with get() = list.Count
    
    member __.Item with get(i:int) = list.[i]

    member __.Contains (lineRange:LineRange) =
        list.Any(fun current -> current.Contains lineRange )

    member __.Clear() = list.Clear()

    member __.Copy() = NormalizedLineRangeCollection(list)

    member __.GetUnvisited(lineRange:LineRange) =
        let rec loop cnt =
            if cnt > list.Count then Some lineRange else
            match list.[cnt] with
            | elm when not<|elm.Intersects lineRange -> loop (cnt+1) 
            | elm when elm.Contains lineRange        -> None
            | elm when elm.StartLineNumber 
                    <= lineRange.StartLineNumber ->
                        Some <| LineRange.CreateFromBounds (elm.LastLineNumber + 1  ) 
                                                           (lineRange.LastLineNumber)
            | elm when elm.StartLineNumber 
                    >  lineRange.StartLineNumber ->
                        Some <| LineRange.CreateFromBounds (lineRange.StartLineNumber)
                                                           (elm.StartLineNumber - 1  ) 
            | _ -> loop (cnt+1 )
        loop 0

    interface IEnumerable<LineRange> with

        member x.GetEnumerator(): Collections.IEnumerator = 
            list.GetEnumerator() :> Collections.IEnumerator
        
        member x.GetEnumerator(): IEnumerator<LineRange> = 
            list.GetEnumerator() :> IEnumerator<LineRange>
