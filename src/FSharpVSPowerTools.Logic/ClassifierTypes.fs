namespace FSharpVSPowerTools.SyntaxColoring

open Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler

[<NoComparison>]
type internal SnapshotSpanWithLine =
    { Span: SnapshotSpan 
      Line: int }

[<Sealed>]
type internal CategorizedSnapshotSpan (columnSpan: CategorizedColumnSpan<ITextSnapshot>, originalSnapshot: ITextSnapshot) =
    let snapshotSpan: SnapshotSpanWithLine option Atom = Atom None 
    member __.ColumnSpan = columnSpan
    member __.GetSnapshotSpan targetSnapshot = 
        snapshotSpan.Swap (fun oldSpan ->
            oldSpan
            |> Option.orTry (fun _ -> 
                fromRange originalSnapshot columnSpan.WordSpan.Range.Value
                |> Option.map (fun span -> 
                    { Span = span
                      Line = span.Start.GetContainingLine().LineNumber }))
            |> Option.map (fun span ->
                if span.Span.Snapshot <> targetSnapshot then
                    let newSpan = span.Span.TranslateTo(targetSnapshot, SpanTrackingMode.EdgeExclusive)
                    { Span = newSpan; Line = newSpan.Start.GetContainingLine().LineNumber }
                else span)) 
        |> ignore
        snapshotSpan.Value

[<NoComparison>]
type internal CategorizedSnapshotSpans =
    { Spans: CategorizedSnapshotSpan[]
      Errors: FSharpErrorInfo[] }

[<AutoOpen>]
module internal Utils =
    open Microsoft.VisualStudio.Text.Classification

    let log (f: unit -> string) = Logging.logInfo (fun _ -> "[SymbolClassifier] " + f()) 

    let getClassificationType (classificationRegistry: IClassificationTypeRegistryService) = 
        memoize <| fun cat ->
            match cat with
            | Category.ReferenceType -> Some Constants.fsharpReferenceType
            | Category.ValueType -> Some Constants.fsharpValueType
            | Category.PatternCase -> Some Constants.fsharpPatternCase
            | Category.Function -> Some Constants.fsharpFunction
            | Category.MutableVar -> Some Constants.fsharpMutableVar
            | Category.Quotation -> Some Constants.fsharpQuotation
            | Category.Module -> Some Constants.fsharpModule
            | Category.Unused -> Some Constants.fsharpUnused
            | Category.Printf -> Some Constants.fsharpPrintf
            | Category.Escaped -> Some Constants.fsharpEscaped
            | Category.Operator -> Some Constants.fsharpOperator
            | _ -> None
            |> Option.map classificationRegistry.GetClassificationType

    let getClassificationSpans 
        (spans: CategorizedSnapshotSpans) 
        (targetSnapshotSpan: SnapshotSpan) 
        (classificationRegistry: IClassificationTypeRegistryService) =

        let spanStartLine = targetSnapshotSpan.Start.GetContainingLine().LineNumber
        let widenSpanStartLine = max 0 (spanStartLine - 10)
        let spanEndLine = targetSnapshotSpan.End.GetContainingLine().LineNumber
        spans.Spans
        // Locations are sorted, so we can safely filter them efficiently.
        // Skip spans that's not are potential candidates for return (we widen the range 
        // because spans may shift to up after translation).
        |> Seq.skipWhile (fun span -> span.ColumnSpan.WordSpan.Line < widenSpanStartLine)
        |> Seq.choose (fun snapshotSpan ->
            maybe {
                let! clType = getClassificationType classificationRegistry snapshotSpan.ColumnSpan.Category
                let! span = snapshotSpan.GetSnapshotSpan targetSnapshotSpan.Snapshot
                return clType, span
            })
        |> Seq.takeWhile (fun (_, span) -> span.Line <= spanEndLine)
        // Because we may translate spans above, some of them may not be contained in the requested `SnapshotSpan`.
        |> Seq.filter (fun (_, span) -> targetSnapshotSpan.Contains span.Span)
        |> Seq.map (fun (clType, span) -> ClassificationSpan (span.Span, clType))
        |> Seq.toArray

    let isTypeCheckerCategory = function
        | Category.ReferenceType
        | Category.ValueType
        | Category.PatternCase
        | Category.Function
        | Category.MutableVar
        | Category.Module -> true
        | Category.Quotation
        | Category.Unused
        | Category.Printf
        | Category.Escaped
        | Category.Operator
        | Category.Other -> false

    let mergeSpans (oldSpans: CategorizedSnapshotSpans) (newSpans: CategorizedSnapshotSpans) =
        let getLineRange includingUnused (spans: CategorizedSnapshotSpan[]) =
            let typeCheckerSpans = 
                spans 
                |> Array.filter (fun x -> 
                    isTypeCheckerCategory x.ColumnSpan.Category
                    || (includingUnused && x.ColumnSpan.Category = Category.Unused))
            typeCheckerSpans,
            typeCheckerSpans
            |> Array.map (fun x -> x.ColumnSpan.WordSpan.Line)
            |> function [||] -> -1, -1 | lines -> Array.min lines, Array.max lines

        // we take into account new Unused spans, but do not old ones.
        let newTcSpans, (newStartLine, newEndLine) = getLineRange true newSpans.Spans
        let oldTcSpans, (oldStartLine, oldEndLine) = getLineRange false oldSpans.Spans
        let isNewRangeLarger = newStartLine <= oldStartLine && newEndLine >= oldEndLine

        // returns `true` if both first and last spans are still here, which means
        // that new spans are not produced from partially valid source file.
        let haveFirstAndLastSpansNotChanged() =
            let sameWordSpan x y =
                x.SymbolKind = y.SymbolKind
                && x.StartCol = y.StartCol
                && x.EndCol = y.EndCol
            match newTcSpans, oldTcSpans with
            | [||], [||] -> true
            | _, [||] | [||], _ -> false
            | x, y ->
                sameWordSpan x.[0].ColumnSpan.WordSpan y.[0].ColumnSpan.WordSpan
                && sameWordSpan x.[x.Length - 1].ColumnSpan.WordSpan y.[y.Length - 1].ColumnSpan.WordSpan
        
        match newSpans.Errors with
        | [||] ->
            log (fun _ -> "Replace spans entirely because new spans has no errors.")
            newSpans
        | _ ->
            log (fun _ -> sprintf "FCS returns errors:\n %+A" newSpans.Errors)

            if isNewRangeLarger then
                log (fun _ -> 
                    sprintf "Replace spans entirely because new span range is wider than old one (old lines = %d..%d, new lines = %d..%d)." 
                            oldStartLine oldEndLine newStartLine newEndLine)
                newSpans
            elif haveFirstAndLastSpansNotChanged() then
                log (fun _ -> "Replace spans entirely because first and last spans have not changed.")
                newSpans
            else
                log (fun _ -> sprintf "Merging spans (new range %A <= old range %A)." 
                                      (newStartLine, newEndLine) (oldStartLine, oldEndLine))
                let spans = 
                    seq { 
                        yield! oldSpans.Spans |> Seq.takeWhile (fun x -> x.ColumnSpan.WordSpan.Line < newStartLine)
                        yield! oldSpans.Spans |> Seq.skipWhile (fun x -> x.ColumnSpan.WordSpan.Line <= newEndLine) 
                        yield! newSpans.Spans
                    }
                    |> Seq.sortBy (fun x -> x.ColumnSpan.WordSpan.Line)
                    |> Seq.toArray
                { Spans = spans; Errors = newSpans.Errors }

