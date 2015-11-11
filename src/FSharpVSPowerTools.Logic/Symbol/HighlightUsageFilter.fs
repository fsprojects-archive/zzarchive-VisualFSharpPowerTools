namespace FSharpVSPowerTools.HighlightUsage

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio
open Microsoft.VisualStudio.OLE.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging

[<RequireQualifiedAccess>]
type private NeighbourKind =
    | Previous
    | Next

type HighlightUsageFilter(textView: IWpfTextView, 
                          referenceTagger: ITagAggregator<TextMarkerTag>) =

    let binarySearch compare (source: _ []) =
        let rec loop start finish =
            if start = finish then
                if compare source.[start] = 0 then Some start
                else None
            elif start > finish then
                None
            else
                let mid = (start + finish) / 2
                match compare source.[mid] with
                | 0 -> Some mid
                | result when result > 0 ->
                    loop start (mid-1)
                | _ ->
                    loop (mid+1) finish
        loop 0 (Array.length source - 1)

    let gotoReference kind =
        textView.TextBuffer.GetSnapshotPoint textView.Caret.Position
        |> Option.iter (fun caretPos ->
            let buffer = textView.TextBuffer
            let span = buffer.CurrentSnapshot.FullSpan
            let comparePosition (span: SnapshotSpan) =
                if span.Start.Position > caretPos.Position then
                    1
                elif span.Start.Position <= caretPos.Position && caretPos.Position <= span.End.Position then
                    0
                else -1

            let spans =
                referenceTagger.GetTags(span)
                |> Seq.collect (fun mappedSpan -> mappedSpan.Span.GetSpans(buffer))
                |> Seq.sortBy (fun span -> span.Start.Position)
                |> Seq.toArray
            if spans.Length > 1 then                
                binarySearch comparePosition spans
                |> Option.map (fun index ->
                    match kind with 
                    | NeighbourKind.Previous -> 
                        let previous = if index = 0 then spans.Length-1 else index-1
                        spans.[previous]
                    | NeighbourKind.Next -> 
                        let next = if index = spans.Length-1 then 0 else index+1
                        spans.[next])
                |> Option.iter (fun span ->
                    textView.Caret.MoveTo(span.Start) |> ignore
                    textView.ViewScroller.EnsureSpanVisible(span, EnsureSpanVisibleOptions.ShowStart)))

    member val IsAdded = false with get, set
    member val NextTarget: IOleCommandTarget = null with get, set

    interface IOleCommandTarget with
        member x.Exec (pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut) =
            if (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidNextHighlightedReference) then
                gotoReference NeighbourKind.Next
            elif (pguidCmdGroup = Constants.guidStandardCmdSet && nCmdId = Constants.cmdidPreviousHighlightedReference) then
                gotoReference NeighbourKind.Previous
            x.NextTarget.Exec(&pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut)

        member x.QueryStatus (pguidCmdGroup, cCmds, prgCmds, pCmdText) =
            if pguidCmdGroup = Constants.guidStandardCmdSet && 
                prgCmds |> Seq.exists (fun x -> x.cmdID = Constants.cmdidNextHighlightedReference 
                                                  || x.cmdID = Constants.cmdidPreviousHighlightedReference) then
                prgCmds.[0].cmdf <- (uint32 OLECMDF.OLECMDF_SUPPORTED) ||| (uint32 OLECMDF.OLECMDF_ENABLED)
                VSConstants.S_OK
            else
                x.NextTarget.QueryStatus(&pguidCmdGroup, cCmds, prgCmds, pCmdText)            