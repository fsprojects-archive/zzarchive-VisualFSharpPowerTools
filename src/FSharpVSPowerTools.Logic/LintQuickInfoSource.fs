namespace FSharpVSPowerTools.Linting

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Language.Intellisense
open System.Collections.Generic
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools

type LintQuickInfoSource(buffer: ITextBuffer, tagAggregatorService: IViewTagAggregatorFactoryService) =
    let mutable disposed = false
//    let mutable data = []
//    
//    let update () = 
//        protect <| fun _ ->
//            let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
//            
//            data <-
//                tagAggregator.GetTags(span)
//                |> Seq.map (fun mappedSpan -> 
//                    let tooltip = mappedSpan.Tag.ToolTipContent :?> string
//                    mappedSpan.Span.GetSpans buffer |> Seq.map (fun span -> span, tooltip))
//                |> Seq.concat
//                |> Seq.toList
//
//    let docEventListener = new DocumentEventListener ([ViewChange.tagsEvent tagAggregator], 200us, update)

    interface IQuickInfoSource with
        member __.AugmentQuickInfoSession (session: IQuickInfoSession, quickInfoContent: IList<obj>, 
                                           applicableToSpan: byref<ITrackingSpan>) = 
            match session.GetTriggerPoint buffer.CurrentSnapshot |> Option.ofNullable with
            | None -> ()
            | Some point ->
                use tagAggregator = tagAggregatorService.CreateTagAggregator<LintTag> session.TextView
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                let res =
                    tagAggregator.GetTags(span)
                    |> Seq.map (fun mappedSpan -> 
                        let tooltip = mappedSpan.Tag.ToolTipContent :?> string
                        mappedSpan.Span.GetSpans buffer |> Seq.map (fun span -> span, tooltip))
                    |> Seq.concat
                    |> Seq.toList
                    |> List.filter (fun (span, _) -> point.InSpan span)

                match res with
                | [] -> ()
                | (span, _) :: _ ->
                    applicableToSpan <- buffer.CurrentSnapshot.CreateTrackingSpan (span.Span, SpanTrackingMode.EdgeExclusive)
                    for _, tooltip in res do
                        quickInfoContent.Add tooltip
        
        member x.Dispose(): unit = 
            if not disposed then
                GC.SuppressFinalize x
                disposed <- true
                //(docEventListener :> IDisposable).Dispose()
                //tagAggregator.Dispose()
