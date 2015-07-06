namespace FSharpVSPowerTools.Linting

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Language.Intellisense
open System.Collections.Generic
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharpVSPowerTools.Common
open System.Windows.Controls
open System.Windows.Documents
open System.Windows

type LintQuickInfoSource(buffer: ITextBuffer, viewTagAggregatorFactoryService: IViewTagAggregatorFactoryService) =
    let createInfoText (tooltips: string list) : UIElement =
        let textBlock = TextBlock() 
        tooltips |> List.iteri (fun i tooltip ->
            textBlock.Inlines.Add (Bold (Run "Lint: "))
            textBlock.Inlines.Add (Run tooltip)
            if i < tooltips.Length - 1 then
                textBlock.Inlines.Add (LineBreak()))
             
        textBlock.SetResourceReference(TextBlock.BackgroundProperty, VSColors.ToolTipBrushKey)
        textBlock.SetResourceReference(TextBlock.ForegroundProperty, VSColors.ToolTipTextBrushKey)
        upcast textBlock

    let mutable tagAggregator = None
    let getTagAggregator textView =
        match tagAggregator with
        | None -> 
            let aggregator = viewTagAggregatorFactoryService.CreateTagAggregator<LintTag> textView
            tagAggregator <- Some aggregator
            aggregator
        | Some x -> x

    interface IQuickInfoSource with
        member __.AugmentQuickInfoSession (session: IQuickInfoSession, quickInfoContent: IList<obj>, 
                                           applicableToSpan: byref<ITrackingSpan>) = 
            match session.GetTriggerPoint buffer.CurrentSnapshot |> Option.ofNullable with
            | None -> ()
            | Some point ->
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                let res =
                    let tags = getTagAggregator(session.TextView).GetTags span
                    tags
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
                    res |> List.map snd |> createInfoText |> quickInfoContent.Add
        
        member __.Dispose() = tagAggregator |> Option.iter (fun x -> x.Dispose())