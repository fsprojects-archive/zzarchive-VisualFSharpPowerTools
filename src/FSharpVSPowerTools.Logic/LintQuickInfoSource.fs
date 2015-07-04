namespace FSharpVSPowerTools.Linting

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Language.Intellisense
open System.Collections.Generic
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open System.Reflection
open System.Windows.Controls
open System.Windows.Documents
open System.Windows

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
    
    let findColorResource name =
        String.Format ("Microsoft.VisualStudio.Shell.{0}.0, Version={0}.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a", 12)
        |> Assembly.Load
        |> Option.ofNull
        |> Option.map (fun assembly ->
            let ty = assembly.GetType "Microsoft.VisualStudio.PlatformUI.EnvironmentColors"
            let prop = ty.GetProperty name
            prop.GetValue(null, null))

    let createInfoText (tooltips: string list) : UIElement =
        let textBlock = TextBlock()
        tooltips |> List.iteri (fun i tooltip ->
            textBlock.Inlines.Add (Bold (Run "Lint: "))
            textBlock.Inlines.Add (Run tooltip)
            if i < tooltips.Length - 1 then
                textBlock.Inlines.Add (LineBreak()))
             
        findColorResource "ToolTipBrushKey"
        |> Option.iter (fun key -> textBlock.SetResourceReference(TextBlock.BackgroundProperty, key))

        findColorResource "ToolTipTextBrushKey"
        |> Option.iter (fun key -> textBlock.SetResourceReference(TextBlock.ForegroundProperty, key))

        upcast textBlock

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
                    res |> List.map snd |> createInfoText |> quickInfoContent.Add
        
        member x.Dispose(): unit = 
            if not disposed then
                GC.SuppressFinalize x
                disposed <- true
                //(docEventListener :> IDisposable).Dispose()
