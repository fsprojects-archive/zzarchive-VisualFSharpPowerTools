namespace FSharpVSPowerTools

open System.ComponentModel.Composition
open System.Windows
open System.Windows.Shapes
open System.Windows.Media
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open System.Windows.Controls
open FSharpVSPowerTools.ProjectSystem
open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging

[<Name("UnusedDeclarationMargin")>]
type UnusedDeclarationMargin(textView: IWpfTextView, tagAggregator: ITagAggregator<UnusedDeclarationTag>) as self =
    inherit Canvas()

    let updateDisplay () =
         if not textView.IsClosed then
            self.Children.Clear()
            let span = SnapshotSpan(textView.TextBuffer.CurrentSnapshot, 0, textView.TextBuffer.CurrentSnapshot.Length)
            tagAggregator.GetTags(span)
            |> Seq.map (fun span -> 
                let pos = span.Tag.Range.Start.Position
                textView.TextSnapshot.GetLineNumberFromPosition(pos))
            |> Seq.distinct
            |> Seq.iter (fun line ->                
                    let markerHeight = textView.LineHeight
                    let markerWidth = 20.0
                    let marker = Rectangle(Fill = Brushes.Yellow, StrokeThickness = 2.0, Stroke = Brushes.Yellow,
                                           Height = markerHeight, Width = markerWidth)
                    Canvas.SetLeft(marker, 0.0)
                    Canvas.SetTop(marker, float line * markerHeight)
                    self.Children.Add(marker) |> ignore)

    let docEventListener = new DocumentEventListener ([ViewChange.tagsChangedEvent tagAggregator], 200us, updateDisplay)

    interface IWpfTextViewMargin with
        member x.Enabled = 
            true
        
        member x.GetTextViewMargin(marginName: string): ITextViewMargin = 
            match marginName with
            | "UnusedDeclarationMargin" -> 
                x :> _
             | _ -> 
                null
        
        member x.MarginSize: float = 
            x.ActualHeight
        
        member x.VisualElement: FrameworkElement = 
            x :> _

        member x.Dispose() = 
            (docEventListener :> IDisposable).Dispose()

[<Export(typeof<IWpfTextViewMarginProvider>)>]
[<Name("UnusedDeclarationMargin")>]
[<ContentType("F#")>]
[<Order(After = PredefinedMarginNames.VerticalScrollBar)>]
[<MarginContainer(PredefinedMarginNames.VerticalScrollBarContainer)>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
type UnusedDeclarationMarginProvider() =
    [<Import>]
    member val TagAggregatorFactoryService: IViewTagAggregatorFactoryService = null with get, set
    interface IWpfTextViewMarginProvider with
        member x.CreateMargin(wpfTextViewHost: IWpfTextViewHost, _marginContainer: IWpfTextViewMargin): IWpfTextViewMargin = 
            let textView = wpfTextViewHost.TextView
            new UnusedDeclarationMargin(textView, x.TagAggregatorFactoryService.CreateTagAggregator<UnusedDeclarationTag>(textView)) :> _
        
        

        
