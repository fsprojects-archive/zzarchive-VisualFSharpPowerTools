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

[<Name(Constants.unusedDeclarationMargin)>]
type UnusedDeclarationMargin(textView: IWpfTextView, marginContainer: IWpfTextViewMargin,
                             tagAggregator: ITagAggregator<UnusedDeclarationTag>) =
    inherit Canvas()

    let children = base.Children
    let verticalScrollBar = marginContainer.GetTextViewMargin(PredefinedMarginNames.VerticalScrollBar)

    let updateDisplay () =
         if not textView.IsClosed then
            children.Clear()
            let totalLines = textView.TextSnapshot.LineCount
            let virtualAdditionalLines = int (textView.ViewportHeight / textView.LineHeight) - 1
            let lineHeight = textView.ViewportHeight / float (totalLines + virtualAdditionalLines)
            let span = SnapshotSpan(textView.TextBuffer.CurrentSnapshot, 0, textView.TextBuffer.CurrentSnapshot.Length)
            tagAggregator.GetTags(span)
            |> Seq.map (fun span -> 
                let pos = span.Tag.Range.Start.Position
                textView.TextSnapshot.GetLineNumberFromPosition(pos))
            |> Seq.distinct
            |> Seq.iter (fun lineNo ->                
                    let markerHeight = min 4.0 (lineHeight / 2.0)
                    let markerWidth = verticalScrollBar.MarginSize
                    let marker = Rectangle(Fill = Brushes.Orange, StrokeThickness = 2.0, Stroke = Brushes.DarkOrange,
                                           Height = markerHeight, Width = markerWidth)
                    // Try to place the marker on top of vertical scroll bar
                    Canvas.SetLeft(marker, -markerWidth)
                    Canvas.SetTop(marker, float lineNo * lineHeight)
                    marker.MouseDown.Add(fun _ -> 
                        let line = textView.TextSnapshot.GetLineFromLineNumber(lineNo)
                        textView.ViewScroller.EnsureSpanVisible(SnapshotSpan(line.Start, 0), EnsureSpanVisibleOptions.AlwaysCenter))
                    children.Add(marker) |> ignore)

    let docEventListener = new DocumentEventListener ([ViewChange.tagsChangedEvent tagAggregator], 200us, updateDisplay)

    interface IWpfTextViewMargin with
        member x.Enabled = 
            true
        
        member x.GetTextViewMargin(marginName: string): ITextViewMargin = 
            match marginName with
            | Constants.unusedDeclarationMargin -> 
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
[<Name(Constants.unusedDeclarationMargin)>]
[<ContentType("F#")>]
[<Order(After = PredefinedMarginNames.VerticalScrollBar)>]
[<MarginContainer(PredefinedMarginNames.VerticalScrollBarContainer)>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
type UnusedDeclarationMarginProvider() =
    [<Import>]
    member val TagAggregatorFactoryService: IViewTagAggregatorFactoryService = null with get, set
    interface IWpfTextViewMarginProvider with
        member x.CreateMargin(wpfTextViewHost: IWpfTextViewHost, marginContainer: IWpfTextViewMargin): IWpfTextViewMargin = 
            let textView = wpfTextViewHost.TextView
            new UnusedDeclarationMargin(textView, marginContainer,
                    x.TagAggregatorFactoryService.CreateTagAggregator<UnusedDeclarationTag>(textView)) :> _
        
        

        
