namespace FSharpVSPowerTools

open System.Windows
open System.Windows.Shapes
open System.Windows.Media
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open System.Windows.Controls
open FSharpVSPowerTools.ProjectSystem
open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open System.Windows.Input

[<Name(Constants.fsharpUnusedDeclarationMargin)>]
type UnusedDeclarationMargin(textView: IWpfTextView, 
                             marginContainer: IWpfTextViewMargin,
                             classifier: IClassifier) =
    inherit Canvas()

    let children = base.Children
    let verticalScrollMargin = marginContainer.GetTextViewMargin(PredefinedMarginNames.VerticalScrollBar)
    let mutable markerData = Unchecked.defaultof<_>
    let markerBrush = SolidColorBrush(Color.FromRgb(255uy, 165uy, 0uy))

    let updateDisplay () = 
        if not textView.IsClosed then
            let span = SnapshotSpan(textView.TextBuffer.CurrentSnapshot, 0, textView.TextBuffer.CurrentSnapshot.Length)
            let ctx = System.Threading.SynchronizationContext.Current
            async {
                let data =
                    classifier.GetClassificationSpans(span)
                    |> Seq.choose (fun classification -> 
                        if classification.ClassificationType.Classification.StartsWith (Constants.fsharpUnused, StringComparison.Ordinal) then
                            let pos = classification.Span.Start.Position
                            Some (textView.TextSnapshot.GetLineNumberFromPosition(pos), pos)
                        else None)
                    |> Seq.distinctBy fst
                    |> Seq.toArray
            
                do! Async.SwitchToContext ctx
                markerData <- data
                children.Clear()
                let verticalScrollBar = verticalScrollMargin :?> IVerticalScrollBar
                let lineStart = verticalScrollBar.TrackSpanTop
                let totalLines = textView.TextSnapshot.LineCount
                let lineHeight = verticalScrollBar.TrackSpanHeight / float totalLines
                for (lineNo, pos) in markerData do               
                    let markerHeight = 3.0
                    let markerMargin = 2.0
                    // Ensure that marker with is non-negative, otherwise Rectangle.Width throws ArgumentException.
                    let markerWidth = max 3.0 (verticalScrollMargin.MarginSize - markerMargin * 2.0)
                    let marker = Rectangle(Fill = Brushes.Orange, StrokeThickness = 0.5, Stroke = markerBrush,
                                            Height = markerHeight, Width = markerWidth,
                                            Cursor = Cursors.Hand, ToolTip = sprintf "Unused declaration(s) at line %i" (lineNo + 1))
                    // Try to place the marker on top of vertical scroll bar
                    Canvas.SetLeft(marker, - (markerWidth + markerMargin))
                    Canvas.SetTop(marker, lineStart + float lineNo * lineHeight)
                    marker.MouseDown.Add(fun _ -> 
                        let line = textView.TextSnapshot.GetLineFromLineNumber(lineNo)
                        textView.Caret.MoveTo(VirtualSnapshotPoint(textView.TextSnapshot, pos)) |> ignore
                        textView.ViewScroller.EnsureSpanVisible(SnapshotSpan(line.Start, 0), EnsureSpanVisibleOptions.ShowStart))
                    children.Add(marker) |> ignore
            } |> Async.StartInThreadPoolSafe

    let docEventListener = new DocumentEventListener ([ViewChange.viewportHeightEvent textView; ViewChange.classificationEvent classifier], 
                                   200us, updateDisplay)

    interface IWpfTextViewMargin with
        member __.Enabled = 
            true
        
        member x.GetTextViewMargin(marginName: string): ITextViewMargin = 
            if marginName = Constants.fsharpUnusedDeclarationMargin then
                x :> _
            else null
        
        member x.MarginSize: float = 
            x.ActualHeight
        
        member x.VisualElement: FrameworkElement = 
            x :> _

        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
