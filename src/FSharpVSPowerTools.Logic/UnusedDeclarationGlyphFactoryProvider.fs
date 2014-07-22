namespace FSharpVSPowerTools

open System.ComponentModel.Composition
open System.Windows
open System.Windows.Shapes
open System.Windows.Media
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities

type UnusedDeclarationGlyphFactory() =
    static let glyphSize = 5.0
    interface IGlyphFactory with
        member x.GenerateGlyph(line: IWpfTextViewLine, tag: IGlyphTag): UIElement = 
            match tag with
            | null | _ when not (tag :? UnusedDeclarationTag) -> null
            | _ -> 
                let height = if line = null then glyphSize else max glyphSize (line.TextHeight + 1.0)
                let width = glyphSize
                Rectangle(Fill = Brushes.Yellow, StrokeThickness = 2.0, Stroke = Brushes.Yellow,
                          Height = height, Width = width) :> _

[<Export(typeof<IGlyphFactoryProvider>)>]
[<Name("UnusedDeclarationGlyph")>]
[<ContentType("F#")>]
[<Order(After = PredefinedMarginNames.VerticalScrollBar)>]
[<MarginContainer(PredefinedMarginNames.VerticalScrollBarContainer)>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
[<TagType(typeof<UnusedDeclarationTag>)>]
type UnusedDeclarationFactoryProvider() =
    interface IGlyphFactoryProvider with
        member x.GetGlyphFactory(_view: IWpfTextView, _margin: IWpfTextViewMargin): IGlyphFactory = 
            UnusedDeclarationGlyphFactory() :> _
        

        
