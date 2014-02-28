namespace FSharpDepthColorizer

open System
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Formatting
open Microsoft.VisualStudio.Utilities
open System.ComponentModel.Composition
open System.Windows.Media
open System.Windows
open System.Windows.Controls
open Microsoft.Win32 
open EnvDTE

// an inexpensive-to-render rectangular adornment
type RectangleAdornment(fillBrush : Brush, geometry : Geometry) as this =
    inherit FrameworkElement()
    let child = new DrawingVisual()
    do
        // disable hit testing and interaction with mouse input events such as drag/drop
        this.IsHitTestVisible <- false
        this.UseLayoutRounding <- true

        let context = child.RenderOpen()
        context.DrawGeometry(fillBrush, null, geometry)
        context.Close()
        this.AddVisualChild(child)

    override this.GetVisualChild(_index) = upcast child
    override this.VisualChildrenCount = 1


// see http://blogs.msdn.com/b/noahric/archive/2010/08/25/editor-fundamentals-text-relative-adornments.aspx
// for more about how an 'adornment manager' works
type FullLineAdornmentManager(view : IWpfTextView, tagAggregator: ITagAggregator<FSharpRegionTag>) =
    let LayerName = "FSharpDepthFullLineAdornment"  // must match the Name attribute Export-ed, further below
    let adornmentLayer = view.GetAdornmentLayer(LayerName)

    // Gets a set of default colors to use depending on whether a light or dark theme is being used
    let defaultColors =
        let defaultLightThemeColors = 
                [|  // greyscale colors
                190uy,190uy,190uy,230uy,230uy,230uy
                170uy,170uy,170uy,210uy,210uy,210uy
                184uy,184uy,184uy,224uy,224uy,224uy
                164uy,164uy,164uy,204uy,204uy,204uy
                178uy,178uy,178uy,218uy,218uy,218uy
                158uy,158uy,158uy,198uy,198uy,198uy
                172uy,172uy,172uy,212uy,212uy,212uy
                152uy,152uy,152uy,192uy,192uy,192uy
                166uy,166uy,166uy,206uy,206uy,206uy
                146uy,146uy,146uy,186uy,186uy,186uy
                |]

        let defaultDarkThemeColors = 
                [| 
                15uy,15uy,15uy,55uy,55uy,55uy
                35uy,35uy,35uy,75uy,75uy,75uy
                21uy,21uy,21uy,61uy,61uy,61uy
                41uy,41uy,41uy,81uy,81uy,81uy
                27uy,27uy,27uy,67uy,67uy,67uy
                47uy,47uy,47uy,87uy,87uy,87uy
                33uy,33uy,33uy,73uy,73uy,73uy
                53uy,53uy,53uy,93uy,93uy,93uy
                39uy,39uy,39uy,79uy,79uy,79uy
                59uy,59uy,59uy,99uy,99uy,99uy
                |]

        let dte = Package.GetGlobalService(typedefof<DTE>) :?> DTE
        let fontsAndColors = dte.Properties("FontsAndColors", "TextEditor").Item("FontsAndColorsItems").Object :?> FontsAndColorsItems
        let background = System.Drawing.ColorTranslator.FromOle(int (fontsAndColors.Item("Plain Text").Background))

        match background.R, background.G, background.B with
        | 30uy, 30uy, 30uy -> defaultDarkThemeColors
        | _ -> defaultLightThemeColors

    // Amount to increase the adornment height to ensure there aren't gaps between adornments
    // due to the way that layout rounding changes the placement of these adornments.
    let AdornmentHeightFudgeFactor = 0.0  // can see the bug if you set this to zero and scale the editor window to e.g. 91%, though for now I don't care

    let colors =
        try
            let key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\VisualStudio\10.0\Text Editor\FSharpDepthColorizer")
            // I don't know if line below is needed actually, I don't really grok how Wow6432Node works
            let key = if key<> null then key else Registry.CurrentUser.OpenSubKey(@"Software\Wow6432Node\Microsoft\VisualStudio\10.0\Text Editor\FSharpDepthColorizer")
            Array.init 10 (fun i -> key.GetValue(sprintf "Depth%d" i) :?> string)
            |> Array.map (fun s -> 
                match s.Split[|','|] |> Array.map byte with
                | [|r1;g1;b1;r2;g2;b2|] -> r1,g1,b1,r2,g2,b2
                | _ -> failwith "Unhandled case")
        with e ->
            defaultColors

    let edgeColors = colors |> Array.map (fun (r,g,b,_,_,_) -> System.Windows.Media.Color.FromRgb(r,g,b))
    let mainColors = colors |> Array.map (fun (_,_,_,r,g,b) -> System.Windows.Media.Color.FromRgb(r,g,b))
    let GetFadeColor(depth, numCharsWide) =
        let depth = depth % colors.Length 
        let thin = 1.0 / (4.0*numCharsWide)
        new LinearGradientBrush(
            new GradientStopCollection( 
                [|
                new GradientStop(edgeColors.[depth], 0.0)
                new GradientStop(mainColors.[depth], thin)
                new GradientStop(mainColors.[depth], 1.0)
                |] ), new Point(0.0, 0.5), new Point(1.0, 0.5))

    // was once useful for debugging
    let trace(s) = 
        let ticks = System.DateTime.Now.Ticks 
        System.Diagnostics.Debug.WriteLine("{0}:{1}", ticks, s)
        ()

    let mutable pixelsPerChar = 7.0 // A hack; when you ask 0-width spans to compute this, they report the wrong answer. This is the default font size on my box, and just need a default value until we find a real character to use.

    let RefreshLine(line : ITextViewLine) =
        trace("refresh line {0}", line.TextTop)
        let tags = tagAggregator.GetTags(line.Extent)
        let tagSpans  = tags 
                        |> Seq.map (fun tag ->
                            let spans = tag.Span.GetSpans(view.TextSnapshot)
                            tag.Tag.Info, new SnapshotSpan(spans.[0].Start, spans.[spans.Count - 1].End))
                        |> (fun x -> new ResizeArray<_>(x))
        for i = 0 to tagSpans.Count-1 do
            let (_,sc,ec,d),tagSpan = tagSpans.[i]
            let adornmentHeight = line.Height - (line.LineTransform.TopSpace - line.DefaultLineTransform.TopSpace)

            if tagSpan.Length > 0 then
                pixelsPerChar <- view.TextViewLines.GetCharacterBounds(tagSpan.Start).Right - view.TextViewLines.GetCharacterBounds(tagSpan.Start).Left
            
            // negative d means a depth of -d and a blank line where we have to adorn in a non-char-relative way since there are no whitespace chars on the line to tag
            let left = if d > 0 then 
                            view.TextViewLines.GetCharacterBounds(tagSpan.Start).Left 
                       else 
                            view.TextViewLines.GetCharacterBounds((snd tagSpans.[0]).Start).Left + pixelsPerChar*(float(sc))
            let right = 
                if (i <> tagSpans.Count - 1) then
                    let r = view.TextViewLines.GetCharacterBounds(tagSpan.End).Right
                    if d > 0 then r else left + pixelsPerChar*(float(ec-sc))
                else
                    view.ViewportWidth + view.MaxTextRightCoordinate
            let depth = abs d
            let color = GetFadeColor(depth, (right-left)/pixelsPerChar)

            let width = max (right - left) 0.0  // sometimes at startup these calculations go funky and I get a negative number for (right-left), hmm...
            //System.Diagnostics.Debug.WriteLine("Rect: line.Top {0} left {1} width {2} color {3}", line.Top, left, width, depth)
            let rectangle = new RectangleGeometry(new Rect(new Size(width, adornmentHeight + AdornmentHeightFudgeFactor)))
            let adornment = new RectangleAdornment(color, rectangle)

            Canvas.SetLeft(adornment, left)
            Canvas.SetTop(adornment, line.Top + (line.Height - adornmentHeight) - (AdornmentHeightFudgeFactor / 2.0))
            Canvas.SetZIndex(adornment, depth)

            // Add adornment to the appropriate adornment layer
            adornmentLayer.AddAdornment(AdornmentPositioningBehavior.TextRelative, Nullable<_>(line.Extent), null, adornment, null) |> ignore

    let RefreshView() =
        trace("refresh view")
        adornmentLayer.RemoveAllAdornments()
        for line in view.TextViewLines do
            RefreshLine(line)

    do
        view.ViewportWidthChanged.Add (fun _ -> RefreshView() )
        view.LayoutChanged.Add (fun e -> for line in e.NewOrReformattedLines do RefreshLine(line) )
        tagAggregator.TagsChanged.Add (fun _e ->
            RefreshView()  // if we don't refresh the whole view, blank lines (which have no chars to hang tags) are not updated, which is a disaster
            (*
            let spans = e.Span.GetSpans(view.TextSnapshot)
            let lineSpan = new SnapshotSpan(spans.[0].Start, spans.[spans.Count - 1].End)
            for line in view.TextViewLines.GetTextViewLinesIntersectingSpan(lineSpan) do
                adornmentLayer.RemoveAdornmentsByVisualSpan(line.Extent)
                RefreshLine(line)
            *)
            )