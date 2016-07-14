namespace FSharp.Editing.VisualStudio.Coloring

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Formatting
open System.Windows.Media
open System.Windows
open System.Windows.Controls
open Microsoft.Win32
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.PlatformUI
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Settings

// An inexpensive-to-render rectangular adornment
type RectangleAdornment(fillBrush: Brush, geometry: Geometry) as self = 
    inherit FrameworkElement()
    let child = new DrawingVisual()
    
    do 
        // disable hit testing and interaction with mouse input events such as drag/drop
        self.IsHitTestVisible <- false
        self.UseLayoutRounding <- true
        let context = child.RenderOpen()
        context.DrawGeometry(fillBrush, null, geometry)
        context.Close()
        self.AddVisualChild(child)
    
    override __.GetVisualChild _ = upcast child
    override __.VisualChildrenCount = 1

// See http://blogs.msdn.com/b/noahric/archive/2010/08/25/editor-fundamentals-text-relative-adornments.aspx
// for more about how an 'adornment manager' works
type DepthColorizerAdornment(view: IWpfTextView, 
                             tagAggregator: ITagAggregator<DepthRegionTag>,
                             themeManager: ThemeManager) = 
    let LayerName = Constants.depthAdornmentLayerName // must match the Name attribute Export-ed, further below
    let adornmentLayer = view.GetAdornmentLayer(LayerName)

    // Amount to increase the adornment height to ensure there aren't gaps between adornments
    // due to the way that layout rounding changes the placement of these adornments.
    // Can see the bug if you set this to zero and scale the editor window to e.g. 91%.
    // Through trial and error, 0.2 seems to be the lowest number to eliminate the gap completely.
    let adornmentHeightFudgeFactor = 0.2
    
    let mutable colors = [||]
    let mutable edgeColors = [||]
    let mutable mainColors = [||]

    let computeColors() =
        let currentTheme = themeManager.GetCurrentTheme()
        let themeToString = function
            | VisualStudioTheme.Dark -> "Dark"
            | _ -> "Light"

        colors <-
            let openSubKey (root: RegistryKey) key = 
                protectOrDefault (fun _ -> root.OpenSubKey key |> Option.ofNull) None
            
            protectOrDefault (fun _ ->
                maybe { 
                    let themeString = themeToString currentTheme
                    let userSettingsKey = VSRegistry.RegistryRoot(Interop.__VsLocalRegistryType.RegType_UserSettings)
                    use! key = openSubKey userSettingsKey (sprintf @"Text Editor\FSharpDepthColorizer\%s" themeString)
                    return [| for i in 0..9 do
                                  let s = key.GetValue(sprintf "Depth%d" i) :?> string
                                  yield match s.Split [| ',' |] |> Array.map byte with
                                        | [| r1; g1; b1; r2; g2; b2 |] -> r1, g1, b1, r2, g2, b2
                                        | _ -> failwith "Unhandled case" |]
                }) None
            |> Option.getOrTry (fun _ ->
                // get the default colors based on the current theme and editor background
                let _, background = themeManager.GetEditorTextColors("Plain Text")
                let _, horizontalBar = themeManager.GetEditorTextColors("Indicator Margin")
                Array.create 10 (horizontalBar.R, horizontalBar.G, horizontalBar.B, background.R, background.G, background.B))
    
        edgeColors <- colors |> Array.map (fun (r, g, b, _, _, _) -> Color.FromRgb(r, g, b))
        mainColors <- colors |> Array.map (fun (_, _, _, r, g, b) -> Color.FromRgb(r, g, b))

    let getFadeColor(depth, numCharsWide) = 
        let depth = depth % colors.Length
        let thin = 1.0 / (4.0 * numCharsWide)
        new LinearGradientBrush(new GradientStopCollection([| new GradientStop(edgeColors.[depth], 0.0)
                                                              new GradientStop(mainColors.[depth], thin)
                                                              new GradientStop(mainColors.[depth], 1.0) |]), 
                                new Point(0.0, 0.5), new Point(1.0, 0.5))
    
    // Was once useful for debugging
    let trace s = 
        let ticks = System.DateTime.Now.Ticks
        debug "%O:%O" ticks s
        ()
    
    // A hack; when you ask 0-width spans to compute this, they report the wrong answer. 
    // This is the default font size on my box, and just need a default value until we find a real character to use.
    let mutable pixelsPerChar = 7.0
    
    let refreshLine(line: ITextViewLine) = 
        protect (fun _ ->
            trace ("refresh line {0}", line.TextTop)
            let tags = tagAggregator.GetTags(line.Extent)
        
            let tagSpans = 
                tags
                |> Seq.map (fun tag -> 
                       let spans = tag.Span.GetSpans(view.TextSnapshot)
                       tag.Tag.Info, new SnapshotSpan(spans.[0].Start, spans.[spans.Count - 1].End))
                |> (fun x -> new ResizeArray<_>(x))
            for i = 0 to tagSpans.Count - 1 do
                let (_, sc, ec, d), tagSpan = tagSpans.[i]
                let adornmentHeight = line.Height - (line.LineTransform.TopSpace - line.DefaultLineTransform.TopSpace)
                if tagSpan.Length > 0 then 
                    pixelsPerChar <- view.TextViewLines.GetCharacterBounds(tagSpan.Start).Right 
                                     - view.TextViewLines.GetCharacterBounds(tagSpan.Start).Left
                // Negative d means a depth of -d and a blank line where we have to adorn in a non-char-relative way since there are no whitespace chars on the line to tag
                let left = 
                    if d > 0 then view.TextViewLines.GetCharacterBounds(tagSpan.Start).Left
                    else view.TextViewLines.GetCharacterBounds((snd tagSpans.[0]).Start).Left + pixelsPerChar * (float (sc))
            
                let right = 
                    if (i <> tagSpans.Count - 1) then 
                        let r = view.TextViewLines.GetCharacterBounds(tagSpan.End).Right
                        if d > 0 then r
                        else left + pixelsPerChar * (float (ec - sc))
                    else view.ViewportWidth + view.MaxTextRightCoordinate
            
                let depth = abs d
                let color = getFadeColor(depth, (right - left) / pixelsPerChar)
                // Sometimes at startup these calculations go funky and I get a negative number for (right-left), hmm...
                let width = max (right - left) 0.0 
                debug "Rect: line.Top %f left %f width %f color %i" line.Top left width depth
                let rectangle = 
                    new RectangleGeometry(new Rect(new Size(width, adornmentHeight + adornmentHeightFudgeFactor)))
                let adornment = new RectangleAdornment(color, rectangle)
                Canvas.SetLeft(adornment, left)
                Canvas.SetTop(adornment, line.Top + (line.Height - adornmentHeight) - (adornmentHeightFudgeFactor / 2.0))
                Canvas.SetZIndex(adornment, depth)
                // Add adornment to the appropriate adornment layer
                adornmentLayer.AddAdornment
                    (AdornmentPositioningBehavior.TextRelative, Nullable<_>(line.Extent), null, adornment, null) |> ignore)
    
    let refreshView() = 
        trace ("refresh view")
        adornmentLayer.RemoveAllAdornments()
        for line in view.TextViewLines do
            refreshLine(line)

    let recomputeColors = ThemeChangedEventHandler(fun _ -> computeColors())
    
    do 
        VSColorTheme.add_ThemeChanged(recomputeColors)
        // Initial computation of colors
        computeColors()
        view.ViewportWidthChanged.Add(fun _ -> refreshView())
        view.LayoutChanged.Add(fun e -> 
            for line in e.NewOrReformattedLines do
                refreshLine(line))
        // if we don't refresh the whole view, blank lines (which have no chars to hang tags) are not updated, which is a disaster
        tagAggregator.TagsChanged.Add(fun _ -> refreshView())
        (*
            let spans = e.Span.GetSpans(view.TextSnapshot)
            let lineSpan = new SnapshotSpan(spans.[0].Start, spans.[spans.Count - 1].End)
            for line in view.TextViewLines.GetTextViewLinesIntersectingSpan(lineSpan) do
                adornmentLayer.RemoveAdornmentsByVisualSpan(line.Extent)
                RefreshLine(line)
        *)

    interface IDisposable with
        member __.Dispose() = 
            VSColorTheme.remove_ThemeChanged(recomputeColors)
        
