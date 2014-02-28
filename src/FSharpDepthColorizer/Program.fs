namespace FSharpDepthColorizer

open System
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

[<Export(typeof<ITaggerProvider>)>]
[<ContentType("F#")>]
[<TagType(typeof<FSharpRegionTag>)>]
type FSharpTaggerProvider() =
    let mutable textDocumentFactoryService : ITextDocumentFactoryService = null
    [<Import>]
    member this.TextDocumentFactoryService with get() = textDocumentFactoryService and set(x) = textDocumentFactoryService <- x
    interface ITaggerProvider with
        member this.CreateTagger<'T when 'T :> ITag>(buffer : ITextBuffer) : ITagger<'T> =
            match this.TextDocumentFactoryService.TryGetTextDocument(buffer) with
            | false, _ -> null
            | true, doc -> downcast (new FSharpTagger(buffer, doc.FilePath) |> box)

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Structured)>]
type MyAdornmentManager() =
    let mutable adornmentLayerDefinition : AdornmentLayerDefinition = null
    let mutable viewTagAggregatorFactoryService : IViewTagAggregatorFactoryService = null

    [<Export>]
    [<Name("FSharpDepthFullLineAdornment")>]
    [<Order(Before=PredefinedAdornmentLayers.Selection)>]
    member this.AdornmentLayerDefinition with get() = adornmentLayerDefinition and set(x) = adornmentLayerDefinition <- x

    [<Import>]
    member this.ViewTagAggregatorFactoryService with get() = viewTagAggregatorFactoryService and set(x) = viewTagAggregatorFactoryService <- x

    interface IWpfTextViewCreationListener with
        member this.TextViewCreated(textView) =
            new FullLineAdornmentManager(textView, this.ViewTagAggregatorFactoryService.CreateTagAggregator<FSharpRegionTag>(textView))
            |> ignore

