namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.Collections.Generic
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem


// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx
type TypeColorTag() = 
    inherit TextMarkerTag("SymbolDefinitionClassificationFormat")

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx
type SyntaxColorTagger(view: ITextView,
                       sourceBuffer: ITextBuffer,
                       textSearchService: ITextSearchService) as self =
    let tagsChanged = Event<_, _>()

    let caretPositionChangedHandler =
        let handler e f =
            let snapshot = sourceBuffer.CurrentSnapshot
            let getText = snapshot.GetText()
            ()

        EventHandler<_>(handler)

    do
        view.Caret.PositionChanged.AddHandler(caretPositionChangedHandler)

    interface ITagger<TypeColorTag> with
        member x.GetTags (spans: NormalizedSnapshotSpanCollection): seq<ITagSpan<TypeColorTag>> =
            Seq.empty

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish