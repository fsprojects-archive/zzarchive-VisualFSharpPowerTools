namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework

type OutliningTaggerHelper() =
    inherit VsTestBase()

    let taggerProvider = OutliningTaggerProvider(
                            vsLanguageService = base.VsLanguageService,
                            serviceProvider = base.ServiceProvider,
                            projectFactory = base.ProjectFactory,
                            textDocumentFactoryService = base.DocumentFactoryService,
                            textEditorFactoryService = null,
                            projectionBufferFactoryService = null,
                            outliningManagerService = null,
                            openDocumentsTracker = base.OpenDocumentsTracker)

    member __.GetView(buffer) =
        createMockTextView buffer

    member __.GetTagger(buffer, _view) = 
        taggerProvider.CreateTagger<_>(buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection(span))
        |> Seq.choose(fun span ->
                match box span.Tag with
                | :? IOutliningRegionTag as tag ->
                    let snapshot = span.Span.Snapshot
                    let lineStart = snapshot.GetLineNumberFromPosition(span.Span.Start.Position) + 1
                    let firstLine = snapshot.GetLineFromPosition(span.Span.Start.Position) 
                    let colStart = span.Span.Start.Position - firstLine.Start.Position + 1
                    Some ((lineStart, colStart), tag.CollapsedForm.ToString())
                | _ -> 
                    None)

module OutliningTaggerTests =

    let helper = OutliningTaggerHelper()
    let fileName = getTempFileName ".fs"

    [<Test>]
    let ``should generate outlining tags``() = 
        let content = """
type Color =
    | Red
    | Green
    | Blue
"""
        let buffer = createMockTextBuffer content fileName
        let view = helper.GetView(buffer)
        helper.AddProject(createVirtualProject(buffer, fileName))
        helper.SetActiveDocument(fileName, content)
        let tagger = helper.GetTagger(buffer, view)

        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> 
                helper.TagsOf(buffer, tagger)
                |> Seq.toList 
                |> assertEqual 
                    [((2, 13), "..."); 
                     ((3, 5), "| Red...")])
