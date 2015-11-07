namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.Linting
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework

type LintTaggerHelper() =
    inherit VsTestBase()

    let taggerProvider = LintTaggerProvider(
                            fsharpVsLanguageService = base.VsLanguageService,
                            serviceProvider = base.ServiceProvider,
                            projectFactory = base.ProjectFactory,
                            textDocumentFactoryService = base.DocumentFactoryService,
                            openDocumentTracker = base.OpenDocumentsTracker)

    member __.GetView(buffer) =
        createMockTextView buffer

    member __.GetTagger(buffer, view) = 
        taggerProvider.CreateTagger<_>(view, buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection(span))
        |> Seq.choose(fun span ->
                match box span.Tag with
                | :? LintTag as tag ->
                    let snapshot = span.Span.Snapshot
                    let lineStart = snapshot.GetLineNumberFromPosition(span.Span.Start.Position) + 1
                    let firstLine = snapshot.GetLineFromPosition(span.Span.Start.Position) 
                    let colStart = span.Span.Start.Position - firstLine.Start.Position + 1
                    Some ((lineStart, colStart), tag.ToolTipContent.ToString())
                | _ -> 
                    None)

module LintTaggerTests =

    let helper = LintTaggerHelper()
    let fileName = getTempFileName ".fs"

    [<Test>]
    let ``should generate lint tags``() = 
        let content = """
let f () = List.iter (fun _ -> ())
"""
        let buffer = createMockTextBuffer content fileName
        let view = helper.GetView(buffer)
        helper.AddProject(createVirtualProject(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let tagger = helper.GetTagger(buffer, view)

        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> 
                helper.TagsOf(buffer, tagger)
                |> Seq.toList 
                |> assertEqual 
                    [((2, 23), "`fun _ -> ()` might be able to be refactored into `ignore`.")])