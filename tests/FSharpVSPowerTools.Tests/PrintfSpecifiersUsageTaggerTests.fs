namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework

type PrintfSpecifiersUsageTaggerHelper() =
    inherit VsTestBase()

    let taggerProvider = 
        new PrintfSpecifiersUsageTaggerProvider(
                fsharpVsLanguageService = base.VsLanguageService,
                serviceProvider = base.ServiceProvider,
                projectFactory = base.ProjectFactory,
                textDocumentFactoryService = base.DocumentFactoryService,
                shellEventListener = base.ShellEventListener,
                printfColorManager = null)

    member __.GetView buffer =
        createMockTextView buffer

    member __.GetTagger(buffer, view) = 
        taggerProvider.CreateTagger<_>(view, buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection span)
        |> Seq.map(fun span ->
            let snapshot = span.Span.Snapshot
            // Use 1-based position for intuitive comparison
            let lineStart = snapshot.GetLineNumberFromPosition(span.Span.Start.Position) + 1 
            let lineEnd = snapshot.GetLineNumberFromPosition(span.Span.End.Position) + 1
            let firstLine = snapshot.GetLineFromPosition(span.Span.Start.Position)
            let lastLine = snapshot.GetLineFromPosition(span.Span.End.Position)
            let colStart = span.Span.Start.Position - firstLine.Start.Position + 1
            let colEnd = span.Span.End.Position - lastLine.Start.Position + 1
            (lineStart, colStart, lineEnd, colEnd - 1))

module PrintfSpecifiersUsageTaggerTests =
    let helper = PrintfSpecifiersUsageTaggerHelper()
    let mutable fileName = null

    [<SetUp>]
    let setUp() = 
        // we must use unique name for each test because we use `AllowStaleResults.MatchingSource` 
        // in HighlightUsageTagger
        fileName <- getTempFileName ".fsx"

    [<Test>]
    let ``should display tags for printf specifiers``() = 
        let content = """
printf "%d foo %+A bar" 1 "str"
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(createVirtualProject(buffer, fileName))
        helper.SetActiveDocument(fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        let testEventTrigger = testEventTrigger tagger.TagsChanged "Timed out before tags changed" 100000<ms> // timeout 
        testEventTrigger
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 2 8) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger) 
                |> Seq.toList 
                |> assertEqual
                     [ (2, 9) => (2, 10)
                       (2, 25) => (2, 25) ])
                       
        testEventTrigger
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 2 16) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger) 
                |> Seq.toList 
                |> assertEqual
                     [ (2, 16) => (2, 18)
                       (2, 27) => (2, 31) ])
                       
    [<Test>]
    let ``should not display tags if moving to a place without a printf specifier or its argument``() = 
        let content = """
printf "%d foo %+A bar" 1 "str"
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(createVirtualProject(buffer, fileName))
        helper.SetActiveDocument(fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        let testEventTrigger = testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout 
        // first tags changed event is raised by `onCaretMove`, but there are no data yet since
        // `onBufferChanged` event handler has not finished gathering Printf specifiers.
        testEventTrigger
            ignore 
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.toList |> assertEqual [])

        testEventTrigger
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 2 12) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.toList |> assertEqual [])