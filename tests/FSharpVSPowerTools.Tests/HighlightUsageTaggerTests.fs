namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework

type HighlightUsageTaggerHelper() =    
    inherit VsTestBase()

    let taggerProvider = HighlightUsageTaggerProvider(
                            fsharpVsLanguageService = base.VsLanguageService,
                            serviceProvider = base.ServiceProvider,
                            projectFactory = base.ProjectFactory)

    member __.GetView(buffer) =
        createMockTextView buffer

    member __.GetTagger(buffer, view) = 
        taggerProvider.CreateTagger<_>(view, buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection(span))
        |> Seq.map(fun span ->
            let snapshot = span.Span.Snapshot
            // Use 1-based position for intuitive comparison
            let lineStart = snapshot.GetLineNumberFromPosition(span.Span.Start.Position) + 1 
            let lineEnd = snapshot.GetLineNumberFromPosition(span.Span.End.Position) + 1
            let startLine = snapshot.GetLineFromPosition(span.Span.Start.Position)
            let endLine = snapshot.GetLineFromPosition(span.Span.End.Position)
            let colStart = span.Span.Start.Position - startLine.Start.Position + 1
            let colEnd = span.Span.End.Position - endLine.Start.Position + 1
            (lineStart, colStart, lineEnd, colEnd - 1))

module HighlightUsageTaggerTaggerTests =
#if APPVEYOR
    let timeout = 60000<ms>
#else
    let timeout = 10000<ms>
#endif

    let helper = HighlightUsageTaggerHelper()
    let fileName = getTempFileName ".fsx"

    [<TestFixtureSetUp>]
    let setUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true

    [<Test>]
    let ``should not display tags if moving to a place without symbol``() = 
        let content = """
let x = 0
x
"""     
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)        
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 1) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.toList |> assertNotEqual [])
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 2 9) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.toList |> assertEqual [])

    [<Test>]
    let ``should generate highlight usage tags for values``() = 
        let content = """
let x = 0
x
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 1) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)                 
                // There are duplications in resulting tags
                |> Seq.distinct
                |> Seq.toList
                |> assertEqual
                     [ (3, 1) => (3, 1);
                       (2, 5) => (2, 5) ])

    [<Test>]
    let ``should not generate highlight usage tags for keywords or whitespaces``() = 
        let content = """
do printfn "Hello world!"
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)        
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 2 1) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)                 
                |> Seq.isEmpty
                |> assertTrue)

