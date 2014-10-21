namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open System.IO

type HighlightUsageTaggerHelper() =    
    inherit VsTestBase()

    let taggerProvider = HighlightUsageTaggerProvider(
                            fsharpVsLanguageService = base.VsLanguageService,
                            serviceProvider = base.ServiceProvider,
                            projectFactory = base.ProjectFactory,
                            textDocumentFactoryService = base.DocumentFactoryService)

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
                |> Seq.toList
                |> assertEqual
                     [ (3, 1) => (3, 1);
                       (2, 5) => (2, 5) ])

    [<Test>]
    let ``should not generate duplicated highlight usage tags for generic types``() = 
        let content = """
module GenericClass =
    type Type1<'a, 'b>() =
        static member Member1() = ()
    let _ = Type1<_,_>.Member1()
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 5 13) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)                 
                |> Seq.toList
                |> assertEqual
                     [ (5, 13) => (5, 17);
                       (3, 10) => (3, 14) ])

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

    [<Test>]
    let ``should generate highlight usage tags for type-provided symbols``() = 
        let content = """
module TypeProviderTests
open FSharp.Data
type Project = XmlProvider<"<root><value>1</value><value>3</value></root>">
let _ = Project.GetSample()
"""
        // Use absolute path just to be sure
        let projectFileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/TypeProviderTests/TypeProviderTests.fsproj"))
        let fileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/TypeProviderTests/TypeProviderTests.fs"))
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(ExternalProjectProvider(projectFileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)        
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 4 8) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)                 
                |> Seq.toList
                |> assertEqual
                     [ (4, 6) => (4, 12); (5, 9) => (5, 15) ])

    [<Test; Ignore; Category "AppVeyorLongRunning"; Description "Not yet know why it fails on CI">]
    let ``should generate highlight usage tags for multi-project symbols``() = 
        let content = """
namespace Project2

module Test =
    let _ = Project1.Class11()
    let _ = Project1.Class11.X
"""
        // Use absolute path just to be sure
        let projectFileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/MultiProjects/Project2/Project2.fsproj"))
        let fileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/MultiProjects/Project2/Project21.fs"))
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(ExternalProjectProvider(projectFileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)        
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 5 22) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)                 
                |> Seq.toList
                |> assertEqual
                     [ (5, 22) => (5, 28); (6, 22) => (6, 28) ])

