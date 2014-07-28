namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework

type ResolveUnopenedNamespaceSmartTaggerHelper() =    
    inherit VsTestBase()

    let taggerProvider = ResolveUnopenedNamespaceSmartTaggerProvider(
                            fsharpVsLanguageService = base.VsLanguageService,
                            serviceProvider = base.ServiceProvider,
                            undoHistoryRegistry = base.UndoHistoryRegistry,
                            projectFactory = base.ProjectFactory)

    member __.GetView(buffer) =
        createMockTextView buffer

    member __.GetTagger(buffer, view) = 
        taggerProvider.CreateTagger<_>(view, buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection(span))

[<TestFixture>]
module ResolveUnopenedNamespaceSmartTaggerTests =
    let timeout = 10000<ms>
    
    let helper = ResolveUnopenedNamespaceSmartTaggerHelper()

    [<SetUp>]
    let deploy() =
        TestUtilities.AssertListener.Initialize()

    [<Test>]
    let ``should be able to get resolve unopened namespace tags``() = 
        let content = "DateTime"
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(SnapshotPoint(view.TextSnapshot, 0)) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.isEmpty |> assertFalse)