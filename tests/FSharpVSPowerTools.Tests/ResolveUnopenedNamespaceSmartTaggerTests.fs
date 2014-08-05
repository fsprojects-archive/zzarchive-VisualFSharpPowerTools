namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Language.Intellisense

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
        |> Seq.choose(fun span ->
                match box span.Tag with
                | :? SmartTag as tag ->
                    tag.ActionSets |> Seq.map (fun ac -> seq ac.Actions) |> Some
                | _ -> 
                    None)
        |> Seq.concat

[<TestFixture>]
module ResolveUnopenedNamespaceSmartTaggerTests =
#if APPVEYOR
    let timeout = 60000<ms>
#else
    let timeout = 10000<ms>
#endif

    let helper = ResolveUnopenedNamespaceSmartTaggerHelper()
    let fileName = getTempFileName ".fsx"

    [<SetUp>]
    let deploy() =
        TestUtilities.AssertListener.Initialize()

    [<Test>]
    let ``return nothing if tags not found``() = 
        let content = "TimeDate"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 1) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.concat |> Seq.toList |> assertEqual [])

    [<Test>]
    let ``should not display unopened namespace tags on known values``() = 
        let content = "System.DateTime.Now"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 17) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.toList |> assertEqual [])

    [<Test; Ignore "Timed out on AppVeyor">]
    let ``should generate correct labels for unopened namespace tags``() = 
        let content = "DateTime"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 1) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> 
                helper.TagsOf(buffer, tagger) 
                |> Seq.map (Seq.map (fun action -> action.DisplayText) >> Seq.toList)
                |> Seq.toList 
                |> assertEqual [ ["open System"]; ["System.DateTime"] ])

    [<Test; Ignore "Timed out on AppVeyor">]
    let ``should insert open declarations at correct positions``() = 
        let content = """
#r "System.dll"
TimeSpan
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 1) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> 
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.tryFind (fun action -> action.DisplayText = "open System")
                    |> Option.get
                tagToInsert.Invoke()
                buffer.CurrentSnapshot.GetText() |> assertEquivString """
#r "System.dll"

open System

TimeSpan
""" )

    [<Test; Ignore "Timed out on AppVeyor">]
    let ``should insert namespace prefix at correct positions``() = 
        let content = """
#r "System.dll"
DateTime
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 1) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> 
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.tryFind (fun action -> action.DisplayText = "System.DateTime")
                    |> Option.get
                tagToInsert.Invoke()
                buffer.CurrentSnapshot.GetText() |> assertEquivString """
#r "System.dll"
System.DateTime
""")