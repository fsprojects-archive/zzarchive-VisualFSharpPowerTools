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
    let timeout = 30000<ms>
    
    let snapshotPoint (snapshot: ITextSnapshot) line (column: int) = 
        let line = snapshot.GetLineFromLineNumber(line - 1)
        SnapshotSpan(line.Start, column).Start

    let helper = ResolveUnopenedNamespaceSmartTaggerHelper()

    [<SetUp>]
    let deploy() =
        TestUtilities.AssertListener.Initialize()

    [<Test>]
    let ``should be able to get unopened namespace tags``() = 
        let content = "TimeSpan"
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 1) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.isEmpty |> assertFalse)

    [<Test>]
    let ``should not display unopened namespace tags on known values``() = 
        let content = "System.DateTime.Now"
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let view = helper.GetView(buffer)
        view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 17) |> ignore
        let tagger = helper.GetTagger(buffer, view)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> helper.TagsOf(buffer, tagger) |> Seq.isEmpty |> assertTrue)

    [<Test>]
    let ``should generate correct labels for unopened namespace tags``() = 
        let content = "DateTime"
        let fileName = getTempFileName ".fsx"
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

    [<Test>]
    let ``should insert open declarations at correct positions``() = 
        let content = """
#r "System.dll"
TimeSpan
"""
        let fileName = getTempFileName ".fsx"
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

    [<Test>]
    let ``should insert open declaration prefix at correct positions``() = 
        let content = """
#r "System.dll"
DateTime
"""
        let fileName = getTempFileName ".fsx"
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