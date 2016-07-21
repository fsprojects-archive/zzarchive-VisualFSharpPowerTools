#nowarn "44"
namespace FSharp.Editing.VisualStudio.Tests

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Language.Intellisense

type ResolveUnopenedNamespaceSmartTaggerHelper() =
    inherit VsTestBase()

    let taggerProvider =
        ResolveUnopenedNamespaceSmartTaggerProvider
            (
                fsharpVsLanguageService = base.VsLanguageService,
                serviceProvider = base.ServiceProvider,
                undoHistoryRegistry = base.UndoHistoryRegistry,
                projectFactory = base.ProjectFactory,
                textDocumentFactoryService = base.DocumentFactoryService
            )

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

module ResolveUnopenedNamespaceSmartTaggerTests =

    let helper = ResolveUnopenedNamespaceSmartTaggerHelper()
    let fileName = getTempFileName ".fsx"

    [<Test>]
    let ``return nothing if tags not found``() =
        let content = "TimeDate"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 1) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger)
                       |> Seq.concat
                       |> Seq.toList
                       |> assertEqual [])

    [<Test>]
    let ``should not display unopened namespace tags on known values``() =
        let content = "System.DateTime.Now"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 17) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger)
                       |> Seq.toList
                       |> assertEqual [])

    [<Test>]
    let ``should not fail if active document raises exceptions``() =
        let content = "System.IO.Path"
        // Give an invalid path for active document
        let fileName = ""
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 10) |> ignore)
            (fun () -> helper.TagsOf(buffer, tagger)
                       |> Seq.toList
                       |> assertEqual [])

    [<Test>]
    let ``should generate correct labels for unopened namespace tags``() =
        let content = "DateTime"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 1) |> ignore)
            (fun () ->
                helper.TagsOf(buffer, tagger)
                |> Seq.map (Seq.map (fun action -> action.DisplayText) >> Seq.toList)
                |> Seq.toList
                |> assertEqual [ ["open System"]; ["System.DateTime"] ])

    [<Test>]
    let ``should generate correct labels for unopened namespace tags if a single namespace contains multiple suitable qualified idents``() =
        let content = "empty"
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 1 1) |> ignore)
            (fun () ->
                helper.TagsOf(buffer, tagger)
                |> Seq.map (Seq.map (fun action -> action.DisplayText) >> Seq.toList)
                |> Seq.toList
                |> assertEqual
                    [ ["open Microsoft.FSharp.Collections (Array.empty)"
                       "open Microsoft.FSharp.Collections (List.empty)"
                       "open Microsoft.FSharp.Collections (Map.empty)"
                       "open Microsoft.FSharp.Collections (Seq.empty)"
                       "open Microsoft.FSharp.Collections (Set.empty)" ]

                      ["Microsoft.FSharp.Collections.Array.empty"
                       "Microsoft.FSharp.Collections.List.empty"
                       "Microsoft.FSharp.Collections.Map.empty"
                       "Microsoft.FSharp.Collections.Seq.empty"
                       "Microsoft.FSharp.Collections.Set.empty" ] ])

    [<Test>]
    let ``should insert open declarations at correct positions``() =
        let content = """
#r "System.dll"
TimeSpan
"""
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 1) |> ignore)
            (fun () ->
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.tryFind (fun action -> action.DisplayText = "open System")
                    |> Option.get
                tagToInsert.Invoke()
                buffer.CurrentSnapshot.GetText()
                |> assertEquivString """
#r "System.dll"

open System

TimeSpan
""" )

    [<Test>]
    let ``should insert namespace prefix at correct positions``() =
        let content = """
#r "System.dll"
DateTime
"""
        let buffer = createMockTextBuffer content fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let view = helper.GetView(buffer)
        let tagger = helper.GetTagger(buffer, view)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () ->  view.Caret.MoveTo(snapshotPoint view.TextSnapshot 3 1) |> ignore)
            (fun () ->
                let tagToInsert =
                    helper.TagsOf(buffer, tagger)
                    |> Seq.concat
                    |> Seq.tryFind (fun action -> action.DisplayText = "System.DateTime")
                    |> Option.get
                tagToInsert.Invoke()
                buffer.CurrentSnapshot.GetText()
                |> assertEquivString """
#r "System.dll"
System.DateTime
""")
