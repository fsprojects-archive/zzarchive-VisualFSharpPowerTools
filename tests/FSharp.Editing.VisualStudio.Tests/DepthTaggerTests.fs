namespace FSharp.Editing.VisualStudio.Tests

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open FSharp.Editing.VisualStudio.Coloring

type DepthTaggerHelper() =
    inherit VsTestBase()

    let taggerProvider = 
        DepthColorizerTaggerProvider(base.ServiceProvider, base.DocumentFactoryService, base.ProjectFactory, base.VsLanguageService,
                                     base.OpenDocumentsTracker)

    member __.GetTagger(buffer) = 
        taggerProvider.CreateTagger<_>(buffer)

    member __.TagsOf(buffer: ITextBuffer, tagger: ITagger<_>) =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagger.GetTags(NormalizedSnapshotSpanCollection(span))
        |> Seq.choose(fun span ->
                match box span.Tag with
                | :? DepthRegionTag as tag ->
                    Some tag.Info
                | _ -> 
                    None)

module DepthTaggerTests =

    let helper = DepthTaggerHelper()
    let fileName = getTempFileName ".fs"

    [<Test>]
    let ``should generate correct depth colorizer tags``() = 
        let content = """
let f x =
    let y = 0
    y

type T() =
    member x.M = ()
"""
        let buffer = createMockTextBuffer "" fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, "")
        let tagger = helper.GetTagger(buffer)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun _ -> 
                helper.SetActiveDocumentContent content
                buffer.Insert(0, content) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)
                |> Seq.toList 
                |> assertEqual 
                    [ (1, 0, 0, 0); 
                      (2, 0, 0, 0); (2, 0, 9, 1); 
                      (3, 0, 4, 1); (3, 4, 255, -2);
                      (4, 0, 5, 1); 
                      (5, 0, 0, 0); 
                      (6, 0, 0, 0); (6, 0, 255, -1); 
                      (7, 0, 4, 0); (7, 4, 19, 1) ])
        ()

    [<Test>]
    let ``should not fail to produce depth colorizer tags on incomplete code``() = 
        let content = """
namespace global

type Hoge () =
"""
        let buffer = createMockTextBuffer "" fileName
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, "")
        let tagger = helper.GetTagger(buffer)
        testEventTrigger tagger.TagsChanged "Timed out before tags changed" timeout
            (fun _ -> 
                helper.SetActiveDocumentContent content
                buffer.Insert(0, content) |> ignore)
            (fun () -> 
                helper.TagsOf(buffer, tagger)
                |> Seq.toList 
                |> assertEqual 
                    [ (1, 0, 0, 0); 
                      (2, 0, 16, 0); 
                      (3, 0, 0, 0); 
                      (4, 0, 0, 0); (4, 0, 255, -1) ])