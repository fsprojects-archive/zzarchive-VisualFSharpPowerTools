namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Language.Intellisense
open FSharpVSPowerTools.DepthColorizer

type DepthTaggerHelper() =    
    inherit VsTestBase()

    let taggerProvider = DepthColorizerTaggerProvider(
                            fsharpVsLanguageService = base.VsLanguageService,
                            serviceProvider = base.ServiceProvider,
                            textDocumentFactoryService = base.DocumentFactoryService)

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

    [<TestFixtureSetUp>]
    let setUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true

    [<Test>]
    let ``should generate correct depth colorizer tags``() = 
        let content = """
let f x =
    let y = 0
    y

type T() =
    member x.M = ()
"""
        let buffer = createMockTextBuffer content fileName
        let tagger = helper.GetTagger(buffer)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
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

    [<Test>]
    let ``should not fail to produce depth colorizer tags on incomplete code``() = 
        let content = """
namespace global

type Hoge () =
"""
        let buffer = createMockTextBuffer content fileName
        let tagger = helper.GetTagger(buffer)
        testEvent tagger.TagsChanged "Timed out before tags changed" timeout
            (fun () -> 
                helper.TagsOf(buffer, tagger)                 
                |> Seq.toList 
                |> assertEqual 
                    [ (1, 0, 0, 0); 
                      (2, 0, 16, 0); 
                      (3, 0, 0, 0); 
                      (4, 0, 0, 0); (4, 0, 255, -1) ])


