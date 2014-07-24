namespace FSharpVSPowerTools.Tests

open TestUtilities
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Threading

type SyntaxConstructClassifierHelper(buffer: ITextBuffer, fileName: string) =    
    inherit VsTestBase(VirtualProjectProvider(buffer, fileName))
    
    let classifierProvider = new SyntaxConstructClassifierProvider(base.ServiceProvider, null,
                                    projectFactory = base.ProjectFactory,
                                    fsharpVsLanguageService = base.VsLanguageService,
                                    classificationRegistry = base.ClassificationTypeRegistryService,
                                    textDocumentFactoryService = base.DocumentFactoryService)

    let classifier = classifierProvider.GetClassifier(buffer)

    member x.Classifier = classifier

    member x.ClassifierSpans =
        classifier.GetClassificationSpans(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length))
        |> Seq.sortBy (fun span -> span.Span.Start.Position)

    interface IDisposable with
        member x.Dispose() = 
            classifierProvider.Dispose()

[<TestClass>]
type SyntaxConstructClassifierTests() =
    [<ClassInitialize>]
    static member Deploy(context: TestContext) =
        AssertListener.Initialize()
        //TestData.Deploy("", includeTestData = false)

    [<TestMethod; Priority(0)>]
    member x.``should be able to get classifier spans``() = 
        let fileName = @"C:\Tests.fs"
        let content = "type T() = class end"
        let buffer = createMockTextBuffer content fileName
        use helper = new SyntaxConstructClassifierHelper(buffer, fileName)
        let classificationChanged = ref false
        use manualEvent = new ManualResetEvent(false)
        helper.Classifier.ClassificationChanged.Add(fun _ -> 
            classificationChanged := true
            manualEvent.Set() |> ignore)
        manualEvent.WaitOne(10000, false) |> ignore
        Assert.IsTrue(!classificationChanged, "Timed out before classification changed")
        Assert.IsTrue(not <| Seq.isEmpty helper.ClassifierSpans)

