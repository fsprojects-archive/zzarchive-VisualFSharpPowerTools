module FSharpVSPowerTools.Tests.ClassifierTests

open TestUtilities
open TestUtilities.Mocks
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Threading

type ClassifierHelper(buffer: ITextBuffer) =
    let serviceProvider = MockServiceProvider()        
    let projectProvider = VirtualProjectProvider(buffer, @"C:\Tests.fs")
    let dte = MockDTE(projectProvider)
    do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    do serviceProvider.Services.["SVsShell"] <- MockVsShell()
    do serviceProvider.Services.["GeneralOptionsPage"] <- MockGeneralOptionsPage()
    do serviceProvider.Services.["DTE"] <- dte
    do serviceProvider.Services.["SDTE"] <- dte

    let fsharpLanguageService = FSharpLanguageService(serviceProvider)
    let documentFactoryService = Mocks.createDocumentFactoryService()
    let openDocumentsTracker = OpenDocumentsTracker(documentFactoryService)
    let vsLanguageService = VSLanguageService(Mocks.createVsEditorAdaptersFactoryService(), fsharpLanguageService, openDocumentsTracker)
    let projectFactory = ProjectFactory(serviceProvider, vsLanguageService)

    let classifierProvider = new SyntaxConstructClassifierProvider(serviceProvider, null,
                                    projectFactory = projectFactory,
                                    fsharpVsLanguageService = vsLanguageService,
                                    classificationRegistry = Mocks.createClassificationTypeRegistryService(),
                                    textDocumentFactoryService = documentFactoryService)

    let classifier = classifierProvider.GetClassifier(buffer)

    member x.Classifier = classifier

    member x.ClassifierSpans =
        classifier.GetClassificationSpans(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length))
        |> Seq.sortBy (fun span -> span.Span.Start.Position)

    interface IDisposable with
        member x.Dispose() = 
            classifierProvider.Dispose()

let createMockTextBuffer content = 
    MockTextBuffer(content, filename = @"C:\Tests.fs", contentType = "F#")

[<TestClass>]
type ClassifierTests() =
    [<ClassInitialize>]
    static member Deploy(context: TestContext) =
        AssertListener.Initialize()
        //TestData.Deploy("", includeTestData = false)

    [<TestMethod; Priority(0)>]
    member x.``should be able to get classifier spans``() =        
        use helper = new ClassifierHelper(createMockTextBuffer "type T() = class end")
        let classificationChanged = ref false
        use manualEvent = new ManualResetEvent(false)
        helper.Classifier.ClassificationChanged.Add(fun _ -> 
            classificationChanged := true
            manualEvent.Set() |> ignore)
        manualEvent.WaitOne(15000, false) |> ignore
        Assert.IsTrue(!classificationChanged, "Timed out before classification changed")
        Assert.IsTrue(not <| Seq.isEmpty helper.ClassifierSpans)

