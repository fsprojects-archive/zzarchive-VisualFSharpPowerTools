module FSharpVSPowerTools.Tests.ClassifierTests

open TestUtilities
open TestUtilities.Mocks
open FSharpVSPowerTools.Tests
open FSharpVSPowerTools
open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.TestTools.UnitTesting

type ClassifierHelper(buffer: ITextBuffer) =
    let serviceProvider = MockServiceProvider()
    do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    do serviceProvider.Services.["SVsShell"] <- MockVsShell()
    do serviceProvider.Services.["GeneralOptionsPage"] <- MockGeneralOptionsPage()
    do serviceProvider.Services.["DTE"] <- MockDTE()
    do serviceProvider.Services.["SDTE"] <- MockDTE()

    let provider = new SyntaxConstructClassifierProvider(serviceProvider, null,
                        textDocumentFactoryService = MockDocumentFactoryService())

    let classifier = provider.GetClassifier(buffer)

    member x.ClassifierSpans =
        classifier.GetClassificationSpans(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length))
        |> Seq.sortBy (fun span -> span.Span.Start.Position)

    interface IDisposable with
        member x.Dispose(): unit = 
            provider.Dispose()

let createMockTextBuffer content = 
    MockTextBuffer(content, fileName = @"C:\Tests.fs", contentType = "F#")

[<TestClass>]
type ClassifierTests() =
    [<ClassInitialize>]
    static member Deploy(context: TestContext) =
        AssertListener.Initialize()
        //TestData.Deploy("", includeTestData = false)

    [<TestMethod; Priority(0)>]
    member x.``should be able to get classifier spans``() =
        use classifier = new ClassifierHelper(createMockTextBuffer "let x = 0")
        Assert.AreEqual(Seq.isEmpty classifier.ClassifierSpans, true)

