module FSharpVSPowerTools.Tests.ClassifierTests

open TestUtilities
open TestUtilities.Mocks
open FSharpVSPowerTools
open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.TestTools.UnitTesting

type ClassifierHelper(buffer: MockTextBuffer) =
    let serviceProvider = MockServiceProvider()
    do serviceProvider.Services.["SVsActivityLog"] <- MockActivityLog()
    do serviceProvider.Services.["SVsShell"] <- MockVsShell()

    let provider = new SyntaxConstructClassifierProvider(serviceProvider, null)

    member x.TextBuffer = buffer
    member x.Classifier = provider.GetClassifier(buffer)

    member x.ClassifierSpans =
        x.Classifier.GetClassificationSpans(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length))
        |> Seq.sortBy (fun span -> span.Span.Start.Position)

    interface IDisposable with
        member x.Dispose(): unit = 
            provider.Dispose()

let createMockTextBuffer code = 
    MockTextBuffer(code, filename = @"C:\Test.fs", contentType = "F#")

[<TestClass>]
type ClassifierTests() =
    [<ClassInitialize>]
    static member Deploy(context: TestContext) =
        AssertListener.Initialize()
        //TestData.Deploy("", includeTestData = false)

    [<TestMethod; Priority(0)>]
    member x.``should be able to get classifier spans``() =
        use classifier = new ClassifierHelper(createMockTextBuffer "let x = 0")
        classifier.ClassifierSpans |> ignore
