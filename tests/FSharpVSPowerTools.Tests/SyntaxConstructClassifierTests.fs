namespace FSharpVSPowerTools.Tests

open TestUtilities
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text
open NUnit.Framework

type ClassificationSpan =
    { Classification: string; Span: int * int * int * int }

type SyntaxConstructClassifierHelper(buffer: ITextBuffer, fileName: string) =    
    inherit VsTestBase(VirtualProjectProvider(buffer, fileName))
    
    let classifierProvider = new SyntaxConstructClassifierProvider(base.ServiceProvider, null,
                                    projectFactory = base.ProjectFactory,
                                    fsharpVsLanguageService = base.VsLanguageService,
                                    classificationRegistry = base.ClassificationTypeRegistryService,
                                    textDocumentFactoryService = base.DocumentFactoryService)

    let classifier = classifierProvider.GetClassifier(buffer)

    member x.Classifier = classifier

    member x.ClassificationSpans =
        classifier.GetClassificationSpans(SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length))
        |> Seq.sortBy (fun span -> span.Span.Start.Position)
        |> Seq.map (fun span ->
            let snapshot = span.Span.Snapshot
            // Use 1-based position for intuitive comparison
            let lineStart = snapshot.GetLineNumberFromPosition(span.Span.Start.Position) + 1 
            let lineEnd = snapshot.GetLineNumberFromPosition(span.Span.End.Position) + 1
            let startLine = snapshot.GetLineFromPosition(span.Span.Start.Position)
            let endLine = snapshot.GetLineFromPosition(span.Span.End.Position)
            let colStart = span.Span.Start.Position - startLine.Start.Position + 1
            let colEnd = span.Span.End.Position - endLine.Start.Position + 1
            
            { Classification = span.ClassificationType.Classification;
              Span = (lineStart, colStart, lineEnd, colEnd - 1) } )

    interface IDisposable with
        member x.Dispose() = 
            classifierProvider.Dispose()

[<TestFixture>]
module SyntaxConstructClassifierTests =
    [<SetUp>]
    let deploy() =
        AssertListener.Initialize()

    let fileName = @"C:\Tests.fsx"

    let (=>) (startLine, startCol) (endLine, endCol) =
        (startLine, startCol, endLine, endCol)
        
    let testEvent event errorMessage (timeout: int) predicate =
        let task =
            event
            |> Async.AwaitEvent
            |> Async.StartAsTask
        task.Wait(timeout) |> ignore
        if task.IsCompleted then
            predicate()
        else
            Assert.Fail errorMessage

    [<Test>]
    let ``should be able to get classification spans``() = 
        let content = "type T() = class end"
        let buffer = createMockTextBuffer content fileName
        use helper = new SyntaxConstructClassifierHelper(buffer, fileName)
        testEvent helper.Classifier.ClassificationChanged "Timed out before classification changed" 10000
            (fun () -> assertFalse (Seq.isEmpty helper.ClassificationSpans))

    [<Test>]
    let ``should be able to get classification spans for main categories``() = 
        let content = """
let moduleFunction x = x + 1
let genericClassOfInt = ResizeArray<int>()
let mutable mutableValue = 1
let (|ActivePattern|_|) x = Some x
let _ = <@ 1 = 1 @>
module Module1 =
    let x = ()
"""
        let buffer = createMockTextBuffer content fileName
        use helper = new SyntaxConstructClassifierHelper(buffer, fileName)
        testEvent helper.Classifier.ClassificationChanged "Timed out before classification changed" 10000
            (fun () -> 
                Seq.toList helper.ClassificationSpans
                |> assertEqual
                    [ { Classification = "FSharp.Function"
                        Span = (2, 5) => (2, 18) };
                      { Classification = "FSharp.ReferenceType"
                        Span = (3, 25) => (3, 35) };
                      { Classification = "FSharp.ValueType"
                        Span = (3, 37) => (3, 39) }; 
                      { Classification = "FSharp.MutableVar"
                        Span = (4, 13) => (4, 24) };
                      { Classification = "FSharp.PatternCase"
                        Span = (5, 7) => (5, 19) }; 
                      { Classification = "FSharp.PatternCase"
                        Span = (5, 29) => (5, 32) };
                      { Classification = "FSharp.Quotation"
                        Span = (6, 9) => (6, 19) }; 
                      { Classification = "FSharp.Module"
                        Span = (7, 8) => (7, 14) } ]) 

    [<Test; Ignore>]
    let ``should be able to get classification spans for unused items``() = 
        let content = """
open System
open System.Collections.Generics
let internal f() = ()
"""
        let buffer = createMockTextBuffer content fileName
        use helper = new SyntaxConstructClassifierHelper(buffer, fileName)
        testEvent helper.Classifier.ClassificationChanged "Timed out before classification changed" 10000
            (fun () -> 
                Seq.toList helper.ClassificationSpans
                |> assertEqual
                    [ { Classification = "FSharp.Unused"
                        Span = (2, 6) => (2, 10) };
                      { Classification = "FSharp.Unused"
                        Span = (3, 6) => (3, 32) };
                      { Classification = "FSharp.Unused"
                        Span = (4, 14) => (4, 14) } ]) 
        

