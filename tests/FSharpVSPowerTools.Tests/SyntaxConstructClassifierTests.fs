namespace FSharpVSPowerTools.Tests

open TestUtilities
open System
open System.IO
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Text.Classification

type ClassificationSpan =
    { Classification: string
      Span: int * int * int * int }

type SyntaxConstructClassifierHelper() =    
    inherit VsTestBase()
    
    let classifierProvider = new SyntaxConstructClassifierProvider(base.ServiceProvider, null,
                                    projectFactory = base.ProjectFactory,
                                    fsharpVsLanguageService = base.VsLanguageService,
                                    classificationRegistry = base.ClassificationTypeRegistryService,
                                    textDocumentFactoryService = base.DocumentFactoryService)

    member x.GetClassifier(buffer) = classifierProvider.GetClassifier(buffer)

    member x.ClassificationSpansOf(buffer: ITextBuffer, classifier: IClassifier) =
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
    let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Tests.fsx")
    type [<Measure>] ms
    let timeout = 30000<ms>
    
    let helper = new SyntaxConstructClassifierHelper()

    [<SetUp>]
    let deploy() =
        AssertListener.Initialize()
    
    let (=>) (startLine, startCol) (endLine, endCol) =
        (startLine, startCol, endLine, endCol)
        
    let testEvent event errorMessage (timeout: int<_>) predicate =
        let task =
            event
            |> Async.AwaitEvent
            |> Async.StartAsTask
        match task.Wait(TimeSpan.FromMilliseconds(float timeout)) with
        | true -> predicate()
        | false ->
            Assert.Fail errorMessage

    [<Test>]
    let ``should be able to get classification spans``() = 
        let content = "type T() = class end"
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout
            (fun () -> helper.ClassificationSpansOf(buffer, classifier) |> Seq.isEmpty |> assertFalse)

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
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout
            (fun () -> 
                helper.ClassificationSpansOf(buffer, classifier)
                |> Seq.toList
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

    [<Test>]
    let ``should be able to get classification spans for unused items``() = 
        let content = """
open System
open System.Collections.Generic
let internal f() = ()
"""
        let fileName = getTempFileName ".fsx"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        // IsSymbolUsedForProject seems to require a file to exist on disks
        // If not, type checking fails with some weird errors
        File.WriteAllText(fileName, "")
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout
            (fun () -> 
                helper.ClassificationSpansOf(buffer, classifier)
                |> Seq.toList
                |> assertEqual
                    [ { Classification = "FSharp.Unused"
                        Span = (2, 6) => (2, 11) };
                      { Classification = "FSharp.Unused"
                        Span = (3, 6) => (3, 31) };
                      { Classification = "FSharp.Unused"
                        Span = (4, 14) => (4, 14) } ])
        File.Delete(fileName)
        

