namespace FSharpVSPowerTools.Tests

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

    member __.GetClassifier(buffer) = classifierProvider.GetClassifier(buffer)

    member __.ClassificationSpansOf(buffer: ITextBuffer, classifier: IClassifier) =
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
        member __.Dispose() = 
            classifierProvider.Dispose()

module SyntaxConstructClassifierTests =
#if APPVEYOR
    let timeout = 60000<ms>
#else
    let timeout = 10000<ms>
#endif
    
    let helper = new SyntaxConstructClassifierHelper()
    let fileName = getTempFileName ".fsx"

    [<TestFixtureSetUp>]
    let setUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true

    [<Test>]
    let ``should not return anything if the code doesn't contain semantic symbol``() = 
        let content = "let x = 0"
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout
            (fun () -> helper.ClassificationSpansOf(buffer, classifier) |> Seq.isEmpty |> assertTrue)

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
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        helper.SetActiveDocument(fileName)
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout <| fun _ ->
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
                    Span = (7, 8) => (7, 14) } ] 

    [<Test>]
    let ``should be able to get classification spans for unused items``() = 
        let content = """
open System
open System.Collections.Generic
let internal f() = ()
"""
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(VirtualProjectProvider(buffer, fileName))
        // IsSymbolUsedForProject seems to require a file to exist on disks
        // If not, type checking fails with some weird errors
        File.WriteAllText(fileName, "")
        helper.SetActiveDocument(fileName)
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout <| fun _ ->
            helper.ClassificationSpansOf(buffer, classifier)
            |> Seq.toList
            |> assertEqual
                [ { Classification = "FSharp.Unused"
                    Span = (2, 6) => (2, 11) };
                  { Classification = "FSharp.Unused"
                    Span = (3, 6) => (3, 31) };
                  { Classification = "FSharp.Unused"
                    Span = (4, 14) => (4, 14) } ]
        File.Delete(fileName)
        

    [<Test>]
    let ``should be able to get classification spans for provided types``() = 
        let content = """
module TypeProviderTests
open FSharp.Data
type Project = XmlProvider<"<root><value>1</value><value>3</value></root>">
let _ = Project.GetSample()
"""
        // Use absolute path just to be sure
        let projectFileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/TypeProviderTests/TypeProviderTests.fsproj"))
        let fileName = Path.GetFullPathSafe(Path.Combine(__SOURCE_DIRECTORY__, "../data/TypeProviderTests/TypeProviderTests.fs"))
        let buffer = createMockTextBuffer content fileName
        helper.AddProject(ExternalProjectProvider(projectFileName))
        helper.SetActiveDocument(fileName)
        let classifier = helper.GetClassifier(buffer)
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout <| fun _ -> 
            helper.ClassificationSpansOf(buffer, classifier)
            |> Seq.toList
            |> assertEqual 
                [ { Classification = "FSharp.Module"
                    Span = (2, 8, 2, 24) }
                  { Classification = "FSharp.ReferenceType"
                    Span = (4, 6, 4, 12) }
                  { Classification = "FSharp.ReferenceType"
                    Span = (4, 16, 4, 26) }
                  { Classification = "FSharp.ReferenceType"
                    Span = (5, 9, 5, 15) }
                  { Classification = "FSharp.Function"
                    Span = (5, 17, 5, 25) } ]
