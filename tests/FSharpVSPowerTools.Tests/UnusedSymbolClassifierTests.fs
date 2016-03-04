namespace FSharpVSPowerTools.Tests

open System
open System.IO
open FSharpVSPowerTools
open Microsoft.VisualStudio.Text
open NUnit.Framework
open Microsoft.VisualStudio.Text.Classification

type UnusedSymbolClassifierHelper() =
    inherit VsTestBase()
    
    let classifierProvider = new UnusedSymbolClassifierProvider(
                                    serviceProvider = base.ServiceProvider, 
                                    classificationColorManager = null,
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

module UnusedSymbolClassifierTests =
    
    let helper = new UnusedSymbolClassifierHelper()
    let mutable fileName = null 
    let mutable dummyFileName = null

    [<SetUp>]
    let setUp() = 
        fileName <- getTempFileName ".fsx"

    [<Test>]
    let ``should be able to get classification spans for unused items``() = 
        let content = """
open System
open System.Collections.Generic
let internal f() = ()
"""
        let buffer = createMockTextBuffer content fileName
        // IsSymbolUsedForProject seems to require a file to exist on disks
        // If not, type checking fails with some weird errors
        dummyFileName <- fileName
        File.WriteAllText(dummyFileName, "")
        helper.SetUpProjectAndCurrentDocument(createVirtualProject(buffer, fileName), fileName, content)
        let classifier = helper.GetClassifier(buffer)
        
        testEvent classifier.ClassificationChanged "Timed out before classification changed" timeout <| fun _ ->
            let actual = helper.ClassificationSpansOf(buffer, classifier) |> Seq.toList
            let expected =
                [ { Classification = "FSharp.Unused"; Span = (2, 6) => (2, 11) }
                  { Classification = "FSharp.Unused"; Span = (3, 6) => (3, 31) }
                  { Classification = "FSharp.Unused"; Span = (4, 14) => (4, 14) } ]
            actual |> assertEqual expected
        
    [<TestFixtureTearDown>]
    let tearDownAll() =
        if File.Exists dummyFileName then
            File.Delete dummyFileName
        dispose helper
        