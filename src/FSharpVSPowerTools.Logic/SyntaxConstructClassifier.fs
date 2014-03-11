namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open System.Net
open System.Net.Sockets

type SyntaxConstructClassifier(buffer: ITextBuffer,
                               classificationRegistry: IClassificationTypeRegistryService) as self =
    let classificationChanged = Event<_, _>()
    let lockObject = new Object()
    let mutable lastSnapshot: ITextSnapshot = null
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable isWorking = false
//    let udpClient = new UdpClient()

    let sendMsg format =
        let sendString (msg: string) =
            let msg = sprintf "[SyntaxConstructClassifier][%s] %s" (DateTime.UtcNow.ToString("hh:mm:ss.s")) msg

            System.Diagnostics.Debug.WriteLine(msg)

//            let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
//            udpClient.SendAsync(bytes, bytes.Length, "localhost", 2009)
//            |> Async.AwaitTask
//            |> Async.Ignore
//            |> Async.Start

        Printf.ksprintf sendString format

    let synchronousUpdate (newSpans: NormalizedSnapshotSpanCollection) =
        lock
            lockObject
            (fun () ->
                wordSpans <- newSpans

                classificationChanged.Trigger(
                    self, 
                    new ClassificationChangedEventArgs(
                        new SnapshotSpan(
                            buffer.CurrentSnapshot,
                            0,
                            buffer.CurrentSnapshot.Length))))

    let updateSyntaxConstructClassifiers callerName =
        let snapshot = buffer.CurrentSnapshot

        if isWorking = false && snapshot <> lastSnapshot then
            sendMsg "%s - Effective update" callerName

            isWorking <- true
            lastSnapshot <- snapshot

            let doAsync = async {
                let linesFirstCharIndex =
                    [|
                        let lines = lastSnapshot.GetText().Split([|'\n'|])
                        let cnt = ref 0

                        for l in lines do
                            yield !cnt
                            cnt := !cnt + l.Length + 1
                    |]

                let snapshotText = snapshot.GetText()
//                let typeLocations =
//                    let tree = SourceCodeClassifier.getUntypedTree (snapshotText)
//                    match tree with
//                    | ParsedInput.ImplFile(implFile) -> 
//                       // Extract declarations and walk over them
//                       let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
//                       SourceCodeClassifier.visitModulesAndNamespaces modules
//                    | _ -> []

                let typeLocations = SourceCodeClassifier.getTypeLocations snapshotText

                let wordSpans =
                    NormalizedSnapshotSpanCollection
                        [|
                            for location in typeLocations do
                                let lineStartIndex = linesFirstCharIndex.[location.Range.StartLine - 1]
                                let span = new Span(start=lineStartIndex + location.Range.StartColumn,
                                                    length=location.Range.EndColumn - location.Range.StartColumn)

                                yield new SnapshotSpan(snapshot, span)
                        |]

                synchronousUpdate wordSpans

                isWorking <- false
            }

            doAsync
            |> Async.Start

    do
        EventHandler<_>(fun _ _ -> updateSyntaxConstructClassifiers "Buffer changed")
        |> buffer.Changed.AddHandler

        // Execute it the first time
        updateSyntaxConstructClassifiers "First execution"

    interface IClassifier with
        member x.GetClassificationSpans(snapshotSpan: SnapshotSpan): IList<ClassificationSpan> =
            // And now return classification spans for everything
            [|
                for (span: SnapshotSpan) in wordSpans do
                    let classificationType =
                        "FSharp.TypeName"
                        |> classificationRegistry.GetClassificationType

                    yield new ClassificationSpan(span, classificationType)
            |]
            :> _

        [<CLIEvent>]
        member x.ClassificationChanged = classificationChanged.Publish