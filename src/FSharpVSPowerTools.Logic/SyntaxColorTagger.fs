namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open System.Net
open System.Net.Sockets


// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx
type TypeColorTag() = 
    inherit TextMarkerTag("SymbolDefinitionClassificationFormat")

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx
type SyntaxColorTagger(view: ITextView,
                       sourceBuffer: ITextBuffer) as self =
    let tagsChanged = Event<_, _>()

    let lockObject = new Object()
    let mutable lastSnapshot: ITextSnapshot = null
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable isWorking = false
    let udpClient = new UdpClient()

    let sendMsg format =
        let sendString (msg: string) =
            let bytes =
                sprintf "[%s] %s" (DateTime.UtcNow.ToString("hh:mm:ss.s")) msg
                |> System.Text.Encoding.UTF8.GetBytes

            udpClient.SendAsync(bytes, bytes.Length, "localhost", 2009)
            |> Async.AwaitTask
            |> Async.Ignore
            |> Async.Start

        Printf.ksprintf sendString format

    let synchronousUpdate (newSpans: NormalizedSnapshotSpanCollection) =
        lock
            lockObject
            (fun () ->
                wordSpans <- newSpans

                tagsChanged.Trigger(self, 
                                    new SnapshotSpanEventArgs(
                                        new SnapshotSpan(
                                            sourceBuffer.CurrentSnapshot,
                                            0,
                                            sourceBuffer.CurrentSnapshot.Length))))

    let updateSyntaxTags callerName =
        let snapshot = sourceBuffer.CurrentSnapshot

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
                let typeLocations =

                    let tree = SourceCodeClassifier.getUntypedTree (snapshotText)
                    match tree with
                    | ParsedInput.ImplFile(implFile) -> 
                       // Extract declarations and walk over them
                       let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
                       SourceCodeClassifier.visitModulesAndNamespaces modules
                    | _ -> []

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
        EventHandler<_>(fun _ _ -> updateSyntaxTags "CaretPosChanged")
        |> view.Caret.PositionChanged.AddHandler

        EventHandler<_>(fun _ _ -> updateSyntaxTags "TextBufferChanged")
        |> view.TextBuffer.Changed.AddHandler

        // Execute it the first time
        updateSyntaxTags "FirstExecution"

    interface ITagger<TypeColorTag> with
        member x.GetTags (spans: NormalizedSnapshotSpanCollection): seq<ITagSpan<TypeColorTag>> =
            seq {
                // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
                // collection throughout
                if (spans.Count > 0 && wordSpans.Count > 0) then
                    let wordSpansSnapshot =
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans to the expected snapshot 
                        if spans.[0].Snapshot <> wordSpans.[0].Snapshot then
                            new NormalizedSnapshotSpanCollection(
                                wordSpans |> Seq.map (fun span -> span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)))
                        else
                            wordSpans

                    // Second, yield all the other words in the file 
                    for (span: SnapshotSpan) in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpansSnapshot) do
                        yield (new TagSpan<_>(span, new TypeColorTag()) :> ITagSpan<_>)
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member x.Dispose() =
            udpClient.Close()