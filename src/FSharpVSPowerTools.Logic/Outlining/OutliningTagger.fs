module FSharpVSPowerTools.Outlining

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools
open FSharpVSPowerTools.Utils
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.UntypedAstUtils.Outlining
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open System.Threading
open System.Diagnostics

let [<Literal>] private UpdateDelay = 200us
let [<Literal>] private MaxTooltipLines = 25

type Tagger
    (buffer: ITextBuffer,
     textDocument: ITextDocument,
     serviceProvider : IServiceProvider,
     projectFactory: ProjectFactory,
     languageService: VSLanguageService) as self = 

    let tagsChanged = Event<_,_>()
    let mutable snapshotSpans : SnapshotSpan [] = [||]

    let triggerUpdate newSnapshotSpans =
        snapshotSpans <- newSnapshotSpans
        tagsChanged.Trigger(
            self,
            SnapshotSpanEventArgs(
                SnapshotSpan(
                    buffer.CurrentSnapshot,
                    0,
                    buffer.CurrentSnapshot.Length - 1)))

    let createTagSpan (snapshotSpan: SnapshotSpan) =
        try
            let snapshot = snapshotSpan.Snapshot
            let firstLine = snapshot.GetLineFromPosition(snapshotSpan.Start.Position)
            let mutable lastLine = snapshot.GetLineFromPosition(snapshotSpan.End.Position)

            let nHintLines = lastLine.LineNumber - firstLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber(firstLine.LineNumber + MaxTooltipLines - 1)

            let missingLinesCount = Math.Max(nHintLines - MaxTooltipLines, 0)
            let hintSnapshotSpan = SnapshotSpan(firstLine.Start, lastLine.End)

            TagSpan(
                snapshotSpan,
                { new IOutliningRegionTag with
                    member __.CollapsedForm      = "..." :> obj
                    member __.IsDefaultCollapsed = false
                    member __.IsImplementation   = false
                    member __.CollapsedHintForm  =
                        let text = hintSnapshotSpan.GetText()
                        match missingLinesCount with
                        | 0 -> text :> obj
                        | n -> sprintf "%s\n...\n\n +%d lines" text n :> obj
                    })
        with
            | :? ArgumentOutOfRangeException ->
                Logging.logInfo "ArgumentOutOfRangeException in Outlining.Tagger.createTagSpan"
                null

    let getTags (nssc: NormalizedSnapshotSpanCollection) =
        if Seq.isEmpty nssc || Array.isEmpty snapshotSpans then Seq.empty
        else
            let newSnapshot = (Seq.head nssc).Snapshot
            if newSnapshot.Version <> snapshotSpans.[0].Snapshot.Version then
                snapshotSpans <- snapshotSpans |> Array.map (fun x -> x.TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive))
            snapshotSpans
            |> Seq.filter nssc.IntersectsWith
            |> Seq.map (createTagSpan)
            |> Seq.cast<ITagSpan<_>>

    let doUpdate () =
        let uiContext = SynchronizationContext.Current
        asyncMaybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let snapshot = buffer.CurrentSnapshot
            let source = snapshot.GetText()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            let! project = projectFactory.CreateForDocument buffer doc
            let! parseFileResults = languageService.ParseFileInProject (doc.FullName, source, project) |> AsyncMaybe.liftAsync
            let! ast = parseFileResults.ParseTree
            let ranges =
                ast
                |> visitAst 
                |> Seq.filter (fun r -> r.StartLine <> r.EndLine)
                |> Seq.map (fun r -> fromFSharpRange snapshot r)
                |> Seq.choose id
                |> Array.ofSeq

            do! Async.SwitchToContext uiContext |> AsyncMaybe.liftAsync
            triggerUpdate ranges

        } |> Async.Ignore |> Async.StartInThreadPoolSafe

    let docEventListener =
        new DocumentEventListener(
            [ViewChange.bufferEvent buffer],
            UpdateDelay,
            doUpdate)

    interface ITagger<IOutliningRegionTag> with

        member __.GetTags spans = getTags spans

        [<CLIEvent>]
        member __.TagsChanged : IEvent<_,_> = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            (docEventListener :> IDisposable).Dispose()
