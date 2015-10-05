module FSharpVSPowerTools.Outlining

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools
open FSharpVSPowerTools.Utils
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open System.Threading

[<Literal>]
let UpdateDelay = 200us
[<Literal>]
let MaxTooltipLines = 30

let visitBinding (Binding(_, _, _, _, _, _, _, _, _, _, range, _)) =
    range

let rec visitDeclaration decl = 
    seq {
        match decl with
        | SynModuleDecl.Let(_, bindings, _) ->
            yield decl.Range
            yield! Seq.map visitBinding bindings
        | SynModuleDecl.NestedModule(_, decls, _, _) ->
            yield decl.Range
            yield! Seq.collect visitDeclaration decls
        | _ -> ()
    }

let visitModuleOrNamespace moduleOrNs =
    seq {
        let (SynModuleOrNamespace(_, _, decls, _, _, _, _)) = moduleOrNs
        yield moduleOrNs.Range
        yield! Seq.collect visitDeclaration decls
    }

let visitAst tree =
    seq {
        match tree with
        | ParsedInput.ImplFile(implFile) ->
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
            yield! Seq.collect visitModuleOrNamespace modules
        | _ -> ()
    }

type SnapshotPoint with
    static member OfPos(snapshot: ITextSnapshot, p: pos) =
        snapshot.GetLineFromLineNumber(p.Line - 1).Start.Add(p.Column)

let rangeToProperOutlineSnapshotSpan (snapshot: ITextSnapshot) (r:range) =
    let firstLine = snapshot.GetLineFromPosition(SnapshotPoint.OfPos(snapshot, r.Start).Position)
    SnapshotSpan(firstLine.End, SnapshotPoint.OfPos(snapshot, r.End))

type Tagger
    ( buffer: ITextBuffer
    , textDocument: ITextDocument
    , serviceProvider : IServiceProvider
    , projectFactory: ProjectFactory
    , languageService: VSLanguageService) as self = 

    let tagsChanged = Event<_,_>()
    let mutable ranges : SnapshotSpan [] = [||]

    let triggerUpdate snapshot newRanges =
        ranges <- Array.map (fun x -> rangeToProperOutlineSnapshotSpan snapshot x) newRanges
        tagsChanged.Trigger(
            self,
            SnapshotSpanEventArgs(
                SnapshotSpan(
                    buffer.CurrentSnapshot,
                    0,
                    buffer.CurrentSnapshot.Length - 1)))

    let createTagSpan (ss: SnapshotSpan) =
        let snapshot = ss.Snapshot
        let firstLine = snapshot.GetLineFromPosition(ss.Start.Position)
        let mutable lastLine = snapshot.GetLineFromPosition(ss.End.Position)

        let nlines = lastLine.LineNumber - firstLine.LineNumber + 1
        if nlines > MaxTooltipLines then
            lastLine <- snapshot.GetLineFromLineNumber(firstLine.LineNumber + MaxTooltipLines - 1)

        let missingLinesCount = Math.Max(nlines - MaxTooltipLines, 0)

        let hintText = lazy(
            // substring(2) ignores newline
            let text = SnapshotSpan(ss.Start, lastLine.End).GetText().Substring(2)
            match missingLinesCount with
            | 0 -> text
            | n -> text + sprintf "\n+ %d lines..." n
        )

        TagSpan(
            ss,
            { new IOutliningRegionTag with
                member x.CollapsedForm      = "..." :> obj
                member x.CollapsedHintForm  = hintText.Force() :> obj
                member x.IsDefaultCollapsed = false
                member x.IsImplementation   = false
            })

    let getTags (nssc: NormalizedSnapshotSpanCollection) =
        if Seq.isEmpty nssc || Array.isEmpty ranges then Seq.empty
        else
            let newSnapshot = (Seq.head nssc).Snapshot
            if newSnapshot.Version <> ranges.[0].Snapshot.Version then
                ranges <- ranges |> Array.map (fun x -> x.TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive))
            ranges
            |> Seq.filter nssc.IntersectsWith
            |> Seq.map (createTagSpan)
            |> Seq.cast<ITagSpan<_>>

    let mutable cts = new CancellationTokenSource()

    let doUpdate () =
        let uiContext = SynchronizationContext.Current
        let worker =
            asyncMaybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let snapshot = buffer.CurrentSnapshot
                let source = snapshot.GetText()

                Debug.Assert(
                    String.getLines source |> Array.length = snapshot.LineCount,
                    "Expected line counts to match")

                let! doc = dte.GetCurrentDocument(textDocument.FilePath)
                let! project = projectFactory.CreateForDocument buffer doc
                let! parseFileResults = languageService.ParseFileInProject (doc.FullName, source, project) |> Async.map Some
                let! ast = parseFileResults.ParseTree
                let ranges =
                    ast
                    |> visitAst 
                    |> Seq.filter (fun r -> r.StartLine <> r.EndLine)
                    |> Array.ofSeq
                return (snapshot, ranges)
            } |> Async.bind (fun x -> async {
                 do! Async.SwitchToContext uiContext
                 match x with
                 | Some(snapshot, r) -> triggerUpdate snapshot r
                 | None -> ()
            })

        cts.Dispose()
        cts <- new CancellationTokenSource()
        Async.StartInThreadPoolSafe(worker, cts.Token)

    let docEventListener =
        new DocumentEventListener(
            [ViewChange.bufferEvent buffer],
            UpdateDelay,
            doUpdate)

    do
        doUpdate ()

    interface ITagger<IOutliningRegionTag> with

        member __.GetTags nssc  = getTags nssc

        [<CLIEvent>]
        member __.TagsChanged : IEvent<_,_> = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            (docEventListener :> IDisposable).Dispose()
