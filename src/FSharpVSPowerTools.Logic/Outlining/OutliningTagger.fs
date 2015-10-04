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

type SnapshotSpan with
    static member OfRange(snapshot: ITextSnapshot, r: range) =
        SnapshotSpan(
            SnapshotPoint.OfPos(snapshot,r.Start),
            SnapshotPoint.OfPos(snapshot, r.End))

type Tagger
    ( buffer: ITextBuffer
    , textDocument: ITextDocument
    , serviceProvider : IServiceProvider
    , projectFactory: ProjectFactory
    , languageService: VSLanguageService) as self = 

    let tagsChanged = Event<_,_>()
    let mutable ranges : SnapshotSpan [] = [||]

    let triggerUpdate snapshot newRanges =
        ranges <- Array.map (fun x -> SnapshotSpan.OfRange(snapshot, x)) newRanges
        tagsChanged.Trigger(
            self,
            SnapshotSpanEventArgs(
                SnapshotSpan(
                    buffer.CurrentSnapshot,
                    0,
                    buffer.CurrentSnapshot.Length - 1)))

    let createTagSpan (ss: SnapshotSpan) =
        let line = ss.Snapshot.GetLineFromPosition(ss.Start.Position)
        let lineText = lazy line.GetText()
        TagSpan(
            ss,
            { new IOutliningRegionTag with
                member x.CollapsedForm      = lineText.Force() :> obj
                member x.CollapsedHintForm  = lineText.Force() :> obj
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
            200us,
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
