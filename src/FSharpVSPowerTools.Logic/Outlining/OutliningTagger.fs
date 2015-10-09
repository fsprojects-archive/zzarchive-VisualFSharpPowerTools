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
open System.Diagnostics

let [<Literal>] UpdateDelay = 200us
let [<Literal>] MaxTooltipLines = 25

let visitBinding (b: SynBinding) =
    let r1 = b.RangeOfBindingSansRhs
    let r2 = b.RangeOfBindingAndRhs
    mkFileIndexRange r1.FileIndex r1.End r2.End

let rec visitSynMemberDefn d =
    seq {
        match d with
        | SynMemberDefn.Member (binding, _) -> yield visitBinding binding
        | SynMemberDefn.LetBindings (bindings, _, _, _) ->
            yield! Seq.map visitBinding bindings
        | SynMemberDefn.Interface(tp, mmembers, _) ->
            yield mkFileIndexRange d.Range.FileIndex tp.Range.End d.Range.End
            match mmembers with
            | Some members -> yield! Seq.collect visitSynMemberDefn members
            | None -> ()
        | SynMemberDefn.NestedType(nt, _, _) ->
            yield! visitTypeDefn nt
        | _ -> ()
    }

and visitTypeDefn (TypeDefn(componentInfo, objectModel, members, range)) =
    seq {
        yield mkFileIndexRange range.FileIndex componentInfo.Range.End range.End
        match objectModel with
        | ObjectModel(_, members, _) -> yield! Seq.collect visitSynMemberDefn members
        | Simple _ -> yield! Seq.collect visitSynMemberDefn members
    }

let rec visitDeclaration (decl: SynModuleDecl) = 
    seq {
        match decl with
        | SynModuleDecl.Let(_, bindings, _) ->
            yield! Seq.map visitBinding bindings
        | SynModuleDecl.Types(types, _) ->
            yield! Seq.collect visitTypeDefn types
        | SynModuleDecl.NestedModule(_, decls, _, _) ->
            yield! Seq.collect visitDeclaration decls
        | _ -> ()
    }

let visitModuleOrNamespace moduleOrNs =
    seq {
        let (SynModuleOrNamespace(_, _, decls, _, _, _, _)) = moduleOrNs
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
        let (line, column) = Pos.toZ(p)
        snapshot.GetLineFromLineNumber(line).Start.Add(column)

type SnapshotSpan with
    static member OfRange(snapshot: ITextSnapshot, r: range) =
        SnapshotSpan(
            SnapshotPoint.OfPos(snapshot, r.Start),
            SnapshotPoint.OfPos(snapshot, r.End))

type Tagger
    ( buffer: ITextBuffer
    , textDocument: ITextDocument
    , serviceProvider : IServiceProvider
    , projectFactory: ProjectFactory
    , languageService: VSLanguageService) as self = 

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

    let createTagSpan (ss: SnapshotSpan) =
        try
            let snapshot = ss.Snapshot
            let firstLine = snapshot.GetLineFromPosition(ss.Start.Position)
            let mutable lastLine = snapshot.GetLineFromPosition(ss.End.Position)

            let nHintLines = lastLine.LineNumber - firstLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber(firstLine.LineNumber + MaxTooltipLines - 1)

            let missingLinesCount = Math.Max(nHintLines - MaxTooltipLines, 0)

            let hintSnapshotSpan = SnapshotSpan(firstLine.Start, lastLine.End)
            let hintText = lazy(
                let text = hintSnapshotSpan.GetText()
                match missingLinesCount with
                | 0 -> text
                | n -> text + sprintf "\n...\n\n +%d lines" n
            )

            TagSpan(
                ss,
                { new IOutliningRegionTag with
                    member __.CollapsedForm      = "..." :> obj
                    member __.CollapsedHintForm  = hintText.Force() :> obj
                    member __.IsDefaultCollapsed = false
                    member __.IsImplementation   = false })
        with
            | :? ArgumentOutOfRangeException -> null

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
                 | Some(snapshot, r) ->
                    try
                        let ss = r |> Array.map (fun x -> SnapshotSpan.OfRange(snapshot, x))
                        triggerUpdate ss
                    with
                        | :? ArgumentOutOfRangeException -> ()
                 | None -> ()
            })

        Async.StartInThreadPoolSafe worker

    let docEventListener =
        new DocumentEventListener(
            [ViewChange.bufferEvent buffer],
            UpdateDelay,
            doUpdate)

    interface ITagger<IOutliningRegionTag> with

        member __.GetTags nssc  = getTags nssc

        [<CLIEvent>]
        member __.TagsChanged : IEvent<_,_> = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            (docEventListener :> IDisposable).Dispose()
