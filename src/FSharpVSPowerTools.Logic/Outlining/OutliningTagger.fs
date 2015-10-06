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

[<Literal>]
let UpdateDelay = 200us
[<Literal>]
let MaxTooltipLines = 10

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
        | SynMemberDefn.Interface(_, mmembers, _) ->
            yield d.Range
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

let rangeToProperOutlineSnapshotSpan (snapshot: ITextSnapshot) (r:range) =
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
    let mutable ranges : SnapshotSpan [] = [||]

    let triggerUpdate snapshot newRanges =
        try
            ranges <- newRanges |> Array.map (fun x ->
                rangeToProperOutlineSnapshotSpan snapshot x)
            tagsChanged.Trigger(
                self,
                SnapshotSpanEventArgs(
                    SnapshotSpan(
                        buffer.CurrentSnapshot,
                        0,
                        buffer.CurrentSnapshot.Length - 1)))
        with
            | :? ArgumentOutOfRangeException -> ()

    let createTagSpan (ss: SnapshotSpan) =
        try
            let snapshot = ss.Snapshot
            let firstLine = snapshot.GetLineFromPosition(ss.Start.Position)
            let firstLineNumber = firstLine.LineNumber
            let mutable lastLine = snapshot.GetLineFromPosition(ss.End.Position)

            let firstHintLine = snapshot.GetLineFromLineNumber(firstLineNumber + 1)
            let nHintLines = lastLine.LineNumber - firstHintLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber(firstHintLine.LineNumber + MaxTooltipLines - 1)

            let hintSnapshotSpan =
                SnapshotSpan(
                    firstHintLine.Start,
                    lastLine.End)

            let missingLinesCount = Math.Max(nHintLines - MaxTooltipLines, 0)
            let collapsedText = lazy (SnapshotSpan(ss.Start, firstLine.End).GetText() + " ...")

            let hintText = lazy(
                let text = hintSnapshotSpan.GetText()
                match missingLinesCount with
                | 0 -> text
                | n -> text + sprintf "\n\n +%d lines..." n
            )

            TagSpan(
                ss,
                { new IOutliningRegionTag with
                    member __.CollapsedForm      = collapsedText.Force() :> obj
                    member __.CollapsedHintForm  = hintText.Force() :> obj
                    member __.IsDefaultCollapsed = false
                    member __.IsImplementation   = false })
        with
            | :? ArgumentOutOfRangeException -> null

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

        Async.StartInThreadPoolSafe worker

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
