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
open System.Text

let [<Literal>] private UpdateDelay = 200us
let [<Literal>] private MaxTooltipLines = 25

type ScopedSpan = Scope * SnapshotSpan

type Tagger
    (buffer: ITextBuffer,
     textDocument: ITextDocument,
     serviceProvider : IServiceProvider,
     projectFactory: ProjectFactory,
     languageService: VSLanguageService) as self = 

    let tagsChanged = Event<_,_>()
    
    let mutable snapshotSpans : ScopedSpan [] = [||]


    // triggerUpdate => tagsChanged
    let triggerUpdate newSnapshotSpans =
        snapshotSpans <- newSnapshotSpans
        tagsChanged.Trigger(
            self,
            SnapshotSpanEventArgs(
                SnapshotSpan(
                    buffer.CurrentSnapshot,
                    0,
                    buffer.CurrentSnapshot.Length - 1)))

    let fromFSharpRange (snapshot: ITextSnapshot) (sr: srange) = 
        let r = sr.Range
        match fromRange snapshot (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn) with
        | Some sshot -> Some (sr.Scope,sshot)
        | None       -> None

    // doUpdate => triggerUpdate => tagsChanged
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
                |> getOutliningRanges 
                |> Seq.map (fun r -> fromFSharpRange snapshot r)
                |> Seq.choose id
                |> Array.ofSeq

            do! Async.SwitchToContext uiContext |> AsyncMaybe.liftAsync
            triggerUpdate ranges

        } |> Async.Ignore |> Async.StartInThreadPoolSafe

    // viewUpdate => doUpdate => triggerUpdate => tagsChanged
    let docEventListener =
        new DocumentEventListener(
            [ViewChange.bufferEvent buffer],
            UpdateDelay,
            doUpdate)  :> IDisposable

    // drills down into the snapshot text to find the first non whitespace line 
    // to display as the text inside the collapse box preceding the `...`
    let getHintText (snapshotSpan:SnapshotSpan) =
        let snapshot= snapshotSpan.Snapshot        
        let firstLineNum = snapshot.GetLineNumberFromPosition(snapshotSpan.Start.Position)
        let rec loop acc =
            if acc >= snapshot.LineCount + firstLineNum then "" else
            let text =  if acc = firstLineNum then
                            let _,colstart,_,_ = snapshotSpan.ToRange()
                            snapshot.GetLineFromLineNumber(acc).GetText().Substring(colstart).Trim()
                        else snapshot.GetLineFromLineNumber(acc).GetText().Trim()
            if String.IsNullOrWhiteSpace text then loop (acc+1) else text
        loop firstLineNum

    
    let cleanHintText (text:string) =
        let leadingWhitespace (str:string) =
            let charr = str.ToCharArray()
            let rec loop acc  =
                if acc >= charr.Length then acc 
                elif not (Char.IsWhiteSpace charr.[acc]) then acc
                else loop (acc+1)
            loop 0
        let lines = text.Split [|'\n'|]
        let minlead = 
            let seed = if lines = [||] then 0 else lines.[0] |> leadingWhitespace
            (seed, lines) ||> Array.fold (fun acc elm -> leadingWhitespace elm |> min acc)
        (StringBuilder(), lines) ||> Array.fold (fun acc elm -> elm.Substring minlead |> acc.Append )
            |> string 

    let createTagSpan ((scope,snapshotSpan): Scope*SnapshotSpan) =
        try
            let snapshot = snapshotSpan.Snapshot
            let firstLine = snapshot.GetLineFromPosition(snapshotSpan.Start.Position)
            let mutable lastLine = snapshot.GetLineFromPosition(snapshotSpan.End.Position)

            let nHintLines = lastLine.LineNumber - firstLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber(firstLine.LineNumber + MaxTooltipLines - 1)

            let missingLinesCount = Math.Max(nHintLines - MaxTooltipLines, 0)

            let hintSnapshotSpan = SnapshotSpan(firstLine.Start, lastLine.End)
            let collapseText, collapseSpan = 
                match scope with
                | Scope.Same ->
                    (getHintText snapshotSpan)+"...", snapshotSpan
                | _ (* Scope.Below *) ->
                     "...", SnapshotSpan(firstLine.End, snapshotSpan.End)
            TagSpan(
                collapseSpan,
                { new IOutliningRegionTag with
                    member __.CollapsedForm      = collapseText :> obj
                    member __.IsDefaultCollapsed = false
                    member __.IsImplementation   = false
                    member __.CollapsedHintForm  =
                        let text = hintSnapshotSpan.GetText() |> cleanHintText
                        match missingLinesCount with
                        | 0 -> text :> obj
                        | n -> sprintf "%s\n...\n\n +%d lines" text n :> obj
                    }) :> ITagSpan<_>
        with
            | :? ArgumentOutOfRangeException ->
                Logging.logInfo "ArgumentOutOfRangeException in Outlining.Tagger.createTagSpan"
                null

    let getTags (nssc: NormalizedSnapshotSpanCollection) =
        if Seq.isEmpty nssc || Array.isEmpty snapshotSpans then Seq.empty
        else
            let newSnapshot = (Seq.head nssc).Snapshot
            if newSnapshot.Version <> (snd snapshotSpans.[0]).Snapshot.Version then
                snapshotSpans <- snapshotSpans 
                                |> Array.map (fun (scp,spn) -> 
                                    scp,spn.TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive))
            snapshotSpans
            |> Seq.filter (snd>>nssc.IntersectsWith)
            |> Seq.map createTagSpan


    do doUpdate()

    interface ITagger<IOutliningRegionTag> with

        member __.GetTags spans = getTags spans

        [<CLIEvent>]
        member __.TagsChanged : IEvent<_,_> = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() =
            docEventListener.Dispose()
