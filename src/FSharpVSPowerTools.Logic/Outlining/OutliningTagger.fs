module FSharpVSPowerTools.Outlining

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools
open FSharpVSPowerTools.Utils
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.UntypedAstUtils.Outlining
open Microsoft.VisualStudio.Shell.Interop
open System.Threading
open System.Text
open Microsoft.FSharp.Compiler.Ast

let [<Literal>] private UpdateDelay = 200us
let [<Literal>] private MaxTooltipLines = 25

type ScopedSpan = Scope * SnapshotSpan

type OutliningTagger
    (textDocument: ITextDocument,
     serviceProvider : IServiceProvider,
     projectFactory: ProjectFactory,
     languageService: VSLanguageService) as self =

    let buffer = textDocument.TextBuffer
    let tagsChanged = Event<_,_> ()
    let mutable scopedSnapSpans : ScopedSpan [] = [||]
    let mutable oldAST = None : ParsedInput option


    let tagTrigger () =
        tagsChanged.Trigger (self, 
            SnapshotSpanEventArgs (SnapshotSpan   
                (buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length - 1)))

    /// triggerUpdate -=> tagsChanged
    let triggerUpdate newSnapshotSpans = scopedSnapSpans <- newSnapshotSpans; tagTrigger ()

    /// convert the FSharp compiler range in SRanges into a snapshotspan and tuple it with its Scope tag
    let fromSRange (snapshot: ITextSnapshot) (sr: srange) : ScopedSpan option = 
        let r = sr.Range
        match VSUtils.fromRange snapshot (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn) with
        | Some sshot -> Some (sr.Scope,sshot)
        | None       -> None


    // There are times when the compiler will return an empty parse tree due to an error in the source file
    // when this happens if we use that empty tree outlining tags will not be created and any scopes that had
    // been collapsed will explode back open causing an annoying buffer shift. To prevent this behavior we check
    // for an empty ast via it's range and ignore it if the length is zero
    let checkAST (oldtree:ParsedInput option) (newTree:ParsedInput) : bool =
        if oldtree.IsNone then true 
        // Check the parsed AST to see if it's empty via it's range
        elif newTree.Range.StartColumn = newTree.Range.EndColumn  
         &&  newTree.Range.StartLine = newTree.Range.EndLine then false else true


    /// doUpdate -=> triggerUpdate -=> tagsChanged
    let doUpdate () =
        let uiContext = SynchronizationContext.Current
        asyncMaybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let snapshot = buffer.CurrentSnapshot in let source = snapshot.GetText ()
            let! doc = dte.GetCurrentDocument (textDocument.FilePath)
            let! project = projectFactory.CreateForDocument buffer doc
            let! parseFileResults = languageService.ParseFileInProject (doc.FullName, source, project) |> AsyncMaybe.liftAsync
            let! ast = parseFileResults.ParseTree
            //Logging.logInfo "|Outlining|\nAST Range\n%A" ast.Range
            //Logging.logInfo "|Outlining|\nAST\n%A" ast
            if checkAST oldAST ast then
                oldAST <- Some ast
                let ranges = (getOutliningRanges>>Seq.choose (fromSRange snapshot)>>Array.ofSeq) ast                
                do! Async.SwitchToContext uiContext |> AsyncMaybe.liftAsync
                triggerUpdate ranges
        } 
        |> Async.Ignore 
        |> Async.StartInThreadPoolSafe


    /// viewUpdate -=> doUpdate -=> triggerUpdate -=> tagsChanged
    let docEventListener =
        new DocumentEventListener ([ViewChange.bufferEvent buffer], UpdateDelay, doUpdate) :> IDisposable


    // drills down into the snapshot text to find the first non whitespace line 
    // to display as the text inside the collapse box preceding the `...`
    let getHintText (snapshotSpan:SnapshotSpan) =
        let snapshot= snapshotSpan.Snapshot
        let firstLineNum = snapshot.GetLineNumberFromPosition(snapshotSpan.Start.Position)
        let rec loop acc =
            if acc >= snapshot.LineCount + firstLineNum then "" else
            let text =  if acc = firstLineNum then
                            let _,colstart,_,_ = snapshotSpan.ToRange ()
                            snapshot.GetLineFromLineNumber(acc).GetText().Substring(colstart).Trim()
                        else snapshot.GetLineFromLineNumber(acc).GetText().Trim()
            if String.IsNullOrWhiteSpace text then loop (acc+1) else text
        loop firstLineNum


    /// Find the length of the shortest whitespace indentation in the textblock used for the outlining
    /// hint tooltip, then trim that length from the front of every line in the textblock
    let tidyHintText (text:string) =
        let leadingWhitespace (str:string) =
            let charr = str.ToCharArray ()
            let rec loop acc  =
                if acc >= charr.Length then acc 
                elif not (Char.IsWhiteSpace charr.[acc]) then acc else loop (acc+1) in loop 0
        // To to find the smallest indentation, an empty line can't serve as the seed
        let lines = lineSplit text

        let rec findStartingLine idx  = 
            if idx >= lines.Length then -1  // return -1 if all the lines are blank
            elif String.IsNullOrWhiteSpace lines.[idx] then findStartingLine (idx+1) 
            else idx // found suitable starting line

        let startIndex = findStartingLine 0            
        if lines = [||] ||  startIndex = -1 then "" else // no hint text to tidy, return an empty string
        let minIndent = 
            let seed = lines.[startIndex] |> leadingWhitespace
            (seed, lines.[startIndex..]) ||> Array.fold (fun acc elm -> 
                if String.IsNullOrWhiteSpace elm then acc // skip over empty lines, we don't want them skewing our min
                else leadingWhitespace elm |> min acc)
        (StringBuilder (), lines) ||> Array.fold (fun acc elm -> elm.SubstringSafe minIndent |> acc.AppendLine ) |> string 


    let createTagSpan ((scope,snapshotSpan): ScopedSpan) =
        try
            let snapshot = snapshotSpan.Snapshot in let firstLine = snapshot.GetLineFromPosition (snapshotSpan.Start.Position)
            let mutable lastLine = snapshot.GetLineFromPosition (snapshotSpan.End.Position)

            let nHintLines = lastLine.LineNumber - firstLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber (firstLine.LineNumber + MaxTooltipLines - 1)

            let missingLinesCount = max (nHintLines - MaxTooltipLines) 0

            let hintSnapshotSpan = SnapshotSpan (firstLine.Start, lastLine.End)
            let collapseText, collapseSpan = 
                match scope with
                | Scope.Same -> ((getHintText snapshotSpan) + "...", snapshotSpan)
                | _ (* Scope.Below *) -> ("...", SnapshotSpan (firstLine.End, snapshotSpan.End))
            TagSpan ( collapseSpan,
                    { new IOutliningRegionTag with
                        member __.CollapsedForm      = collapseText :> obj
                        member __.IsDefaultCollapsed = false
                        member __.IsImplementation   = false
                        member __.CollapsedHintForm  =
                            let text = hintSnapshotSpan.GetText () |> tidyHintText
                            match missingLinesCount with
                            | 0 -> text :> obj
                            | n -> sprintf "%s\n\n +%d lines..." text n :> obj
                    }) :> ITagSpan<_>
        with 
        | :? ArgumentOutOfRangeException ->
            Logging.logInfo "ArgumentOutOfRangeException in Outlining.Tagger.createTagSpan"
            null


    /// viewUpdate -=> doUpdate -=> triggerUpdate -=> tagsChanged -=> getTags
    let getTags (normalizedSnapshotSpans: NormalizedSnapshotSpanCollection) : IOutliningRegionTag ITagSpan seq =
        match Seq.isEmpty normalizedSnapshotSpans, Array.isEmpty scopedSnapSpans with
        | false, false -> 
            let newSnapshot = (Seq.head normalizedSnapshotSpans).Snapshot
            if newSnapshot.Version <> (snd scopedSnapSpans.[0]).Snapshot.Version then
                scopedSnapSpans <- scopedSnapSpans 
                                   |> Array.map (fun (scp,spn) -> 
                                        scp, spn.TranslateTo (newSnapshot, SpanTrackingMode.EdgeExclusive))            
            scopedSnapSpans 
            |> Seq.filter (snd >> normalizedSnapshotSpans.IntersectsWith) 
            |> Seq.map createTagSpan
        | true , _ 
        | _    , true -> Seq.empty  
        

    // Construct tags on creation
    do  tagTrigger()
        

    interface ITagger<IOutliningRegionTag> with
        member __.GetTags spans = 
            protectOrDefault (fun _ -> getTags spans) Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            scopedSnapSpans <- [||]
            docEventListener.Dispose ()