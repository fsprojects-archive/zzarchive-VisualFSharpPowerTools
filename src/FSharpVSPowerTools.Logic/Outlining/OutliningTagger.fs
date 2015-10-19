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
open Microsoft.VisualStudio.Text.Projection
open Microsoft.VisualStudio.Text.Editor
open System.Windows.Media
open System.Windows
open System.Windows.Controls

let [<Literal>] private UpdateDelay = 200us
let [<Literal>] private MaxTooltipLines = 25

type ScopedSpan = Scope * SnapshotSpan

type OutliningControl(createView: ITextBuffer -> IWpfTextView, createBuffer) as self =
    inherit ContentControl()
   
    do self.IsVisibleChanged.Add (fun (e: DependencyPropertyChangedEventArgs) ->
        let nowVisible = e.NewValue :?> bool
        if nowVisible then
            match self.Content with
            | null ->
                let view = createView (createBuffer())
                self.Content <- view.VisualElement
            | _ -> ()
        else
            (self.Content :?> ITextView).Close()
            self.Content <- null)
        
    override __.ToString() =
        match self.Content with
        | null ->
            createBuffer().CurrentSnapshot.GetText()
        | content ->
            (content :?> ITextView).TextBuffer.CurrentSnapshot.GetText()

let sizeToFit (view: IWpfTextView) =
    let isNormal d = (not (Double.IsNaN d)) && (not (Double.IsInfinity d))
    view.VisualElement.Height <- view.LineHeight * float view.TextBuffer.CurrentSnapshot.LineCount

    // In order to compute the width, we need "MaxTextRightCoordinate", but we won't have
    // that until a layout event occurs.  Fortunately, a layout event is going to occur because we set
    // 'Height' above.
    view.LayoutChanged.Add  (fun _ ->
        view.VisualElement.Dispatcher.BeginInvoke(Action(fun () ->
            let newWidth = view.MaxTextRightCoordinate
            let currentWidth = view.VisualElement.Width
            if isNormal newWidth && isNormal currentWidth && newWidth <= currentWidth then ()
            else
                view.VisualElement.Width <- view.MaxTextRightCoordinate)) 
        |> ignore) 

type OutliningTagger
    (textDocument: ITextDocument,
     serviceProvider : IServiceProvider,
     textEditorFactoryService: ITextEditorFactoryService,
     projectionBufferFactoryService: IProjectionBufferFactoryService,
     projectFactory: ProjectFactory,
     languageService: VSLanguageService) as self =

    let buffer = textDocument.TextBuffer
    let tagsChanged = Event<_,_> ()
    let mutable scopedSnapSpans : ScopedSpan [] = [||]

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
            let ranges = (getOutliningRanges>>Seq.choose (fromSRange snapshot)>>Array.ofSeq) ast
            do! Async.SwitchToContext uiContext |> AsyncMaybe.liftAsync
            triggerUpdate ranges
        } 
        |> Async.Ignore 
        |> Async.StartInThreadPoolSafe


    /// viewUpdate -=> doUpdate -=> triggerUpdate -=> tagsChanged
    let docEventListener =
        new DocumentEventListener ([ViewChange.bufferEvent buffer], UpdateDelay, doUpdate) :> IDisposable

    /// Find the length of the shortest whitespace indentation in the textblock used for the outlining
    let inferIndent (text: string) =
        let countleadingWhitespace (str: string) =
            let rec loop acc =
                if acc >= str.Length then acc 
                elif not (Char.IsWhiteSpace str.[acc]) then acc 
                else loop (acc+1)
            loop 0

        // To find the smallest indentation, an empty line can't serve as the seed
        let lines = lineSplit text

        let rec tryFindStartingLine idx  = 
            if idx >= lines.Length then None  // return None if all the lines are blank
            elif String.IsNullOrWhiteSpace lines.[idx] then tryFindStartingLine (idx+1) 
            else Some idx // found suitable starting line

        match tryFindStartingLine 0 with
        | None -> 0
        | Some startIndex ->
            if lines = [||] then 0 else
            let minIndent = 
                let seed = countleadingWhitespace lines.[startIndex]
                (seed, lines.[startIndex..]) 
                ||> Array.fold (fun indent line -> 
                    if String.IsNullOrWhiteSpace line then indent // skip over empty lines, we don't want them skewing our min
                    else countleadingWhitespace line |> min indent)
            minIndent

    let createElisionBufferView (textEditorFactoryService: ITextEditorFactoryService) (finalBuffer: ITextBuffer) =
        let roles = textEditorFactoryService.CreateTextViewRoleSet("")
        let view = textEditorFactoryService.CreateTextView(finalBuffer, roles, Background = Brushes.Transparent)
        view.ZoomLevel <- 0.75 * view.ZoomLevel
        sizeToFit view
        view

    let createElisionBufferNoIndent (factoryService: IProjectionBufferFactoryService) (hintSnapshotSpan: SnapshotSpan) =
        let exposedSpans = NormalizedSnapshotSpanCollection(hintSnapshotSpan)
        let elisionBuffer = factoryService.CreateElisionBuffer(null, exposedSpans, ElisionBufferOptions.None)
        
        let snapshot = hintSnapshotSpan.Snapshot
        let indentationColumn = inferIndent (hintSnapshotSpan.GetText())
        let spansToElide = ResizeArray<Span>()
        
        let startLineNumber = snapshot.GetLineNumberFromPosition(hintSnapshotSpan.Span.Start)
        let endLineNumber = snapshot.GetLineNumberFromPosition(hintSnapshotSpan.Span.End)

        for lineNumber in startLineNumber..endLineNumber do
            let line = snapshot.GetLineFromLineNumber(lineNumber)
            let lineStart = line.Start.Position
            spansToElide.Add(Span.FromBounds(lineStart, lineStart + indentationColumn))
                
        elisionBuffer.ElideSpans(NormalizedSpanCollection(spansToElide)) |> ignore
        elisionBuffer

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
                            OutliningControl((createElisionBufferView textEditorFactoryService), 
                                             (fun _ -> createElisionBufferNoIndent projectionBufferFactoryService hintSnapshotSpan)) :> _
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