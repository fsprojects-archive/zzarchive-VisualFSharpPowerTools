module FSharpVSPowerTools.Outlining

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools
open FSharpVSPowerTools.Utils
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.UntypedAstUtils.Outlining
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open System.Threading
open System.Text
open Microsoft.VisualStudio.Text.Projection
open Microsoft.VisualStudio.Text.Editor
open System.Windows.Media
open System.Windows
open System.Windows.Controls
open Microsoft.FSharp.Compiler.Ast

let [<Literal>] private UpdateDelay = 200us
let [<Literal>] private MaxTooltipLines = 25

//type ScopedSpan = Scope * Collapse * SnapshotSpan
[<Struct; NoComparison>]
type ScopeSpan =
    val Scope : Scope
    val Collapse : Collapse
    val SnapSpan : SnapshotSpan
    new (scope, collapse, snapSpan) = 
        {Scope = scope; Collapse = collapse; SnapSpan = snapSpan}

/// A colored outlining hint control similar to
/// https://github.com/dotnet/roslyn/blob/57aaa6c9d8bc1995edfc261b968777666172f1b8/src/EditorFeatures/Core/Implementation/Outlining/OutliningTaggerProvider.Tag.cs
type OutliningControl (createView: ITextBuffer -> IWpfTextView, createBuffer) as self =
    inherit ContentControl ()

    do self.IsVisibleChanged.Add (fun (e: DependencyPropertyChangedEventArgs) ->
        match e.NewValue, self.Content with
        | (:? bool as nowVisible), null ->
            if nowVisible then
                let view = createView (createBuffer ())
                self.Content <- view.VisualElement
        | (:? bool as nowVisible), (:? ITextView as content) ->
            if not nowVisible then
                content.Close ()
                self.Content <- null
        | _ -> ())

    override __.ToString () =
        match self.Content with
        | null ->
            createBuffer().CurrentSnapshot.GetText ()
        | content ->
            (content :?> ITextView).TextBuffer.CurrentSnapshot.GetText ()

let inline scaleToFit (view: IWpfTextView) =
    let isNormal d = (not (Double.IsNaN d)) && (not (Double.IsInfinity d))
    let suffixLineCount = 2
    view.VisualElement.Height <- view.LineHeight * float (view.TextBuffer.CurrentSnapshot.LineCount + suffixLineCount)

    // In order to compute the width, we need "MaxTextRightCoordinate", but we won't have
    // that until a layout event occurs.  Fortunately, a layout event is going to occur because we set
    // 'Height' above.
    view.LayoutChanged.Add  (fun _ ->
        view.VisualElement.Dispatcher.BeginInvoke (Action (fun () ->
            let newWidth = view.MaxTextRightCoordinate
            let currentWidth = view.VisualElement.Width
            if isNormal newWidth && isNormal currentWidth && newWidth <= currentWidth then ()
            else
                view.VisualElement.Width <- view.MaxTextRightCoordinate))
        |> ignore)
    view

type OutliningTagger
    (textDocument: ITextDocument,
     serviceProvider : IServiceProvider,
     textEditorFactoryService: ITextEditorFactoryService,
     projectionBufferFactoryService: IProjectionBufferFactoryService,
     projectFactory: ProjectFactory,
     languageService: VSLanguageService) as self =

    let buffer = textDocument.TextBuffer
    let tagsChanged = Event<_,_> ()
    let mutable scopedSnapSpans : ScopeSpan [] = [||]
    let mutable oldAST = None : ParsedInput option

 
    /// triggerUpdate -=> tagsChanged
    let triggerUpdate newSnapshotSpans = 
        scopedSnapSpans <- newSnapshotSpans
        tagsChanged.Trigger (self, SnapshotSpanEventArgs buffer.CurrentSnapshot.FullSpan)


    /// convert the FSharp compiler range in SRanges into a snapshotspan and tuple it with its Scope tag
    let fromScopeRange (snapshot: ITextSnapshot) (sr: ScopeRange) : ScopeSpan option =
        let r = sr.Range
        match VSUtils.fromRange snapshot (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn) with
        | Some sshot -> ScopeSpan (sr.Scope, sr.Collapse, sshot) |> Some
        | None       -> None


    // There are times when the compiler will return an empty parse tree due to an error in the source file
    // when this happens if we use that empty tree outlining tags will not be created and any scopes that had
    // been collapsed will explode back open causing an annoying buffer shift. To prevent this behavior we check
    // for an empty ast via it's range and ignore it if the length is zero
    let checkAST (oldtree:ParsedInput option) (newTree:ParsedInput) : bool =
        let inline lengthZero (emptyTree:ParsedInput) =
             emptyTree.Range.StartColumn = emptyTree.Range.EndColumn &&  emptyTree.Range.StartLine = emptyTree.Range.EndLine 
        match oldtree, newTree with
        | None, _ -> true
        | _, emptyTree when lengthZero emptyTree -> false 
        | _ -> true


    /// doUpdate -=> triggerUpdate -=> tagsChanged
    let doUpdate () =
        let uiContext = SynchronizationContext.Current
        asyncMaybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE> ()
            let snapshot = buffer.CurrentSnapshot in let source = snapshot.GetText ()
            let! doc =
                dte.GetCurrentDocument (textDocument.FilePath)
            let! project = projectFactory.CreateForDocument buffer doc
            let! parseFileResults = languageService.ParseFileInProject (doc.FullName, source, project) |> AsyncMaybe.liftAsync
            let! ast = parseFileResults.ParseTree
            //Logging.logInfo "[Outlining]\nAST Range\n%A" ast.Range
            Logging.logInfo "[Outlining]\nAST\n%A" ast
            if checkAST oldAST ast then
                oldAST <- Some ast
                let scopedSpans = (getOutliningRanges >> Seq.choose (fromScopeRange snapshot) >> Array.ofSeq) ast
                do! Async.SwitchToContext uiContext |> AsyncMaybe.liftAsync
                triggerUpdate scopedSpans
        }
        |> Async.Ignore
        |> Async.StartInThreadPoolSafe


    /// viewUpdate -=> doUpdate -=> triggerUpdate -=> tagsChanged
    let docEventListener =
        new DocumentEventListener ([ViewChange.bufferEvent buffer], UpdateDelay, doUpdate) :> IDisposable

    /// Find the length of the shortest whitespace indentation in the textblock used for the outlining
    let findIndent (text: string) =
        let countLeadingWhitespace (str: string) =
            let rec loop acc =
                if acc >= str.Length then acc
                elif not (Char.IsWhiteSpace str.[acc]) then acc
                else loop (acc+1)
            loop 0

        let lines = String.getLines text

        // To find the smallest indentation, an empty line can't serve as the seed
        let rec tryFindStartingLine idx  =
            if idx >= lines.Length then None  // return None if all the lines are blank
            elif String.IsNullOrWhiteSpace lines.[idx] then tryFindStartingLine (idx+1)
            else Some idx // found suitable starting line

        match tryFindStartingLine 0 with
        | None -> 0
        | Some startIndex ->
            if lines = [||] then 0 else
            let minIndent =
                let seed = countLeadingWhitespace lines.[startIndex]
                (seed, lines.[startIndex..])
                ||> Array.fold (fun indent line ->
                    if String.IsNullOrWhiteSpace line then indent // skip over empty lines, we don't want them skewing the min
                    else countLeadingWhitespace line |> min indent)
            minIndent


    /// Create the WPFTextView for the Outlining tooltip scaled to 75% of the document's ZoomLevel
    let createElisionBufferView (textEditorFactoryService: ITextEditorFactoryService) (finalBuffer: ITextBuffer) =
        let roles = textEditorFactoryService.CreateTextViewRoleSet ""
        let view = textEditorFactoryService.CreateTextView (finalBuffer, roles, Background = Brushes.Transparent)
        serviceProvider.GetWPFTextViewOfDocument textDocument.FilePath
        |>  Option.iterElse (fun cv -> view.ZoomLevel <- 0.75 * cv.ZoomLevel) 
                            (fun _ -> view.ZoomLevel <- 0.75 * view.ZoomLevel)
        scaleToFit view


    let createElisionBufferNoIndent (suffixOpt: string option) (projectionBufferFactoryService: IProjectionBufferFactoryService) (hintSnapshotSpan: SnapshotSpan) =
        let exposedSpans = NormalizedSnapshotSpanCollection (hintSnapshotSpan)
        let elisionBuffer = projectionBufferFactoryService.CreateElisionBuffer
                               (projectionEditResolver = null,
                                exposedSpans = exposedSpans,
                                options = ElisionBufferOptions.None)

        let snapshot = hintSnapshotSpan.Snapshot
        let indentationColumn = findIndent (hintSnapshotSpan.GetText ())
        let spansToElide = ResizeArray<Span> ()

        let startLineNumber, endLineNumber = snapshot.LineBounds hintSnapshotSpan

        for lineNumber in startLineNumber..endLineNumber do
            let line = snapshot.GetLineFromLineNumber (lineNumber)
            let lineStart = line.Start.Position
            spansToElide.Add(Span.FromBounds (lineStart, lineStart + indentationColumn))

        elisionBuffer.ElideSpans (NormalizedSpanCollection (spansToElide)) |> ignore

        match suffixOpt with
        | Some suffix ->
            let elisionSpan = elisionBuffer.CurrentSnapshot.FullSpan
            let sourceSpans: obj [] =
                [|
                    elisionSpan.Snapshot.CreateTrackingSpan (elisionSpan.Span, SpanTrackingMode.EdgeExclusive);
                    suffix
                |]
            projectionBufferFactoryService.CreateProjectionBuffer
               (projectionEditResolver = null,
                sourceSpans = sourceSpans,
                options = ProjectionBufferOptions.None) :> ITextBuffer
        | None ->
            elisionBuffer :> ITextBuffer


    // drills down into the snapshot text to find the first non whitespace line
    // to display as the text inside the collapse box preceding the `...`
    let getHintText (snapshotSpan:SnapshotSpan) =
        let textshot = snapshotSpan.Snapshot
        let firstLineNum = snapshotSpan.FirstLineNum textshot
        let rec loop acc =
            if acc >= textshot.LineCount + firstLineNum then "" else
            let text =  if acc = firstLineNum then
                            let _,colstart,_,_ = snapshotSpan.ToRange ()
                            textshot.LineText(acc).Substring(colstart).Trim ()
                        else textshot.LineText(acc).Trim ()
            if String.IsNullOrWhiteSpace text then loop (acc+1) else text
        loop firstLineNum


    // outlined regions that should be collapsed by default will make use of 
    // the scope argument currently hidden by the wildcard `(scope,collapse,snapshotSpan)`
    // probably easiest to use a helper function and put it in `member __.IsDefaultCollapsed = isCollapsed scope`
    let createTagSpan (scopedSpan: ScopeSpan) =
        let scope, collapse, snapshotSpan = scopedSpan.Scope, scopedSpan.Collapse, scopedSpan.SnapSpan
        try
            let snapshot = snapshotSpan.Snapshot in let firstLine = snapshot.GetLineFromPosition (snapshotSpan.Start.Position)
            let mutable lastLine = snapshot.GetLineFromPosition (snapshotSpan.End.Position)

            let nHintLines = lastLine.LineNumber - firstLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber (firstLine.LineNumber + MaxTooltipLines - 1)

            let missingLinesCount = max (nHintLines - MaxTooltipLines) 0
            let hintSnapshotSpan = SnapshotSpan (firstLine.Start, lastLine.End)

            let collapseText, collapseSpan =         
                /// Determine the text that will be displayed in the collapse box and the contents of the hint tooltip    
                let mkOutliningPair (token:string) (md:int) (collapse:Collapse)=
                    match collapse, firstLine.GetText().IndexOf token with // Type extension where `with` is on a lower line
                    | Collapse.Same, -1 -> ((getHintText snapshotSpan) + "...", snapshotSpan)
                    | _ (* Collapse.Below *) , -1 ->  ("...", snapshotSpan)
                    | Collapse.Same, idx  ->
                        let modSpan = SnapshotSpan (SnapshotPoint (snapshot, firstLine.Start.Position + idx + token.Length + md), snapshotSpan.End)
                        ((getHintText modSpan) + "...", modSpan)   
                    | _ (*Collapse.Below*), idx ->
                        let modSpan = SnapshotSpan (SnapshotPoint (snapshot, firstLine.Start.Position + idx + token.Length + md), snapshotSpan.End)
                        ( "...", modSpan) 
                
                let (|OutliningPair|_|) (collapse:Collapse) (_:Scope) =
                    match collapse with
                    | Collapse.Same -> Some ((getHintText snapshotSpan) + "...", snapshotSpan)
                    | _ (*Collapse.Below*) -> Some ("...", snapshotSpan)
                let lineText = firstLine.GetText()

                let inline pairAlts str1 str2 =
                    if lineText.Contains str1 then mkOutliningPair str1 0 collapse else mkOutliningPair str2 0 collapse

                match scope with
                | Scope.Type
                | Scope.Module
                | Scope.Member
                | Scope.LetOrUse 
                | Scope.LetOrUseBang -> mkOutliningPair "=" 0 collapse
                | Scope.ObjExpr
                | Scope.TypeExtension -> mkOutliningPair "with" 0 collapse
                | Scope.MatchClause -> 
                    let idx = lineText.IndexOf "->" 
                    if idx = -1 then  mkOutliningPair "|" -1 collapse else
                    let substr = lineText.SubstringSafe (idx+2)
                    if substr = String.Empty || String.IsNullOrWhiteSpace substr then
                        mkOutliningPair "->" 0 Collapse.Below
                    else
                        mkOutliningPair "->" 0 Collapse.Same
                | Scope.YieldOrReturn -> pairAlts "yield" "return"
                | Scope.YieldOrReturnBang -> pairAlts "yield!" "return!"
                | Scope.Lambda ->  mkOutliningPair "->" 0 collapse
                | Scope.IfThenElse -> mkOutliningPair "if" 0 collapse
                | OutliningPair collapse pair -> pair
                | _ -> ("...", snapshotSpan) // should never be reached due to AP
    
            let createBuffer _ = 
                (match missingLinesCount with
                | 0 -> None
                | n -> Some (sprintf "\n\n +%d lines..." n)
                |> createElisionBufferNoIndent) projectionBufferFactoryService hintSnapshotSpan

            TagSpan ( collapseSpan,
                    { new IOutliningRegionTag with
                        member __.CollapsedForm      = collapseText :> obj
                        member __.IsDefaultCollapsed = false
                        member __.IsImplementation   = false
                        member __.CollapsedHintForm  =
                            OutliningControl (createElisionBufferView textEditorFactoryService, createBuffer) :> _ 
                    }) :> ITagSpan<_> |> Some
        with
        | :? ArgumentOutOfRangeException ->
            Logging.logInfo "ArgumentOutOfRangeException in Outlining.Tagger.createTagSpan"
            None

    /// viewUpdate -=> doUpdate -=> triggerUpdate -=> tagsChanged -=> getTags
    let getTags (normalizedSnapshotSpans: NormalizedSnapshotSpanCollection) : IOutliningRegionTag ITagSpan seq =
        let (|EmptySeq|) xs = if Seq.isEmpty xs then EmptySeq else ()
        match normalizedSnapshotSpans, scopedSnapSpans with
        | EmptySeq, [||] -> Seq.empty
        | _ ->
            let newSnapshot = (Seq.head normalizedSnapshotSpans).Snapshot
            let snapSpan = scopedSnapSpans.[0].SnapSpan
            if newSnapshot.Version <> snapSpan.Snapshot.Version then
                scopedSnapSpans <- scopedSnapSpans
                                   |> Array.map (fun s ->
                                        ScopeSpan (s.Scope, s.Collapse, s.SnapSpan.TranslateTo (newSnapshot, SpanTrackingMode.EdgeExclusive)))
            scopedSnapSpans
            |> Seq.filter (fun s -> normalizedSnapshotSpans.IntersectsWith s.SnapSpan)
            // insert a filter here using scope to remove the regions that should not be outlined at all
            |> Seq.choose createTagSpan 


    interface ITagger<IOutliningRegionTag> with
        member __.GetTags spans =
            protectOrDefault (fun _ -> getTags spans) Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish


    interface IDisposable with
        member __.Dispose () =
            docEventListener.Dispose ()
            scopedSnapSpans <- [||]
