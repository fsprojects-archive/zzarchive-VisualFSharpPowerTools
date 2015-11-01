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
open Microsoft.VisualStudio.Text.Projection
open Microsoft.VisualStudio.Text.Editor
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open Microsoft.FSharp.Compiler.Ast
open FSharpVSPowerTools.UntypedAstUtils

let [<Literal>] private UpdateDelay = 200us
let [<Literal>] private MaxTooltipLines = 25

[<Struct; NoComparison>]
type ScopeSpan =
    val Scope : Scope
    val Collapse : Collapse
    val SnapSpan : SnapshotSpan
    new (scope, collapse, snapSpan) =
        {Scope = scope; Collapse = collapse; SnapSpan = snapSpan}

/// A colored outlining hint control similar to
/// https://github.com/dotnet/roslyn/blob/57aaa6c9d8bc1995edfc261b968777666172f1b8/src/EditorFeatures/Core/Implementation/Outlining/OutliningTaggerProvider.Tag.cs
type OutliningHint (createView: ITextBuffer -> IWpfTextView, createBuffer) as self =
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
        match oldtree, newTree with
        | None, _ -> true
        | _, emptyTree when emptyTree.Range.IsEmpty -> false 
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
            //Logging.logInfo "[Outlining]\nAST\n%A" ast
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


    let wpfTextView = lazy(serviceProvider.GetWPFTextViewOfDocument textDocument.FilePath)

    let outliningOptions = lazy(Setting.getOutliningOptions serviceProvider)


    /// Create the WPFTextView for the Outlining tooltip scaled to 75% of the document's ZoomLevel
    let createElisionBufferView (textEditorFactoryService: ITextEditorFactoryService) (finalBuffer: ITextBuffer) =
        let roles = textEditorFactoryService.CreateTextViewRoleSet ""
        let view = textEditorFactoryService.CreateTextView (finalBuffer, roles, Background = Brushes.Transparent)
        let zoomLevel = outliningOptions.Value.TooltipZoomLevel
        wpfTextView.Value
        |>  Option.iterElse (fun cv -> view.ZoomLevel <- zoomLevel * cv.ZoomLevel)
                            (fun _ -> view.ZoomLevel <- zoomLevel * view.ZoomLevel)
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
        let firstLineNum = snapshotSpan.StartLineNum
        let rec loop acc =
            if acc >= textshot.LineCount + firstLineNum then "" else
            let text =  if acc = firstLineNum then
                            let _,colstart,_,_ = snapshotSpan.ToRange ()
                            textshot.LineText(acc).Substring(colstart).Trim ()
                        else textshot.LineText(acc).Trim ()
            if String.IsNullOrWhiteSpace text then loop (acc+1) else text
        loop firstLineNum

    let collapseByDefault scope =
        let options = outliningOptions.Value
        match scope with
        | Scope.Open                  -> options.OpensCollapsedByDefault             
        | Scope.Module                -> options.ModulesCollapsedByDefault   
        | Scope.HashDirective         -> options.HashDirectivesCollapsedByDefault   
        | Scope.Attribute             -> options.AttributesCollapsedByDefault
        | Scope.Interface             
        | Scope.TypeExtension         
        | Scope.Type                  -> options.TypesCollapsedByDefault   
        | Scope.Member                -> options.MembersCollapsedByDefault   
        | Scope.LetOrUse              -> options.LetOrUseCollapsedByDefault 
        | Scope.Match                 
        | Scope.MatchClause           
        | Scope.MatchLambda           -> options.PatternMatchesCollapsedByDefault 
        | Scope.IfThenElse            
        | Scope.ThenInIfThenElse      
        | Scope.ElseInIfThenElse      -> options.IfThenElseCollapsedByDefault 
        | Scope.TryWith               
        | Scope.TryInTryWith          
        | Scope.WithInTryWith         
        | Scope.TryFinally            
        | Scope.TryInTryFinally       
        | Scope.FinallyInTryFinally   -> options.TryWithFinallyCollapsedByDefault     
        | Scope.ArrayOrList           -> options.CollectionsCollapsedByDefault
        | Scope.CompExpr               
        | Scope.ObjExpr                
        | Scope.Quote                  
        | Scope.Record                 
        | Scope.Tuple                  
        | Scope.SpecialFunc           -> options.TypeExpressionsCollapsedByDefault 
        | Scope.CompExprInternal       
        | Scope.LetOrUseBang           
        | Scope.YieldOrReturn          
        | Scope.YieldOrReturnBang     -> options.CExpressionMembersCollapsedByDefault
        | Scope.UnionCase              
        | Scope.EnumCase               
        | Scope.RecordField            
        | Scope.SimpleType             
        | Scope.RecordDefn             
        | Scope.UnionDefn             -> options.SimpleTypesCollapsedByDefault   
        | Scope.For                   
        | Scope.While                 -> options.LoopsCollapsedByDefault    
//        | Scope.Namespace             ->
//        | Scope.Do                    -> 
//        | Scope.Lambda                 
        | _ -> false    

    let outliningEnabled scope =
        let options = outliningOptions.Value
        match scope with
        | Scope.Open                  -> options.OpensEnabled             
        | Scope.Module                -> options.ModulesEnabled   
        | Scope.HashDirective         -> options.HashDirectivesEnabled   
        | Scope.Attribute             -> options.AttributesEnabled
        | Scope.Interface             
        | Scope.TypeExtension         
        | Scope.Type                  -> options.TypesEnabled   
        | Scope.Member                -> options.MembersEnabled   
        | Scope.LetOrUse              -> options.LetOrUseEnabled 
        | Scope.Match                 
        | Scope.MatchClause           
        | Scope.MatchLambda           -> options.PatternMatchesEnabled 
        | Scope.IfThenElse            
        | Scope.ThenInIfThenElse      
        | Scope.ElseInIfThenElse      -> options.IfThenElseEnabled 
        | Scope.TryWith               
        | Scope.TryInTryWith          
        | Scope.WithInTryWith         
        | Scope.TryFinally            
        | Scope.TryInTryFinally       
        | Scope.FinallyInTryFinally   -> options.TryWithFinallyEnabled     
        | Scope.ArrayOrList           -> options.CollectionsEnabled
        | Scope.CompExpr               
        | Scope.ObjExpr                
        | Scope.Quote                  
        | Scope.Record                 
        | Scope.Tuple                  
        | Scope.SpecialFunc           -> options.TypeExpressionsEnabled 
        | Scope.CompExprInternal       
        | Scope.LetOrUseBang           
        | Scope.YieldOrReturn          
        | Scope.YieldOrReturnBang     -> options.CExpressionMembersEnabled
        | Scope.UnionCase              
        | Scope.EnumCase               
        | Scope.RecordField            
        | Scope.SimpleType             
        | Scope.RecordDefn             
        | Scope.UnionDefn             -> options.SimpleTypesEnabled   
        | Scope.For                   
        | Scope.While                 -> options.LoopsEnabled    
//        | Scope.Namespace             ->
//        | Scope.Do                    -> 
//        | Scope.Lambda                 
        | _ -> true   


    // outlined regions that should be collapsed by default will make use of
    // the scope argument currently hidden by the wildcard `(scope,collapse,snapshotSpan)`
    // probably easiest to use a helper function and put it in `member __.IsDefaultCollapsed = isCollapsed scope`
    let createTagSpan (scopedSpan: ScopeSpan) =
        let scope, collapse, snapshotSpan = scopedSpan.Scope, scopedSpan.Collapse, scopedSpan.SnapSpan
        try
            let snapshot = snapshotSpan.Snapshot
            let firstLine = snapshot.GetLineFromPosition (snapshotSpan.Start.Position)
            let mutable lastLine = snapshot.GetLineFromPosition (snapshotSpan.End.Position)

            let nHintLines = lastLine.LineNumber - firstLine.LineNumber + 1
            if nHintLines > MaxTooltipLines then
                lastLine <- snapshot.GetLineFromLineNumber (firstLine.LineNumber + MaxTooltipLines - 1)

            let missingLinesCount = max (nHintLines - MaxTooltipLines) 0
            let hintSnapshotSpan = SnapshotSpan (firstLine.Start, lastLine.End)

            let collapseText, collapseSpan =
                /// Determine the text that will be displayed in the collapse box and the contents of the hint tooltip
                let inline mkOutliningPair (token:string) (md:int) (collapse:Collapse) =
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

                let (|StartsWith|_|) (token:string) (sspan:SnapshotSpan) =
                    if token = "{" then // quick terminate for `{` start, this case must follow all access qualified matches in pattern match below
                        let modspan = (sspan.ModBoth 1 -1)
                        Some ((getHintText modspan) + "...", modspan) else
                    let startText = lineText.SubstringSafe sspan.StartColumn
                    if startText.StartsWith token then
                        match sspan.PositionOf "{" with
                        | bl,_ when bl>sspan.StartLineNum -> Some ("{...}",(sspan.ModStart (token.Length)))
                        | bl,bc when bl=sspan.StartLineNum ->
                            let modSpan = (sspan.ModStart (bc-sspan.StartColumn)).ModBoth 1 -1
                            Some (getHintText modSpan+"...",modSpan)
                        | _ -> None
                    else None

                match scope with
                | Scope.Type
                | Scope.Module
                | Scope.Member
                | Scope.LetOrUse
                | Scope.LetOrUseBang -> mkOutliningPair "=" 0 collapse
                | Scope.ObjExpr
                | Scope.Interface
                | Scope.TypeExtension -> mkOutliningPair "with" 0 collapse
                | Scope.MatchClause ->
                    let idx = lineText.IndexOf "->"
                    if idx = -1 then  mkOutliningPair "|" -1 collapse else  // special case to collapse compound guards
                    let substr = lineText.SubstringSafe (idx+2)
                    if substr = String.Empty || String.IsNullOrWhiteSpace substr then
                        mkOutliningPair "->" 0 Collapse.Below
                    else
                        mkOutliningPair "->" 0 Collapse.Same
                | Scope.YieldOrReturn -> pairAlts "yield" "return"
                | Scope.YieldOrReturnBang -> pairAlts "yield!" "return!"
                | Scope.Lambda ->  mkOutliningPair "->" 0 collapse
                | Scope.IfThenElse -> mkOutliningPair "if" 0 collapse
                | Scope.RecordDefn ->
                    match snapshotSpan with
                    | StartsWith "private"  pair -> pair
                    | StartsWith "public"   pair -> pair
                    | StartsWith "internal" pair -> pair
                    | StartsWith "{"  pair -> pair
                    | _ -> ("...", snapshotSpan) // should never be reached due to AP
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
                        member __.IsDefaultCollapsed = collapseByDefault scope
                        member __.IsImplementation   = false
                        member __.CollapsedHintForm  =
                            OutliningHint (createElisionBufferView textEditorFactoryService, createBuffer) :> _
                    }) :> ITagSpan<_> 
            |> Some
        with
        | :? ArgumentOutOfRangeException ->
            Logging.logInfo (fun _ -> "ArgumentOutOfRangeException in Outlining.Tagger.createTagSpan")
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
            |> Seq.filter (fun s -> outliningEnabled s.Scope)
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
