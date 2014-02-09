namespace FSharpVSPowerTools.HighlightUsage

open System
open System.Diagnostics
open System.Threading
open System.ComponentModel.Composition
open System.Windows.Media
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open EnvDTE
open VSLangProj
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

type HighlightUsageTag() = 
    inherit TextMarkerTag("MarkerFormatDefinition/HighlightIdentifierFormatDefinition")

[<Export(typeof<EditorFormatDefinition>)>]
[<Name("MarkerFormatDefinition/HighlightIdentifierFormatDefinition")>]
[<UserVisible(true)>]
type HighlightIdentifierFormatDefinition() =
    inherit MarkerFormatDefinition()
    do  
      base.BackgroundColor <- Nullable Colors.LightGreen
      base.ForegroundColor <- Nullable Colors.DarkGreen
      base.DisplayName <- "F# Highlight Usage"
      base.ZOrder <- 5

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type HighlightUsageTagger(view : ITextView, sourceBuffer : ITextBuffer, 
                          textStructureNavigator : ITextStructureNavigator) =
    let tagsChanged = Event<_, _>()
    let updateLock = obj()
    
    member val private View = view with get, set
    member val private SourceBuffer = sourceBuffer with get, set
    member val private TextStructureNavigator = textStructureNavigator with get, set
    member val private WordSpans = NormalizedSnapshotSpanCollection() with get, set
    member val private CurrentWord = Nullable<SnapshotSpan>() with get, set
    member val private RequestedPoint : SnapshotPoint = SnapshotPoint() with get, set

    /// Perform a synchronous update, in case multiple background threads are running
    member private this.SynchronousUpdate(currentRequest : SnapshotPoint, newSpans : NormalizedSnapshotSpanCollection,
                                          newCurrentWord : Nullable<SnapshotSpan>) =
        lock updateLock (fun () ->
            if currentRequest = this.RequestedPoint then
                this.WordSpans <- newSpans
                this.CurrentWord <- newCurrentWord

                tagsChanged.Trigger(this, SnapshotSpanEventArgs(SnapshotSpan(this.SourceBuffer.CurrentSnapshot, 0, 
                                                                    this.SourceBuffer.CurrentSnapshot.Length)))
            )

    member private this.DoUpdate(currentRequest : SnapshotPoint, currentWord : SnapshotSpan, wordSpans : SnapshotSpan seq) =
        if currentRequest = this.RequestedPoint then
            lock updateLock (fun () ->
                let (beginLine, beginCol, _, _) = currentWord.GetRange()
                let dte = Package.GetGlobalService(typedefof<DTE>) :?> DTE
                let doc = dte.ActiveDocument
                Debug.Assert(doc <> null && doc.ProjectItem.ContainingProject <> null, "Should be able to find active document.")
                let project = doc.ProjectItem.ContainingProject.Object :?> VSProject
                if project = null then
                    Debug.WriteLine("Can't find containing project. Probably the document is opened in an ad-hoc way.")
                    this.SynchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), Nullable())
                else
                    let currentFile = doc.FullName
                    let projectProvider = ProjectProvider(currentFile, project)
                    let projectFileName = projectProvider.ProjectFileName
                    let source = currentRequest.Snapshot.GetText()
                    let currentLine = currentRequest.GetContainingLine().GetText()
                    let framework = projectProvider.TargetFramework
                    let args = Array.ofSeq projectProvider.CompilerOptions
                    let sourceFiles = Array.ofSeq projectProvider.SourceFiles
                    let results = 
                        VSLanguageService.Instance.GetUsesOfSymbolAtLocation(projectFileName, currentFile, source, sourceFiles, 
                                                                             beginLine, beginCol, currentLine, args, framework)
                        |> Async.RunSynchronously
                    match results with
                    | Some(currentSymbolName, lastIdent, currentSymbolRange, references) -> 
                        // TODO: use lastIdent to highlight relevant part of the long identifier
                        let foundUsages =
                            references
                            |> Seq.filter (fun (fileName, range) -> fileName = currentFile)
                            |> Seq.map (fun (_, ((beginLine, beginCol), (endLine, endCol))) -> 
                                // Range01 type consists of zero-based values, which is a bit confusing
                                fromPosition(currentWord.Snapshot, beginLine + 1, beginCol, endLine + 1, endCol))
                        this.SynchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(foundUsages), Nullable currentWord)
                    | None ->
                        // Return empty values in order to clear up markers
                        this.SynchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), Nullable())
            )

    member private this.UpdateWordAdornments(threadContext : obj) =
        let currentRequest = this.RequestedPoint

        // Find all words in the buffer like the one the caret is on
        match this.TextStructureNavigator.FindAllWords(currentRequest) with
        | None ->
            // If we couldn't find a word, just clear out the existing markers
            this.SynchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), Nullable())
        | Some currentWord ->
            // If this is the same word we currently have, we're done (e.g. caret moved within a word).
            if this.CurrentWord.HasValue && currentWord = this.CurrentWord.Value then ()
            else
                // Find the new spans
                let (span, newSpans) = currentWord.FindNewSpans()
                // If we are still up-to-date (another change hasn't happened yet), do a real update))
                this.DoUpdate(currentRequest, span, newSpans)

    member private this.UpdateAtCaretPosition(caretPosition : CaretPosition) =
        let point = caretPosition.Point.GetPoint(this.SourceBuffer, caretPosition.Affinity)

        if point.HasValue then 
            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            if this.CurrentWord.HasValue &&
               this.CurrentWord.Value.Snapshot = this.View.TextSnapshot &&
               point.Value.CompareTo(this.CurrentWord.Value.Start) >= 0 &&
               point.Value.CompareTo(this.CurrentWord.Value.End) <= 0 then ()
            else
                this.RequestedPoint <- point.Value
                ThreadPool.QueueUserWorkItem(new WaitCallback(this.UpdateWordAdornments)) |> ignore

    member private this.ViewLayoutChanged = 
        EventHandler<_>(fun _ (e : TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                this.UpdateAtCaretPosition(this.View.Caret.Position))

    member private this.CaretPositionChanged =
        EventHandler<_>(fun _ (e : CaretPositionChangedEventArgs) ->
            this.UpdateAtCaretPosition(e.NewPosition))

    member this.Initialize() =
        this.View.LayoutChanged.AddHandler(this.ViewLayoutChanged)
        this.View.Caret.PositionChanged.AddHandler(this.CaretPositionChanged)

    interface ITagger<HighlightUsageTag> with
        member this.GetTags (spans : NormalizedSnapshotSpanCollection) : ITagSpan<HighlightUsageTag> seq =
            seq {
                if not this.CurrentWord.HasValue then ()
                else
                    // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
                    // collection throughout
                    let currentWord = ref this.CurrentWord.Value
                    let wordSpans = ref this.WordSpans
                    if spans.Count = 0 || (!wordSpans).Count = 0 then ()
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        if spans.[0].Snapshot <> (!wordSpans).[0].Snapshot then
                            wordSpans := 
                                NormalizedSnapshotSpanCollection(!wordSpans |> Seq.map (fun span -> 
                                    span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)))
                            currentWord := (!currentWord).TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)
                        // First, yield back the word the cursor is under (if it overlaps)
                        // Note that we'll yield back the same word again in the wordspans collection;
                        // the duplication here is expected.
                        if spans.OverlapsWith(NormalizedSnapshotSpanCollection(!currentWord)) then
                            yield TagSpan<HighlightUsageTag>(!currentWord, HighlightUsageTag()) :> ITagSpan<_>

                        // Second, yield all the other words in the file
                        for span in NormalizedSnapshotSpanCollection.Overlap(spans, !wordSpans) ->
                            TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>
            }
        member x.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member x.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)
         
    interface IDisposable with
        member this.Dispose() = 
            this.View.LayoutChanged.RemoveHandler(this.ViewLayoutChanged)
            this.View.Caret.PositionChanged.RemoveHandler(this.CaretPositionChanged)
