namespace FSharpVSPowerTools.HighlightUsage

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic
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
type HighlightUsageTagger(v : ITextView, sb : ITextBuffer, ts : ITextSearchService, tn : ITextStructureNavigator) as self =
    let tagsChanged = Event<_, _>()
    let updateLock = obj()

    let mutable view = v
    let mutable sourceBuffer = sb
    let mutable textSearchService = ts
    let mutable textStructureNavigator = tn
    let mutable wordSpans = NormalizedSnapshotSpanCollection()
    let mutable currentWord = Nullable<SnapshotSpan>()
    let mutable requestedPoint = SnapshotPoint()

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate(currentRequest : SnapshotPoint, newSpans : NormalizedSnapshotSpanCollection,
                          newWord : Nullable<SnapshotSpan>) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                wordSpans <- newSpans
                currentWord <- newWord
                tagsChanged.Trigger(self, SnapshotSpanEventArgs(SnapshotSpan(sourceBuffer.CurrentSnapshot, 0, 
                                                                    sourceBuffer.CurrentSnapshot.Length))))

    let doUpdate(currentRequest : SnapshotPoint, newWord : SnapshotSpan, newWordSpans : SnapshotSpan seq) =
        if currentRequest = requestedPoint then
            lock updateLock (fun () ->
                let (beginLine, beginCol, _, _) = newWord.GetRange()
                let dte = Package.GetGlobalService(typedefof<DTE>) :?> DTE
                let doc = dte.ActiveDocument
                Debug.Assert(doc <> null && doc.ProjectItem.ContainingProject <> null, "Should be able to find active document.")
                let project = doc.ProjectItem.ContainingProject.Object :?> VSProject
                if project = null then
                    Debug.WriteLine("Can't find containing project. Probably the document is opened in an ad-hoc way.")
                    synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), Nullable())
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
                        let possibleSpans = HashSet(newWordSpans)
                        // TODO: use lastIdent to highlight the relevant part of the long identifier
                        let foundUsages =
                            references
                            |> Seq.map (fun (fileName, ((beginLine, beginCol), (endLine, endCol))) -> 
                                // Range01 type consists of zero-based values, which is a bit confusing
                                fileName, fromPosition(newWord.Snapshot, beginLine + 1, beginCol, endLine + 1, endCol))
                            |> Seq.filter (fun (fileName, span) -> fileName = currentFile && possibleSpans.Contains(span))
                            |> Seq.map snd
                        synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(foundUsages), Nullable newWord)
                    | None ->
                        // Return empty values in order to clear up markers
                        synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), Nullable())
            )

    let updateWordAdornments(_threadContext : obj) =
        let currentRequest = requestedPoint

        // Find all words in the buffer like the one the caret is on
        match textStructureNavigator.FindAllWords(currentRequest) with
        | None ->
            // If we couldn't find a word, just clear out the existing markers
            synchronousUpdate(currentRequest, NormalizedSnapshotSpanCollection(), Nullable())
        | Some newWord ->
            // If this is the same word we currently have, we're done (e.g. caret moved within a word).
            if currentWord.HasValue && newWord = currentWord.Value then ()
            else
                // Find the new spans
                let mutable findData = FindData(newWord.GetText(), newWord.Snapshot)
                findData.FindOptions <- FindOptions.WholeWord ||| FindOptions.MatchCase
                let newSpans = textSearchService.FindAll(findData)
                // If we are still up-to-date (another change hasn't happened yet), do a real update))
                doUpdate(currentRequest, newWord, newSpans)

    let updateAtCaretPosition(caretPosition : CaretPosition) =
        let point = caretPosition.Point.GetPoint(sourceBuffer, caretPosition.Affinity)

        if point.HasValue then 
            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            if currentWord.HasValue &&
               currentWord.Value.Snapshot = view.TextSnapshot &&
               point.Value.CompareTo(currentWord.Value.Start) >= 0 &&
               point.Value.CompareTo(currentWord.Value.End) <= 0 then ()
            else
                requestedPoint <- point.Value
                ThreadPool.QueueUserWorkItem(new WaitCallback(updateWordAdornments)) |> ignore

    let viewLayoutChanged = 
        EventHandler<_>(fun _ (e : TextViewLayoutChangedEventArgs) ->
            // If a new snapshot wasn't generated, then skip this layout 
            if e.NewSnapshot <> e.OldSnapshot then  
                updateAtCaretPosition(view.Caret.Position))

    let caretPositionChanged =
        EventHandler<_>(fun _ (e : CaretPositionChangedEventArgs) ->
            updateAtCaretPosition(e.NewPosition))

    do
      view.LayoutChanged.AddHandler(viewLayoutChanged)
      view.Caret.PositionChanged.AddHandler(caretPositionChanged)

    interface ITagger<HighlightUsageTag> with
        member __.GetTags (spans : NormalizedSnapshotSpanCollection) : ITagSpan<HighlightUsageTag> seq =
            seq {
                if not currentWord.HasValue then ()
                else
                    // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
                    // collection throughout
                    let newWord = ref currentWord.Value
                    let newWordSpans = ref wordSpans
                    if spans.Count = 0 || (!newWordSpans).Count = 0 then ()
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        if spans.[0].Snapshot <> (!newWordSpans).[0].Snapshot then
                            newWordSpans := 
                                NormalizedSnapshotSpanCollection(!newWordSpans |> Seq.map (fun span -> 
                                    span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)))
                            newWord := (!newWord).TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)
                        // First, yield back the word the cursor is under (if it overlaps)
                        // Note that we'll yield back the same word again in the wordspans collection;
                        // the duplication here is expected.
                        if spans.OverlapsWith(NormalizedSnapshotSpanCollection(!newWord)) then
                            yield TagSpan<HighlightUsageTag>(!newWord, HighlightUsageTag()) :> ITagSpan<_>

                        // Second, yield all the other words in the file
                        for span in NormalizedSnapshotSpanCollection.Overlap(spans, !newWordSpans) ->
                            TagSpan<HighlightUsageTag>(span, HighlightUsageTag()) :> ITagSpan<_>
            }
        member __.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member __.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)
         
    interface IDisposable with
        member __.Dispose() = 
            view.LayoutChanged.RemoveHandler(viewLayoutChanged)
            view.Caret.PositionChanged.RemoveHandler(caretPositionChanged)
