namespace FSharpVSPowerTools.HighlightUsage

open System
open System.IO
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open FSharpPowerTools.Core.HighlightUsageInFile
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices

// Reference at http://social.msdn.microsoft.com/Forums/vstudio/en-US/8e0f71f6-4794-4f0e-9a63-a8b55bc22e00/predefined-textmarkertag?forum=vsx

[<AutoOpen>]
module private Utils =
    let getMarker vsVersion isDef =
        if isDef && vsVersion >= VisualStudioVersion.VS2015 then
            "MarkerFormatDefinition/HighlightedDefinition"
        else
            "MarkerFormatDefinition/HighlightedReference"

type HighlightUsageTag(vsVersion, isDef) = 
    inherit TextMarkerTag(getMarker vsVersion isDef)
    member __.IsFromDefinition = isDef

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type HighlightUsageTagger(doc: ITextDocument,
                          view: ITextView, 
                          vsLanguageService: VSLanguageService, 
                          serviceProvider: IServiceProvider,
                          projectFactory: ProjectFactory) as self =
    let tagsChanged = Event<_, _>()
    let updateLock = obj()
    let mutable wordSpans = []
    let mutable currentWord: SnapshotSpan option = None
    let mutable requestedPoint = SnapshotPoint()

    let buffer = view.TextBuffer

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate (currentRequest, newSpans, newWord) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                wordSpans <- newSpans
                currentWord <- newWord
                let span = buffer.CurrentSnapshot.FullSpan
                tagsChanged.Trigger(self, SnapshotSpanEventArgs span))

    let symbolUsesToSpans (word: SnapshotSpan) fileName (lastIdent: string) (symbolUses: FSharpSymbolUse []) =
        let filePath = Path.GetFullPathSafe(fileName)
        symbolUses
        |> Seq.choose (fun symbolUse -> 
            // We have to filter by full paths otherwise the range is invalid wrt current snapshot
            if Path.GetFullPathSafe(symbolUse.FileName) = filePath then
                fromFSharpRange word.Snapshot symbolUse.RangeAlternate
                |> Option.map (fun range -> symbolUse.IsFromDefinition, range)
            else None)
        |> fixInvalidSymbolSpans word.Snapshot lastIdent

    let doUpdate (currentRequest: SnapshotPoint, symbol, newWord: SnapshotSpan,
                  fileName: string, projectProvider: IProjectProvider, CallInUIContext callInUIContext) =
        async {
            if currentRequest = requestedPoint then
                try
                    let! res = vsLanguageService.FindUsagesInFile (newWord, symbol, fileName, projectProvider, AllowStaleResults.MatchingSource)
                    match res with
                    | Some (UsageInFile (_, lastIdent, refs)) -> 
                        let references = symbolUsesToSpans newWord fileName lastIdent refs
                        // Ignore symbols without any use
                        let word = if List.isEmpty references then None else Some newWord
                        do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, references, word)
                    | _ -> 
                        // Return empty values in order to clear up markers
                        do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, [], None)
                with e ->
                    Logging.logExceptionWithContext(e, "Failed to update highlight references.")
                    do! callInUIContext <| fun _ -> synchronousUpdate (currentRequest, [], None)
        }

    let dte = serviceProvider.GetDte()
    let project() = projectFactory.CreateForDocument buffer doc.FilePath

    let updateAtCaretPosition ((CallInUIContext callInUIContext) as ciuc) =
        asyncMaybe {
            let caretPos = view.Caret.Position
            Logging.logInfo <| fun _ -> sprintf "Caret pos = %O" caretPos

            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            match buffer.GetSnapshotPoint caretPos, currentWord with
            | Some point, Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> 
                ()
            | Some point, _ ->
                requestedPoint <- point
                let currentRequest = requestedPoint
                let! project = project()
                return!
                    match vsLanguageService.GetSymbol(currentRequest, doc.FilePath, project) with
                    | Some (newWord, symbol) ->
                        // If this is the same word we currently have, we're done (e.g. caret moved within a word).
                        match currentWord with
                        | Some cw when cw = newWord -> 
                            async.Return None
                        | _ ->
                            // If we are still up-to-date (another change hasn't happened yet), do a real update
                            doUpdate (currentRequest, symbol, newWord, doc.FilePath, project, ciuc) |> liftAsync
                    | None ->
                        callInUIContext <| fun _ -> 
                            synchronousUpdate (currentRequest, [], None)
                        |> liftAsync
            | None, _ -> ()
        } 
        |> Async.Ignore

    let docEventListener = 
        new DocumentEventListener([ViewChange.layoutEvent view; ViewChange.caretEvent view], 100us, updateAtCaretPosition)

    let vsVersion = VisualStudioVersion.fromDTEVersion dte.Version

    let createHighlightUsageTag isDef span = 
        let highlightMarker = HighlightUsageTag(vsVersion, isDef)
        TagSpan<_>(span, highlightMarker) :> ITagSpan<_>

    let getTags (spans: NormalizedSnapshotSpanCollection) = 
        [
            match currentWord, wordSpans with
            | Some word, (_, firstWordSpan) :: _ when spans.Count > 0 -> 
                let currentSnapshot = spans.[0].Snapshot
                let wordSpans = 
                    if currentSnapshot = firstWordSpan.Snapshot then
                        wordSpans
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        wordSpans |> List.map (fun (isDef, span) -> isDef, span.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive))
                
                let word = 
                    if currentSnapshot = word.Snapshot then word
                    else word.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)
                
                let duplicated = ref false
                // First, yield all the other words in the file
                for (isDef, span) in wordSpans do
                    if not !duplicated && word = span then
                        duplicated := true
                    if spans.OverlapsWith span then
                        yield createHighlightUsageTag isDef span
                
                // Second, yield back the word the cursor is under (if it overlaps)
                // Note that we won't yield back the same word again in the word spans collection;
                // the duplication is not expected.
                if spans.OverlapsWith word && not !duplicated then 
                    yield createHighlightUsageTag false word
                
            | _ -> ()
        ]

    interface ITagger<HighlightUsageTag> with
        member __.GetTags spans =
            upcast (protectOrDefault (fun _ -> getTags spans) [])
        
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = dispose docEventListener
