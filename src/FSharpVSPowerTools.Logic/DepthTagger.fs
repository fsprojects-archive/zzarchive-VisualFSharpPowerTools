namespace FSharpVSPowerTools.DepthColorizer
   
open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open System.Threading
open System.Diagnostics

// The tag that carries metadata about F# color-regions.
type DepthRegionTag(info: int * int * int * int) = 
    interface ITag
    // why are (line,startColumn,endColumn,depth) here, and not just depth?  
    // because we might have range info for indent coloring on a blank line, and there are no chars to tag there, so we put a tag in column 0 and carry all this info as metadata
    member __.Info = info

[<NoComparison>]
type private DepthTaggerState =
    { Snapshot: ITextSnapshot
      Tags: ITagSpan<DepthRegionTag>[]
      Results: (ITrackingSpan * (int * int * int * int)) list }

type DepthTagger(buffer: ITextBuffer, filename: string, fsharpLanguageService: VSLanguageService) as self =
    // computed periodically on a background thread
    let lastResults = Atom []
    // only updated on the UI thread in the GetTags method
    let mutable state = None
    let tagsChanged = Event<_,_>()
    
    let refreshTags() = 
        let uiContext = SynchronizationContext.Current
        async { 
            let snapshot = buffer.CurrentSnapshot // this is the possibly-out-of-date snapshot everyone here works with
            let sourceCodeOfTheFile = snapshot.GetText()                        
            let! ranges = fsharpLanguageService.CheckerAsync (fun x ->
                            DepthParser.GetNonoverlappingDepthRanges(sourceCodeOfTheFile, filename, x))
            let newResults =
                ranges 
                |> Seq.fold (fun res ((line, startCol, endCol, _) as info) ->
                    try 
                        // -1 because F# reports 1-based line nums, whereas VS wants 0-based
                        let startLine = snapshot.GetLineFromLineNumber (min (line - 1) (snapshot.LineCount - 1))
                        let startPoint = startLine.Start.Add (min startCol startLine.Length)
                        let endLine = snapshot.GetLineFromLineNumber (min (line - 1) (snapshot.LineCount - 1))
                        let endPoint = endLine.Start.Add (min endCol endLine.Length)
                        let trackingSpan = 
                            snapshot.CreateTrackingSpan
                                (SnapshotSpan(startPoint, endPoint).Span, SpanTrackingMode.EdgeExclusive)
                        (trackingSpan, info) :: res
                    with e -> 
                        Logging.logException e
                        if Debugger.IsAttached then Debugger.Break()
                        res) []
                |> List.rev

            lastResults.Swap (fun _ -> newResults) |> ignore
            debug "[DepthTagger] Firing tags changed"
            // Switch back to UI thread before firing events
            do! Async.SwitchToContext(uiContext)
            tagsChanged.Trigger (self, SnapshotSpanEventArgs (SnapshotSpan (snapshot, 0, snapshot.Length)))
        } 
        |> Async.StartInThreadPoolSafe
    
    let docEventListener = new DocumentEventListener ([ViewChange.bufferEvent buffer], 500us, refreshTags) 
    
    let getTags (spans: NormalizedSnapshotSpanCollection) = 
        match spans |> Seq.toList, state with
        | [], _ -> [||]
        | firstSpan :: _, Some state 
            when state.Snapshot === firstSpan.Snapshot && state.Results === lastResults.Value ->
            debug "[DepthTagger] Using cached results"
            state.Tags
        | firstSpan :: _, _ ->
            debug "[DepthTagger] Computing fresh results"
            let snapshot = firstSpan.Snapshot
            let tags = [| for span, depth in lastResults.Value ->
                            TagSpan(span.GetSpan(snapshot), DepthRegionTag(depth)) :> ITagSpan<_> |]
            state <- Some { Snapshot = snapshot; Tags = tags; Results = lastResults.Value }
            tags

    interface ITagger<DepthRegionTag> with
        member __.GetTags spans = 
            protectOrDefault (fun _ -> getTags spans :> _) Seq.empty
        
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
         
