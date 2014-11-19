namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools

module DocumentationOutlining =
    
// #region Collapse
    let startDoc  = "///"
    let comment = "//"
    let starcomStart ="(*"
    let starcomEnd  = "*)"

    let collapsed = "...."
    let endHide   = "///"
    let tooltip   = "showhovered"

    [<Struct>]
    // TODO - Add vals to region to set the collapsed line value and tooltip
    type Region =
        val StartLine   : int
        val EndLine     : int



//        new ( startline, endline ) =
//            {   StartLine   = startline
//                EndLine     = endline        }

        new ( startline, endline ) =
            {   StartLine   = startline
                EndLine     = endline        }
        override self.ToString() = 
            sprintf "[Region {ln %d - ln %d}]" self.StartLine self.EndLine
  
  
    let validStarts = [ "///"; "//"; "*)"; "(*";]
    

    // #region Functions
    let lineMatch (line:ITextSnapshotLine) start = 
        line.GetText().TrimStart().StartsWith(start)

    let lineMatchAny (line:ITextSnapshotLine) ls =
        List.fold( fun acc start -> 
                       acc || lineMatch line start ) false ls

    let lineContains (line:ITextSnapshotLine) token = 
        line.GetText().Contains(token)

    let takeBlocks ( lines:seq<ITextSnapshotLine> )= 
        let rec loop (lines:seq<ITextSnapshotLine>) =
            seq{    if lines.Count() = 0 then () else 
                    let block = lines
                                |> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                                |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
                    let rest = lines|> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc ) 
                                    |> Seq.skip ( Seq.length block )
                    yield block
                    yield! loop rest
                }
        loop lines |> Seq.map(List.ofSeq) |> List.ofSeq



    let buildRegions (regls:ITextSnapshotLine list list) =
        let build (ls:ITextSnapshotLine list) =
            match ls with 
            | [] -> None
            | _  -> let fstline = ls.[0].LineNumber
                    let lstline = ls.[ls.Length-1].LineNumber
                    if fstline = lstline then None else
                    Some <| Region( fstline, lstline)
        match regls with
            | [] -> []
            | _  -> regls   |> List.map build 
                            |> List.filter ( fun x -> x <> None ) 
                            |> List.map Option.get 



    let asSnapshotSpan (region:Region) (snapshot:ITextSnapshot) =
        let startLine   = snapshot.GetLineFromLineNumber(region.StartLine)
        let endLine     = if region.StartLine= region.EndLine 
                            then startLine 
                            else snapshot.GetLineFromLineNumber(region.EndLine)
        SnapshotSpan(startLine.Start, endLine.End)

    //#endregion

    let getTags ( spans     : NormalizedSnapshotSpanCollection  )  
                ( regions   : Region list                       )
                ( snapshot  : ITextSnapshot                     ) : IEnumerable<ITagSpan<IOutliningRegionTag>> = 
        seq {
                debug "trying to yield tags"
                if spans.Count = 0 then
                    yield TagSpan(SnapshotSpan(),OutliningRegionTag() :> IOutliningRegionTag) :> ITagSpan<IOutliningRegionTag>

                let currentRegions, currentSnapshot = regions, snapshot
                let snapEntire = SnapshotSpan(spans.[0].Start, spans.[spans.Count-1].End).TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive )
                let startLineNum = snapEntire.Start.GetContainingLine().LineNumber
                let endLineNum   = snapEntire.End.GetContainingLine().LineNumber

                for rg in currentRegions do 
                    if rg.StartLine <= endLineNum && rg.EndLine >= startLineNum then
                        let startLine = currentSnapshot.GetLineFromLineNumber(rg.StartLine)
                        let endLine = currentSnapshot.GetLineFromLineNumber(rg.EndLine)
                                    
                        yield TagSpan<IOutliningRegionTag>
                                (   SnapshotSpan(startLine.Start , endLine.End) ,
                                    OutliningRegionTag(false, false, collapsed, tooltip)) :> ITagSpan<IOutliningRegionTag>
            }    


    type OutliningTagger(buffer:ITextBuffer) as self =
        
        let mutable _snapshot = buffer.CurrentSnapshot
        let _tagsChanged   = Event<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>() 
        let mutable _regions: Region list = []
       
        do  
            self.Reparse()
           
            buffer.Changed.Add(self.BufferChanged) 

        member __.Buffer  with get() = buffer

        member __.TagsChanged = _tagsChanged.Publish

        member __.Snapshot 
            with get() = _snapshot
            and  set v = _snapshot <- v

        member __.Regions 
            with get() = _regions 
            and  set v = _regions <- v

        member __.Reparse() =
            let newSnapshot  = buffer.CurrentSnapshot

            
            let newRegions = 
                if newSnapshot <> null then 
                   takeBlocks newSnapshot.Lines     |> buildRegions                     
                else []

            let oldSpans = 
                self.Regions |> List.map ( fun r -> 
                    ( asSnapshotSpan r self.Snapshot).TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive).Span)

            let newSpans = 
                newRegions |> List.map ( fun r -> 
                    ( asSnapshotSpan r newSnapshot).Span)

            let oldSpanCollection = NormalizedSpanCollection oldSpans
            let newSpanCollection = NormalizedSpanCollection newSpans
            let removed = NormalizedSpanCollection.Difference( oldSpanCollection, newSpanCollection )

            let changeStart = if removed.Count  > 0 then removed.[0].Start               else Int32.MaxValue
            let changeEnd   = if removed.Count  > 0 then removed.[removed.Count-1].End   else -1

            let changeStart'= if newSpans.Count() > 0 then 
                                Math.Min(changeStart, newSpans.ElementAt(0).Start ) else changeStart
            let changeEnd'  = if newSpans.Count() > 0 then 
                                Math.Max(changeEnd  , newSpans.ElementAt(newSpans.Count()-1).End ) else changeEnd

            self.Snapshot <- newSnapshot
            self.Regions  <- newRegions
            let eventSpan = SnapshotSpan( self.Snapshot, Span.FromBounds(changeStart',changeEnd'))

            if changeStart' <= changeEnd' then
                _tagsChanged.Trigger(self, SnapshotSpanEventArgs eventSpan )


        member __.BufferChanged( args:TextContentChangedEventArgs  ) =
            if args.After <> self.Snapshot then 
                self.Reparse()


        interface ITagger<IOutliningRegionTag> with

            member __.GetTags ( spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<IOutliningRegionTag>> = 
                seq {
                        if spans.Count = 0 then
                            yield TagSpan(SnapshotSpan(),OutliningRegionTag() :> IOutliningRegionTag) :> ITagSpan<IOutliningRegionTag>

                        let currentRegions, currentSnapshot = self.Regions, self.Snapshot
                        let snapEntire = SnapshotSpan(spans.[0].Start, spans.[spans.Count-1].End).TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive )
                        let startLineNum = snapEntire.Start.GetContainingLine().LineNumber
                        let endLineNum   = snapEntire.End.GetContainingLine().LineNumber

                        for rg in currentRegions do 
                            if rg.StartLine <= endLineNum && rg.EndLine >= startLineNum then
                                let startLine = currentSnapshot.GetLineFromLineNumber(rg.StartLine)
                                let endLine = currentSnapshot.GetLineFromLineNumber(rg.EndLine)

                                yield TagSpan<IOutliningRegionTag>
                                            (   SnapshotSpan(startLine.Start , endLine.End) ,
                                                OutliningRegionTag(false, false, collapsed, tooltip)) :> ITagSpan<IOutliningRegionTag>
                    }


            [<CLIEvent>]
            member __.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
                _tagsChanged.Publish
  
        
    [<Export(typeof<ITaggerProvider>)>]
    [<TagType(typeof<IOutliningRegionTag>)>]
    [<ContentType("F#")>]
    type OutliningTaggerProvider() =
        interface ITaggerProvider with
            member __.CreateTagger<'T when 'T :> ITag>(buffer: ITextBuffer): ITagger<'T>  = 
                let sc = Func<ITagger<'T>>( fun () -> new OutliningTagger(buffer) :> obj :?> ITagger<_> )
                buffer.Properties.GetOrCreateSingletonProperty<ITagger<_>>(sc)
 


