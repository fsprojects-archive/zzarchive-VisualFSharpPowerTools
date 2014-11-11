namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition

open EnvDTE
open Microsoft.VisualStudio.Text.Outlining
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open System.Windows.Threading
open FSharpVSPowerTools

module DocumentationOutlining =
    
//    [<Struct; CustomComparison >]
//    type SnapshotLineRange =
//        val Snapshot        : ITextSnapshot
//        val StartLineNumber : int
//        val Count           : int
//
//        new ( snapshot, startline, count ) =
//            {   Snapshot        = snapshot
//                StartLineNumber = startline 
//                Count           = count      }
//
//        member x.Startline 
//            with get() = x.Snapshot.GetLineFromLineNumber x.StartLineNumber
//
//        member x.Start 
//            with get() = x.Startline.Start
//
//        member x.LastLineNumber 
//            with get() = x.StartLineNumber + x.Count - 1 
//
//        member x.LastLine
//            with get() = x.Snapshot.GetLineFromLineNumber x.LastLineNumber
//
//        member x.LineRange 
//            with get() = LineRange()




    let startDoc = "///"
    let collapsed = "...."
    let endHide   = "///"
    let tooltip   = "showhovered"

    [<Struct>]
    type Region =
        val StartLine   : int
        val EndLine     : int


        new ( startline, (*startoffset,*) endline ) =
            {   StartLine   = startline
       ///         StartOffset = startoffset
                EndLine     = endline        }
  
  
    let validStarts = [ "///"; "//"; "*)"; "(*";]
    
    

    let lineMatch (line:ITextSnapshotLine) start = 
        line.GetText().TrimStart().StartsWith(start)

    let lineMatchAny (line:ITextSnapshotLine) ls =
        List.fold( fun acc start -> 
                       acc || lineMatch line start ) false ls

    let takeBlocks ( lines:seq<ITextSnapshotLine> ) = 
        let rec loop lines =
            seq{if Seq.isEmpty lines then () else 
                let block = lines
                            |> Seq.skipWhile ( fun ln -> not <| lineMatch ln startDoc )
                            |> Seq.takeWhile ( fun ln -> lineMatch ln startDoc )
                let rest = lines |> Seq.skip ( Seq.length block )
                yield block
                yield! loop rest}
        loop lines |> Seq.map(List.ofSeq) |> List.ofSeq


    let buildRegions (regls:ITextSnapshotLine list list) =
        let build (ls:ITextSnapshotLine list) =
            match ls with 
            | [] -> None
            | _  -> let fstline = ls.[0].LineNumber
                    let lstline = ls.[ls.Length-1].LineNumber
                    Some <| Region( fstline, lstline)
        match regls with
        | [] -> []
        | _  -> regls |> List.map build 
                      |> List.filter ( fun x -> x <> None ) 
                      |> List.map Option.get 

    let docRegions = takeBlocks >> buildRegions


    let asSnapshotSpan (region:Region) (snapshot:ITextSnapshot) =
        let startLine   = snapshot.GetLineFromLineNumber(region.StartLine)
        let endLine     = if region.StartLine= region.EndLine 
                            then startLine 
                            else snapshot.GetLineFromLineNumber(region.EndLine)
        SnapshotSpan(startLine.Start, endLine.End)


    type taggerDel<'T when 'T :> ITag> = delegate of unit -> ITagger<'T>




    type OutliningTagger(buffer:ITextBuffer) as self =
        
      //  let subscriptions = ResizeArray<IDisposable>()
        let tagsChanged   = Event<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>() 
       // let mutable textBuffer = buffer
//        do 
//            self.Reparse(buffer.CurrentSnapshot)
//            buffer.Changed.Subscribe(self.BufferChanged) |> self.UnsubscribeOnDispose
        do
            debug "Outlining Tagger was created"
            buffer.Changed.Add(self.BufferChanged) 


//        member __.UnsubscribeOnDispose idisposable =
//            subscriptions.Add idisposable

        member __.ITagger = self :> ITagger<IOutliningRegionTag>

//        member __.Buffer 
//            with get() = textBuffer
//            and  set v = textBuffer <- v

        member val Buffer = buffer with get, set
        member val Snapshot = buffer.CurrentSnapshot with get, set
        member val Regions: Region list  = [] with get, set
        //member val Regions  = List<Region>()

        member __.Reparse() =
            let newSnapshot  = buffer.CurrentSnapshot
            
//            ( takeBlocks buffer.CurrentSnapshot.Lines
//                |> Seq.map( fun ln ->
//                ln  |> Seq.map string
//                    |> Seq.reduce ( fun a b -> a + ", " + b )))
//                |> Seq.iter( fun x -> debug "Block -- %A"  x  )    
//            
            debug "Attempting to reparse"
            let newRegions = if newSnapshot <> null then docRegions  newSnapshot.Lines else []
            debug "Found >>> %A Regions <<<" newRegions.Length

            let oldSpans = self.Regions.Select(fun r -> 
                    (asSnapshotSpan r self.Snapshot).TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive).Span)

            let newSpans = newRegions.Select(fun r -> 
                    (asSnapshotSpan r newSnapshot).Span)

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
                tagsChanged.Trigger(self, SnapshotSpanEventArgs eventSpan )


        member __.BufferChanged( args:TextContentChangedEventArgs  ) =
            if args.After <> self.Snapshot then 
                self.Reparse()


//        interface IDisposable with
//            member x.Dispose(): unit = 
//                subscriptions |> Seq.iter (fun x -> x.Dispose())
//                subscriptions.Clear()

        //interface ITagger<IOutliningRegionTag> with
        interface ITagger<IOutliningRegionTag> with

            member x.GetTags ( spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<IOutliningRegionTag>> = 
                seq {
                        debug "trying to yield tags"
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
            member x.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
                tagsChanged.Publish
        
    [<Export(typeof<ITaggerProvider>)>]
    [<TagType(typeof<IOutliningRegionTag>)>]
    [<ContentType("F#")>]
    type OutliningTaggerProvider   () =
        interface ITaggerProvider with

            member x.CreateTagger<'T when 'T :> ITag>(buffer: ITextBuffer): ITagger<'T> = 
                debug "trying to create tagger"
                let sc = Func<ITagger<'T>>( fun () -> new OutliningTagger(buffer) :> obj :?> ITagger<_> )
                buffer.Properties.GetOrCreateSingletonProperty<ITagger<_>>(sc)
            
            


//
//    let filterLines (ls:ITextSnapshotLine list) : ITextSnapshotLine list =
//        ls |> List.filter( fun line -> lineMatchAny line validStarts )
//
//    
//    let groupRegionLines ( ls:ITextSnapshotLine list ):ITextSnapshotLine list list =
//
//        let rec findStart = function
//            | hd::_ when lineMatch hd startDoc -> //debug "found the start of a region in %A" ls
//                                                  ls
//            | _::tl                            -> findStart tl
//            | []                                -> []
//
//        let rec findEnd (acc:ITextSnapshotLine list) prevNum (ls:ITextSnapshotLine list) =
//            let rec loop (acc:ITextSnapshotLine list) prevNum (ls:ITextSnapshotLine list) =
//                match ls with 
//                | hd::tl when ( not <| lineMatchAny hd validStarts ) 
//                                -> acc, tl
//                | hd::tl        -> findEnd (hd::acc) hd.LineNumber tl
//                | []            -> acc, []
//            let prevNum' = if acc.Length = 1 then acc.Head.LineNumber else prevNum
//            if acc.Length > 1 && ls <> [] then 
//                if prevNum'+1<>ls.Head.LineNumber then 
//                    acc,ls
//                else 
//                    loop acc prevNum' ls
//            else 
//                loop acc prevNum' ls
//
//        let findRegion ls = findStart ls |> findEnd [] 0
//
//
//        let rec findRegions (acc:ITextSnapshotLine list list) ls =
//            match findRegion ls with
//            | [], _     -> []
//            | rg, tl    -> findRegions (rg::acc) tl
//        debug "finding regions in %A lines" ls.Length
//        findRegions [] ls
//
//
//
//    let reorderRegionLines (regls:ITextSnapshotLine list list) =
//        regls |> List.map ( fun r -> 
//            r |> List.sortWith ( fun l1 l2 -> compare l1.LineNumber l2.LineNumber ))
