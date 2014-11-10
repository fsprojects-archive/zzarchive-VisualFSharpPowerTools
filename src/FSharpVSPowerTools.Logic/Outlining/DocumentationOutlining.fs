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
open Lens
open Lens.Operators

module DocumentationOutlining =
    

    let startDoc = "///"
    let collapsed = "...."
    let endHide   = "///"
    let tooltip   = "showhovered"

    [<Struct>]
    type Region =
        val StartLine   : int
        ///val StartOffset : int
        val EndLine     : int
        //val Level       : int
        //val Parent      : Region option
        //val IsRegion    : bool

        new ( startline, (*startoffset,*) endline ) =
            {   StartLine   = startline
       ///         StartOffset = startoffset
                EndLine     = endline        }
    
    let region fst lst = Region(fst,lst)
//        static member Startline_ =
//            lens ( fun   (r:Region) -> r.Startline )
//                 ( fun v (r:Region) -> Region(v,r.StartOffset,r.Endline))
//        
//        static member StartOffset_ =
//            lens ( fun   (r:Region) -> r.StartOffset)
//                 ( fun v (r:Region) -> Region(r.Startline,v,r.Endline))
//
//        static member Endline_ =
//            lens ( fun   (r:Region) -> r.Endline )
//                 ( fun v (r:Region) -> Region(r.Startline,r.StartOffset,v))


    let validStarts = [ "///"; "//"; "*)"; "(*"; "" ]


    let lineMatch (line:ITextSnapshotLine) start = 
        line.GetText().Trim().StartsWith(start)


    let lineMatchAny (line:ITextSnapshotLine) ls =
        List.fold( fun acc start -> 
                       acc || lineMatch line start ) false ls

    let filterLines (ls:ITextSnapshotLine list) : ITextSnapshotLine list =
        ls |> List.filter( fun line -> lineMatchAny line validStarts )

    
    let groupRegionLines ( ls:ITextSnapshotLine list ) =

        let rec findStart = function
            | hd::_ when lineMatch hd startDoc -> ls
            | _::tl                            -> findStart tl
            | []                                -> []

        let rec findEnd (acc:ITextSnapshotLine list) prevNum (ls:ITextSnapshotLine list) =
            let rec loop (acc:ITextSnapshotLine list) prevNum (ls:ITextSnapshotLine list) =
                match ls with 
                | hd::tl when ( not <| lineMatchAny hd validStarts ) 
                                -> acc, tl
                | hd::tl        -> findEnd (hd::acc) hd.LineNumber tl
                | []            -> acc, []
            let prevNum' = if acc.Length = 1 then acc.Head.LineNumber else prevNum
            if acc.Length > 1 && ls <> [] then 
                if prevNum'+1<>ls.Head.LineNumber then 
                    acc,ls
                else 
                    loop acc prevNum' ls
            else 
                loop acc prevNum' ls

        let rec findRegions (acc:ITextSnapshotLine list list) ls =
            match findStart ls |> findEnd [] 0 with
            | [],[]     -> []
            | [], tl    -> findRegions acc tl
            | rg, tl    -> findRegions (rg::acc) tl
            
        findRegions [] ls
        //|> List.map ( fun x -> List.toArray x )
        //|> List.toArray


    let reorderRegionLines (regls:ITextSnapshotLine list list) =
        regls |> List.map ( fun r -> 
            r |> List.sortWith ( fun l1 l2 -> compare l1.LineNumber l2.LineNumber ))


    let buildRegions (regls:ITextSnapshotLine list list) =
        let build (ls:ITextSnapshotLine list) =
            let fstline = ls.[0]
            let lstline = ls.[ls.Length-1]
            region fstline.LineNumber lstline.LineNumber
        regls |> List.map build

    let docRegions = filterLines >> groupRegionLines >> reorderRegionLines >> buildRegions

    type OutliningTagger(buffer:ITextBuffer) as self =
        
        let subscriptions   = ResizeArray<IDisposable>()
        
        do 
            buffer.Changed.Subscribe(self.BufferChanged) |> self.UnsubscribeOnDispose
            ()


        member __.UnsubscribeOnDispose idisposable =
            subscriptions.Add idisposable

        member val Buffer   = buffer with get, set
        member val Snapshot = buffer.CurrentSnapshot with get, set
        member val Regions: Region list  = [] with get, set
        //member val Regions  = List<Region>()



        member __.Reparse(newSnapshot:ITextSnapshot) =

            self.Regions <- docRegions <| List.ofSeq newSnapshot.Lines


            ()

        member __.BufferChanged( args:TextContentChangedEventArgs  ) =
            if args.After <> self.Snapshot then 
                self.Reparse(self.Snapshot)

        interface IDisposable with
            member x.Dispose(): unit = 
                subscriptions |> Seq.iter (fun x -> x.Dispose())
                subscriptions.Clear()

        interface ITagger<IOutliningRegionTag> with

            member x.GetTags ( spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<IOutliningRegionTag>> = 
                // TODO - Maybe i should implement get tags with a seq workflow
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
            member x.TagsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs> = 
                Unchecked.defaultof<_>
            

