namespace FSharpVSPowerTools.Outlining

open FSharpVSPowerTools.Outlining.Parsers
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System
open System.ComponentModel.Composition
open System.Linq
open System.Text

module Comments = 
    let encompassedBy (rg : Region) (encomp : Region) = rg.StartLine > encomp.StartLine && rg.EndLine < encomp.EndLine
    
    [<Struct>]
    type private ParsedRegions = 
        val DocBlocks : Region list
        val ComBlocks : Region list
        val StarBlocks : Region list
        new(docBlocks, comBlocks, starBlocks) = 
            { DocBlocks = docBlocks
              ComBlocks = comBlocks
              StarBlocks = starBlocks }
    
    let snapshotText (lines : seq<ITextSnapshotLine>) = 
        let folder (sb : StringBuilder) (ln : ITextSnapshotLine) = sb.Append(ln.GetText() + "\n")
        let sb = lines |> Seq.fold folder (StringBuilder())
        sb.ToString()
    
    let private constructAllRegions fileText = 
        let findInText psr = findInString fileText psr |> List.choose id
        let docCandidates = findInText docBlock
        let comCandidates = findInText comBlock
        let starCandidates = findInText starBlock
        let triStrings = findInText skipTriString
        let multiStrings = findInText skipMultiString
        let folder (rg : Region) (isEncompassed : bool) (encomp : Region) = isEncompassed || encompassedBy rg encomp
        let inline regionFilter (filterls : Region list) (rgls : Region list) = 
            rgls |> List.filter (fun rg -> not <| List.fold (folder rg) false filterls)
        let doubleFilter = regionFilter triStrings >> regionFilter multiStrings
        let starBlocksFinal = doubleFilter starCandidates
        let tripleFilter = doubleFilter >> regionFilter starBlocksFinal
        let docBlocksFinal = tripleFilter docCandidates
        let comBlocksFinal = tripleFilter comCandidates
        ParsedRegions(docBlocksFinal, comBlocksFinal, starBlocksFinal)
    
    let private asSnapshotSpan (region : Region) (snapshot : ITextSnapshot) = 
        if region.StartLine > snapshot.Length || region.EndLine > snapshot.Length then None
        else 
            let startLine = snapshot.GetLineFromLineNumber(region.StartLine)
            
            let endLine = 
                if region.StartLine = region.EndLine then startLine
                else snapshot.GetLineFromLineNumber(region.EndLine)
            Some <| SnapshotSpan(startLine.End - region.StartOffset, 
                                 if region.EndOffset = -1 then endLine.End
                                 else endLine.Start + region.EndOffset)
    
    let private parse (newSnapshot : ITextSnapshot) (oldSnapshot : ITextSnapshot) (oldRegions : Region list) 
        (newRegions : Region list) = 
        let oldSpans : Span list = 
            oldRegions
            |> List.map (fun r -> 
                   let rs = (asSnapshotSpan r oldSnapshot)
                   if rs.IsSome then Some <| rs.Value.TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive).Span
                   else None)
            |> List.choose id
        
        let newSpans : Span list = 
            newRegions
            |> List.map (fun r -> 
                   let rs = (asSnapshotSpan r newSnapshot)
                   if rs.IsNone then None
                   else Some rs.Value.Span)
            |> List.choose id
        
        let oldSpanCollection = NormalizedSpanCollection oldSpans
        let newSpanCollection = NormalizedSpanCollection newSpans
        let removed = NormalizedSpanCollection.Difference(oldSpanCollection, newSpanCollection)
        
        let changeStart : int = 
            if removed.Count > 0 then removed.[0].Start
            else Int32.MaxValue
        
        let changeEnd : int = 
            if removed.Count > 0 then removed.[removed.Count - 1].End
            else -1
        
        let changeStart' : int = 
            if newSpans.Count() > 0 then min changeStart (newSpans.ElementAt(0).Start)
            else changeStart
        
        let changeEnd' : int = 
            if newSpans.Count() > 0 then max changeEnd (newSpans.ElementAt(newSpans.Count() - 1).End)
            else changeEnd
        
        let eventSpan : SnapshotSpan option = 
            if changeStart' > changeEnd' then None
            else Some <| SnapshotSpan(newSnapshot, Span.FromBounds(changeStart', changeEnd'))
        
        (newSnapshot, eventSpan)
    
    let private generateTags (spans : NormalizedSnapshotSpanCollection) (currentSnapshot : ITextSnapshot) 
        (currentRegions : Region list) : seq<ITagSpan<IOutliningRegionTag>> = 
        if spans.Count = 0 then [] :> seq<_>
        else 
            let snapEntire = 
                SnapshotSpan(spans.[0].Start, spans.[spans.Count - 1].End)
                    .TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)
            
            /// StartLineNumber for the entire textbuffer
            let startLineNum = snapEntire.Start.GetContainingLine().LineNumber
            
            /// EndLineNumber for the entire textbuffer
            let endLineNum = snapEntire.End.GetContainingLine().LineNumber
            
            let rec loop (acc : ITagSpan<IOutliningRegionTag> list) (currgs : Region list) = 
                match currgs with
                | rg :: tl when rg.StartLine <= endLineNum && rg.EndLine >= startLineNum -> 
                    let startLine = currentSnapshot.GetLineFromLineNumber(rg.StartLine)
                    let endLine = currentSnapshot.GetLineFromLineNumber(rg.EndLine)
                    
                    let tag = 
                        TagSpan<IOutliningRegionTag>
                            (//( SnapshotSpan(startLine.Start + rg.StartOffset, 
                             SnapshotSpan(startLine.End - rg.StartOffset, 
                                          if rg.EndOffset = -1 then endLine.End
                                          else endLine.Start + rg.EndOffset), 
                             OutliningRegionTag(false, false, rg.Collapsed, rg.Contents)) :> ITagSpan<IOutliningRegionTag>
                    loop (tag :: acc) tl
                | _ :: tl -> loop acc tl
                | [] -> acc
            
            loop [] currentRegions :> seq<_>
    
    [<NoComparison>]
    type private TaggerState = 
        { OutlineRegions : Region list
          Snapshot : ITextSnapshot }
    
    type OutliningTagger(buffer : ITextBuffer) as self = 
        let tagsChanged = Event<EventHandler<SnapshotSpanEventArgs>, SnapshotSpanEventArgs>()
        
        let mutable taggerState = 
            { OutlineRegions = []
              Snapshot = buffer.CurrentSnapshot }
        
        do 
            self.Reparse()
            buffer.Changed.Add(self.BufferChanged)
        
        member __.TagsChanged = tagsChanged.Publish
        
        member __.Reparse() = 
            if buffer.CurrentSnapshot = null then ()
            else 
                let regionRcd = 
                    buffer.CurrentSnapshot.Lines
                    |> snapshotText
                    |> constructAllRegions
                
                let allRegions = List.concat [ regionRcd.DocBlocks; regionRcd.ComBlocks; regionRcd.StarBlocks ]
                let newSnapshot, rgEventSpan = 
                    parse buffer.CurrentSnapshot taggerState.Snapshot taggerState.OutlineRegions allRegions
                taggerState <- { taggerState with Snapshot = newSnapshot
                                                  OutlineRegions = allRegions }
                if rgEventSpan.IsSome then tagsChanged.Trigger(self, SnapshotSpanEventArgs rgEventSpan.Value)
        
        member __.BufferChanged(args) = 
            if args.After <> buffer.CurrentSnapshot then ()
            else self.Reparse()
        
        interface ITagger<IOutliningRegionTag> with
            member __.GetTags(spans : NormalizedSnapshotSpanCollection) : seq<ITagSpan<IOutliningRegionTag>> = 
                generateTags spans taggerState.Snapshot taggerState.OutlineRegions
            [<CLIEvent>]
            member __.TagsChanged : IEvent<EventHandler<SnapshotSpanEventArgs>, SnapshotSpanEventArgs> = 
                tagsChanged.Publish
    
    [<Export(typeof<ITaggerProvider>)>]
    [<TagType(typeof<IOutliningRegionTag>)>]
    [<ContentType("F#")>]
    type OutliningTaggerProvider() = 
        interface ITaggerProvider with
            member __.CreateTagger buffer = 
                buffer.Properties.GetOrCreateSingletonProperty(fun () -> OutliningTagger(buffer) :> obj :?> _)
