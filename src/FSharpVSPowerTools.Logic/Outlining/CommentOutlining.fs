namespace FSharpVSPowerTools.Outlining

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Linq

module Comments = 
    /// <summary>
    /// Holds the data that will be used to construct the IOutliningRegionTag
    /// </summary>
    [<Struct>]
    type Region (startline: int, endline:int, collapsed:string, contents:string) = 
        /// Line number of the first line in the region
        member __.StartLine = startline
        /// Line number of the last line in the region
        member __.EndLine = endline
        /// The text displayed inline when a region is collapsed
        member __.Collapsed = collapsed
        /// The text displayed in the tooltip when a collapsed region is moused over
        member __.Contents = contents

        override x.ToString() = sprintf "[Region {ln %d - ln %d}]" x.StartLine x.EndLine
    
    /// Check if the given string matches the start of the ITextSnapshotLine's trimmed text 
    let private lineMatch (line : ITextSnapshotLine) start : bool = line.GetText().TrimStart().StartsWith(start)
    
    /// Uses the functions matchToken and notMatchToken to determine which
    /// lines will be collected into region blocks
    let private takeBlocksOf (lines : seq<ITextSnapshotLine>) (matchToken : ITextSnapshotLine -> bool) 
        (notMatchToken : ITextSnapshotLine -> bool) = 
        let rec loop (lines : seq<ITextSnapshotLine>) = 
            seq {   if lines.Count() = 0 then () else
                    /// List of consequtive ITextSnapshotLines that begin with a token
                    let block = lines |> Seq.skipWhile notMatchToken // skip lines that satisfy notMatchToken
                                      |> Seq.takeWhile matchToken    // take all consequtive lines that satisfy matchToken
                    /// sequence of elements in 'lines' following the block
                    let rest  = lines |> Seq.skipWhile notMatchToken
                                      |> Seq.skip (Seq.length block)
                    yield  block
                    yield! loop rest
                }
        loop lines |> Seq.map (List.ofSeq) |> List.ofSeq
    
    /// Token that indicates the start of a documentation comment line ' /// '
    let private doccom = "///"
    
    /// Token that indicates the start of a single line comment ' // '
    let private comment = "//"
    
    let private matchDoccom ln = lineMatch ln doccom
    let private matchComment ln = lineMatch ln comment && (matchDoccom >> not) ln
    let private notMatchComment ln = (not <| lineMatch ln comment) || matchDoccom ln
    let private doccomBlocks lines = takeBlocksOf lines matchDoccom (matchDoccom >> not) 
    let private commentBlocks lines = takeBlocksOf lines matchComment notMatchComment
    let private trimLineStart (ln : ITextSnapshotLine) = ln.GetText().TrimStart([| ' ' |])
    
    let private removeToken (str : string) (token : string) = 
        let idx = str.IndexOf(token)
        str.Remove(idx, token.Length)
    
    let private removeDoccom str = removeToken str doccom
    let private removeComment str = removeToken str comment
    let private filterWhitespace (ln : ITextSnapshotLine) = 
        String(ln.GetText().ToCharArray() |> Array.filter (fun ch -> ch <> ' '))
    
    /// <summary>
    /// <para> Construct a string that will be displayed when the mouse is over the region's collapse string </para>
    /// <para> 'removeFunc' is used cut the comment tokens out of the tooltip string </para>
    /// </summary>
    let private getTooltipOf (lines : ITextSnapshotLine list) (removeFunc : string -> string) = 
        match lines with
        | [] -> ""
        | lns -> lns |> List.map (trimLineStart >> removeFunc)
                     |> String.concat "\n"
    
    let private getDoccomTooltip lines = getTooltipOf lines removeDoccom
    let private getCommentTooltip lines = getTooltipOf lines removeComment
    let private summary = "<summary>"
    let private ellipsis = "..."
    
    /// Given a list of lines that comprise a documentation comment region, determine 
    /// the string to display when the region is collapsed
    let private getDoccomCollapsed (lines : ITextSnapshotLine list) : string = 
        if lines = [] then ellipsis else
        let fstLineText = lines.[0].GetText()
        match lines.[0] |> filterWhitespace |> removeDoccom with
        // if the filtered first line is shorter than summary, use that line
        | ffln when ffln.Length < summary.Length -> fstLineText + ellipsis
        // if the first line of the doc comment is only "<summary>", take the next line
        | ffln when ffln = summary &&
            lines.Length > 1 -> (lines.[1].GetText()) + ellipsis
        // if the description follows the <summary> tag on the same line drop the tag and take the rest
        | ffln when ffln.[0..summary.Length - 1] = summary -> 
            let sumStart = fstLineText.IndexOf(summary) + summary.Length
            /// whitespace preceeding the doccom token
            //let indent = fstLineText.Substring(0, fstLineText.IndexOf(doccom) + doccom.Length)
            let indent = fstLineText.[0..fstLineText.IndexOf(doccom) + doccom.Length]
            indent + fstLineText.[sumStart..fstLineText.Length - sumStart] + ellipsis
        // if xml tags aren't used, take the first line of the doc comment
        | _ -> fstLineText + ellipsis
    
    /// Given a list of lines that comprise a comment region, determine the string to display when the
    /// region is collapsed
    let private getCommentCollapsed (lines : ITextSnapshotLine list) : string = 
        match lines with 
        | [] -> ellipsis
        | _ -> lines.[0].GetText() + ellipsis
    
    /// <summary>
    /// <para> Takes an ITextSnapshotline list list, where each inner list comprises the scope of a region and  </para>
    /// <para> constructs a Region using the linenumber of the first and last line of the inner list            </para>
    /// </summary>
    let inline private parseRegionsOf (regionls : ITextSnapshotLine list list) 
               (getCollapsed : ITextSnapshotLine list -> string) (getTooltip : ITextSnapshotLine list -> string) : Region list = 
        let build (ls : ITextSnapshotLine list) = 
            match ls with
            | [] -> None
            | _  -> let fstline = ls.[0].LineNumber
                    let lstline = ls.[ls.Length - 1].LineNumber
                    if fstline = lstline then None else 
                    let collapsed = getCollapsed ls
                    let contents = getTooltip ls
                    Some <| Region(fstline, lstline, collapsed, contents)
        regionls
        |> List.map build
        |> List.choose id
    
    let private buildDoccomRegions ls = (doccomBlocks ls |> parseRegionsOf) getDoccomCollapsed getDoccomTooltip
    let private buildCommentRegions ls = (commentBlocks ls |> parseRegionsOf) getCommentCollapsed getCommentTooltip
    
    let private asSnapshotSpan (region : Region) (snapshot : ITextSnapshot) = 
        let startLine = snapshot.GetLineFromLineNumber(region.StartLine)
        
        let endLine = 
            if region.StartLine = region.EndLine then startLine
            else snapshot.GetLineFromLineNumber(region.EndLine)
        SnapshotSpan(startLine.Start, endLine.End)
    
    let private parse (buffer : ITextBuffer) (oldSnapshot : ITextSnapshot) (oldRegions : Region list) 
        (newRegions : Region list) = 
        let newSnapshot = buffer.CurrentSnapshot
        let oldSpans : Span list = 
            oldRegions 
            |> List.map 
                   (fun r -> 
                   (asSnapshotSpan r oldSnapshot).TranslateTo(newSnapshot, SpanTrackingMode.EdgeExclusive).Span)
        let newSpans : Span list = newRegions |> List.map (fun r -> (asSnapshotSpan r newSnapshot).Span)
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
            else Some <| SnapshotSpan(oldSnapshot, Span.FromBounds(changeStart', changeEnd'))
        
        (newSnapshot, eventSpan)
    
    let private generateTags (spans : NormalizedSnapshotSpanCollection) (currentSnapshot : ITextSnapshot) 
        (currentRegions : Region list) : IEnumerable<ITagSpan<IOutliningRegionTag>> = 
        seq { // If there are no spans yield an empty ITagSpan
            if spans.Count = 0 then 
                yield TagSpan(SnapshotSpan(), OutliningRegionTag() :> IOutliningRegionTag) :> ITagSpan<IOutliningRegionTag>
            /// SnapshotSpan for the entire textbuffer
            let snapEntire = 
                SnapshotSpan(spans.[0].Start, spans.[spans.Count - 1].End)
                    .TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)
            
            /// StartLineNumber for the entire textbuffer
            let startLineNum = snapEntire.Start.GetContainingLine().LineNumber
            
            /// EndLineNumber for the entire textbuffer
            let endLineNum = snapEntire.End.GetContainingLine().LineNumber
            
            // Yield a TagSpan for every region that was parsed
            for rg in currentRegions do
                if rg.StartLine <= endLineNum && rg.EndLine >= startLineNum then 
                    let startLine = currentSnapshot.GetLineFromLineNumber(rg.StartLine)
                    let endLine = currentSnapshot.GetLineFromLineNumber(rg.EndLine)
                    yield TagSpan<IOutliningRegionTag>
                              (SnapshotSpan(startLine.Start, endLine.End), 
                               OutliningRegionTag(false, false, rg.Collapsed, rg.Contents)) :> ITagSpan<IOutliningRegionTag>
        }
    
    [<NoComparison>]
    type TaggerState = 
        { DocRegions : Region list
          ComRegions : Region list
          DocSnapshot : ITextSnapshot
          ComSnapshot : ITextSnapshot }
    
    type OutliningTagger(buffer : ITextBuffer) as self = 
        let tagsChanged = Event<EventHandler<SnapshotSpanEventArgs>, SnapshotSpanEventArgs>()
        let mutable taggerState = 
            { DocRegions = []
              ComRegions = []
              DocSnapshot = buffer.CurrentSnapshot
              ComSnapshot = buffer.CurrentSnapshot }  
        do 
            self.Reparse()
            buffer.Changed.Add(self.BufferChanged)
        
        member __.TagsChanged = tagsChanged.Publish
        
        member __.Reparse() = 
            if buffer.CurrentSnapshot = null then () else 
            let newDocRegions = buildDoccomRegions buffer.CurrentSnapshot.Lines
            // parse the textbuffer to find any changes in the Documentation comment regions
            let newDocSnapshot, docEventSpan = 
                parse buffer taggerState.DocSnapshot taggerState.DocRegions newDocRegions
            taggerState <- { taggerState with DocSnapshot = newDocSnapshot }
            taggerState <- { taggerState with DocRegions = newDocRegions }
            if docEventSpan.IsSome then tagsChanged.Trigger(self, SnapshotSpanEventArgs docEventSpan.Value)
            let newComRegions = buildCommentRegions buffer.CurrentSnapshot.Lines
            // parse the textbuffer to find any changes in the comment regions
            let newComSnapshot, comEventSpan = 
                parse buffer taggerState.ComSnapshot taggerState.ComRegions newComRegions
            taggerState <- { taggerState with ComSnapshot = newComSnapshot }
            taggerState <- { taggerState with ComRegions = newComRegions }
            if comEventSpan.IsSome then tagsChanged.Trigger(self, SnapshotSpanEventArgs comEventSpan.Value)
        
        member __.BufferChanged(args : TextContentChangedEventArgs) = 
            if args.After <> buffer.CurrentSnapshot then ()
            else self.Reparse()
        
        interface ITagger<IOutliningRegionTag> with
            
            member __.GetTags(spans : NormalizedSnapshotSpanCollection) : IEnumerable<ITagSpan<IOutliningRegionTag>> = 
                let docTags = generateTags spans taggerState.DocSnapshot taggerState.DocRegions
                let comTags = generateTags spans taggerState.ComSnapshot taggerState.ComRegions
                Seq.append docTags comTags
            
            [<CLIEvent>]
            member __.TagsChanged : IEvent<EventHandler<SnapshotSpanEventArgs>, SnapshotSpanEventArgs> = 
                tagsChanged.Publish
    
    [<Export(typeof<ITaggerProvider>)>]
    [<TagType(typeof<IOutliningRegionTag>)>]
    [<ContentType("F#")>]
    type OutliningTaggerProvider() = 
        interface ITaggerProvider with
            member __.CreateTagger buffer =
                 buffer.Properties.GetOrCreateSingletonProperty( fun() -> OutliningTagger(buffer) :> obj :?> _ )
