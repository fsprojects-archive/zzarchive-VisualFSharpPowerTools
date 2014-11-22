namespace FSharpVSPowerTools.Outlining

open System
open System.Collections.Generic
open System.Linq
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools

[<AutoOpen>]
module Comments =
    
    /// <summary>
    /// Holds the data that will be used to construct the IOutliningRegionTag
    /// </summary>
    [<Struct>]
    type Region =
        /// Line number of the first line in the region
        val StartLine   : int
        /// Line number of the last line in the region
        val EndLine     : int
        /// The text displayed inline when a region is collapsed
        val Collapsed   : string
        /// The text displayed in the tooltip when a collapsed region is moused over
        val Contents    : string 
        
        new (   startline   ,   endline     , 
                collapsed   ,   contents    ) =
            {   StartLine   =   startline
                EndLine     =   endline        
                Collapsed   =   collapsed 
                Contents    =   contents    }

        new ( startline, endline ) = Region(startline,endline,"...","")
            
        override self.ToString() = 
            sprintf "[Region {ln %d - ln %d}]" self.StartLine self.EndLine
  

    /// <summary>
    /// Check if the given string matches the start of the ITextSnapshotLine's trimmed text 
    /// </summary>
    /// <param name="line"> The line being matched against </param>
    /// <param name="start"> The string being searched for</param>
    let inline lineMatch (line:ITextSnapshotLine) start : bool = 
        line.GetText().TrimStart().StartsWith(start)


    /// <summary>
    /// Check if any string in the given string list matches the start of the ITextSnapshotLine's trimmed text 
    /// </summary>
    let inline lineMatchAny (line:ITextSnapshotLine) ls : bool =
        List.fold( fun acc start -> 
                       acc || lineMatch line start ) false ls

    /// <summary>
    /// Check if the ITextSnapshotLine contains the string token
    /// </summary>
    let inline lineContains (line:ITextSnapshotLine) token : bool = 
        line.GetText().Contains(token)


    /// <summary>
    /// Uses the functions matchToken and notMatchToken to determine which
    /// lines will be collected into region blocks
    /// </summary>
    let inline takeBlocksOf 
            ( lines         : seq<ITextSnapshotLine>    ) 
            ( matchToken    : ITextSnapshotLine -> bool )
            ( notMatchToken : ITextSnapshotLine -> bool ) =         
                                
        let rec loop ( lines : seq<ITextSnapshotLine> ) =
            seq{    if lines.Count() = 0 then () else 
                    /// List of consequtive ITextSnapshotLines that begin with a token
                    let block = lines
                                // skip lines that satisfy notMatchToken
                                |> Seq.skipWhile notMatchToken
                                // take all consequtive lines that satisfy matchToken
                                |> Seq.takeWhile matchToken
                    /// sequence of elements in 'lines' following the block
                    let rest = lines|> Seq.skipWhile notMatchToken
                                    |> Seq.skip ( Seq.length block )
                    yield  block
                    yield! loop rest
                }
        loop lines |> Seq.map(List.ofSeq) |> List.ofSeq
    
    /// Token that indicates the start of a documentation comment line ' /// '
    let doccom  = "///"
    /// Token that indicates the start of a single line comment ' // '
    let comment = "//"

    let matchDoccom     ln  = lineMatch ln doccom
    let notMatchDoccom  ln  = not <| lineMatch ln doccom
        
    let matchComment    ln  = lineMatch ln comment && notMatchDoccom ln
    let notMatchComment ln  = (not <| lineMatch ln comment) || matchDoccom ln
        
    let doccomBlocks  lines = takeBlocksOf lines matchDoccom  notMatchDoccom
    let commentBlocks lines = takeBlocksOf lines matchComment notMatchComment

    let trimLine      ( ln  : ITextSnapshotLine ) = ln.GetText().Trim()
    let trimLineStart ( ln  : ITextSnapshotLine ) = ln.GetText().TrimStart([|' '|]) 
    let trimStart     ( str : string            ) = str.TrimStart([|' '|]) 


    let private removeToken (str:string) (token:string) = 
        let idx = str.IndexOf(token) 
        str.Remove( idx, token.Length  ) 

    let private removeDoccom  str = removeToken str doccom
    let private removeComment str = removeToken str comment

    let filterWhitespace (ln:ITextSnapshotLine)  = 
        String( ln.GetText().ToCharArray() |> Array.filter ( fun ch -> ch <> ' ') )


    /// <summary>
    /// <para> Construct a string that will be displayed when the mouse is over the region's collapse string </para>
    /// <para> 'removeFunc' is used cut the comment tokens out of the tooltip string </para>
    /// </summary>
    let inline getTooltipOf (lines:ITextSnapshotLine list) (removeFunc:string->string) =
        match lines with 
        | []    -> ""
        | lns   -> lns  |> List.map ( trimLineStart >> removeFunc )
                        |> String.concat "\n"

    let getDoccomTooltip  lines = getTooltipOf lines removeDoccom
    let getCommentTooltip lines = getTooltipOf lines removeComment
    
    let summary  = "<summary>"
    let ellipsis = "..."

    /// Given a list of lines that comprise a documentation comment region, determine 
    /// the string to display when the region is collapsed
    let getDoccomCollapsed ( lines: ITextSnapshotLine list ) =
        let fstLineText = lines.[0].GetText() 
        let fstFiltered = lines.[0] |> filterWhitespace |> removeDoccom
        let sumlength   = if summary.Length-1 > fstFiltered.Length 
                            then fstFiltered.Length else summary.Length-1
        // if the filtered first line is shorter than summary, use that line
        // preempts any index out of bound errors from "fstFiltered.[0..sumlength]"
        if  fstFiltered.Length < summary.Length  then fstLineText + ellipsis
        // if the first line of the doc comment is only "<summary>", take the next line
        elif fstFiltered = summary      then (lines.[1].GetText() ) + ellipsis
        // if the description follows the <summary> tag on the same line
        // drop the tag and take the rest
        elif fstFiltered.[0..sumlength] = summary then 
            let sumStart    = fstLineText.IndexOf(summary) + summary.Length
            /// whitespace preceeding the doccom token
            let indent  = fstLineText.Substring( 0, fstLineText.IndexOf(doccom) + doccom.Length )
            indent + fstLineText.Substring(sumStart, fstLineText.Length-sumStart) + ellipsis
        // if xml tags aren't used, take the first line of the doc comment
        else fstLineText + ellipsis


    /// Given a list of lines that comprise a comment region, determine the string to display when the
    /// region is collapsed
    let getCommentCollapsed ( lines: ITextSnapshotLine list ) = lines.[0].GetText() + ellipsis
     

    /// <summary>
    /// <para> Takes an ITextSnapshotline list list, where each inner list comprises the scope of a region and  </para>
    /// <para> constructs a Region using the linenumber of the first and last line of the inner list            </para>
    /// </summary>
    /// <param name="regls"></param>
    let inline parseRegionsOf 
           ( regionls     : ITextSnapshotLine list list      ) 
           ( getCollapsed : ITextSnapshotLine list -> string )
           ( getTooltip   : ITextSnapshotLine list -> string ) : Region list =
        let build (ls:ITextSnapshotLine list) =
            match ls with 
            | [] -> None
            | _  -> let fstline = ls.[0].LineNumber
                    let lstline = ls.[ls.Length-1].LineNumber
                    if fstline = lstline then None else
                    let collapsed = getCollapsed ls
                    let contents  = getTooltip   ls
                    Some <| Region( fstline, lstline, collapsed, contents  )
        match regionls with
            | [] -> []
            | _ -> regionls |> List.map build 
                            |> List.filter ( fun x -> x <> None ) 
                            |> List.map Option.get 


    let parseDoccomRegions  regionls = parseRegionsOf regionls getDoccomCollapsed  getDoccomTooltip
    let parseCommentRegions regionls = parseRegionsOf regionls getCommentCollapsed getCommentTooltip

    let buildDoccomRegions  ls = doccomBlocks  ls |> parseDoccomRegions
    let buildCommentRegions ls = commentBlocks ls |> parseCommentRegions


    let asSnapshotSpan (region:Region) (snapshot:ITextSnapshot) =
        let startLine   = snapshot.GetLineFromLineNumber(region.StartLine)
        let endLine     = if region.StartLine= region.EndLine 
                            then startLine 
                            else snapshot.GetLineFromLineNumber(region.EndLine)
        SnapshotSpan( startLine.Start, endLine.End )


    let private parse 
            ( buffer           : ITextBuffer                           )
            ( oldSnapshot      : ITextSnapshot                         ) 
            ( regions          : Region list                           )
            ( buildRegionsFunc : seq<ITextSnapshotLine> -> Region list )  =

        let newSnapshot = buffer.CurrentSnapshot
        let newRegions  = buildRegionsFunc newSnapshot.Lines
            
        let oldSpans = 
            regions |> List.map ( fun r -> 
                ( asSnapshotSpan r oldSnapshot ).TranslateTo( newSnapshot, SpanTrackingMode.EdgeExclusive).Span )

        let newSpans = 
            newRegions |> List.map ( fun r ->  ( asSnapshotSpan r newSnapshot).Span )

        let oldSpanCollection = NormalizedSpanCollection oldSpans
        let newSpanCollection = NormalizedSpanCollection newSpans
        let removed = NormalizedSpanCollection.Difference( oldSpanCollection, newSpanCollection )

        let changeStart = if removed.Count  > 0 then removed.[0].Start               else Int32.MaxValue
        let changeEnd   = if removed.Count  > 0 then removed.[removed.Count-1].End   else -1

        let changeStart'= if newSpans.Count() > 0 then 
                            Math.Min(changeStart, newSpans.ElementAt(0).Start ) else changeStart
        let changeEnd'  = if newSpans.Count() > 0 then 
                            Math.Max(changeEnd  , newSpans.ElementAt(newSpans.Count()-1).End ) else changeEnd
        let eventSpan = SnapshotSpan( oldSnapshot, Span.FromBounds(changeStart',changeEnd'))
        let triggerChange = changeStart' <= changeEnd'

        newSnapshot, newRegions, eventSpan, triggerChange


    let generateTags( spans :  NormalizedSnapshotSpanCollection )
                    ( currentSnapshot  : ITextSnapshot          )
                    ( currentRegions   : Region list            ) : IEnumerable<ITagSpan<IOutliningRegionTag>> =  
        seq {   // If there are no spans yield an empty ITagSpan
                if spans.Count = 0 then
                    yield TagSpan(SnapshotSpan(),OutliningRegionTag() :> IOutliningRegionTag) :> ITagSpan<IOutliningRegionTag>

                //let currentRegions, currentSnapshot = regions, snapshot
                /// SnapshotSpan for the entire textbuffer
                let snapEntire   = SnapshotSpan(spans.[0].Start, spans.[spans.Count-1].End).TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive )
                /// StartLineNumber for the entire textbuffer
                let startLineNum = snapEntire.Start.GetContainingLine().LineNumber
                /// EndLineNumber for the entire textbuffer
                let endLineNum   = snapEntire.End.GetContainingLine().LineNumber

                // Yield a TagSpan for every region that was parsed
                for rg in currentRegions do 
                    if rg.StartLine <= endLineNum && rg.EndLine >= startLineNum then
                        let startLine = currentSnapshot.GetLineFromLineNumber( rg.StartLine )
                        let endLine   = currentSnapshot.GetLineFromLineNumber( rg.EndLine   )

                        yield TagSpan<IOutliningRegionTag>
                                    (   SnapshotSpan(startLine.Start , endLine.End) ,
                                        OutliningRegionTag(false, false, rg.Collapsed, rg.Contents)) :> ITagSpan<IOutliningRegionTag>
            }


    type OutliningTagger( buffer : ITextBuffer ) as self =
        let mutable _docSnapshot = buffer.CurrentSnapshot
        let mutable _comSnapshot = buffer.CurrentSnapshot
        let _tagsChanged      = Event<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>() 

        let mutable _docRegions : Region list = []
        let mutable _comRegions : Region list = []
       
        do  self.Reparse()
            buffer.Changed.Add(self.BufferChanged) 

        member __.TagsChanged = _tagsChanged.Publish

        member __.DocSnapshot 
            with get() = _docSnapshot
            and  set v = _docSnapshot <- v

        member __.ComSnapshot 
            with get() = _comSnapshot
            and  set v = _comSnapshot <- v
        
        member __.DocRegions 
            with get() = _docRegions 
            and  set v = _docRegions <- v

        member __.ComRegions 
            with get() = _comRegions 
            and  set v = _comRegions <- v

        member __.Reparse() =
            if buffer.CurrentSnapshot = null then () else
            
            // parse the textbuffer to find any changes in the Documentation comment regions
            let newDocSnapshot, newDocRegions, docEventSpan, docTrigger = 
                parse buffer self.DocSnapshot self.DocRegions buildDoccomRegions

            self.DocSnapshot <- newDocSnapshot
            self.DocRegions  <- newDocRegions
            
            if docTrigger then
                _tagsChanged.Trigger(self, SnapshotSpanEventArgs docEventSpan )

            // parse the textbuffer to find any changes in the comment regions
            let newComSnapshot, newComRegions, comEventSpan, comTrigger = 
                parse buffer self.ComSnapshot self.ComRegions buildCommentRegions

            self.ComSnapshot <- newComSnapshot
            self.ComRegions  <- newComRegions

            if comTrigger then
                _tagsChanged.Trigger(self, SnapshotSpanEventArgs comEventSpan )


        member __.BufferChanged( args:TextContentChangedEventArgs  ) =
            if args.After <> buffer.CurrentSnapshot then () else
            self.Reparse()


        interface ITagger<IOutliningRegionTag> with

            member __.GetTags ( spans: NormalizedSnapshotSpanCollection): IEnumerable<ITagSpan<IOutliningRegionTag>> = 
                let docTags = generateTags spans self.DocSnapshot self.DocRegions
                let comTags = generateTags spans self.ComSnapshot self.ComRegions
                Seq.append docTags comTags
            
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
 
