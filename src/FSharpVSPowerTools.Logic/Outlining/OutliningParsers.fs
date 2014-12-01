namespace FSharpVSPowerTools.Outlining

open FParsec
open Microsoft.FSharp.Collections
open System
open System.Text

module Parsers = 
    /// <summary>
    /// Holds the data that will be used to construct the IOutliningRegionTag
    /// </summary>
    [<Struct>]
    type Region = 
        /// Line number of the first line in the region
        val StartLine : int
        /// Line number of the last line in the region
        val EndLine : int
        val StartOffset : int
        // EndOffset is only used by the star comments
        val EndOffset : int
        /// The text displayed inline when a region is collapsed
        val Collapsed : string
        /// The text displayed in the tooltip when a collapsed region is moused over
        val Contents : string
        
        new(startline, endline, startOffset, endOffset, collapsed, contents) = 
            { StartLine = startline
              EndLine = endline
              StartOffset = startOffset
              EndOffset = endOffset
              Collapsed = collapsed
              Contents = contents }
        
        override x.ToString() = sprintf "[Region {ln %d - ln %d}]" x.StartLine x.EndLine
    
    type lineNum = int64
    
    type blockLines = (lineNum * string) list
    
    /// "///"    
    let private doccom = "///"
    
    /// "//"
    let private comment = "//"
    
    let inline private trimLineStart (ln : string) = ln.TrimStart([| ' ' |])
    
    let inline private removeToken (str : string) (token : string) = 
        let idx = str.IndexOf(token)
        str.Remove(idx, token.Length)
    
    let private removeDoccom str = removeToken str doccom
    let private removeComment str = removeToken str comment
    
    let private filterWhitespace (ln : string) = 
        ln.ToCharArray()
        |> Array.filter (fun ch -> ch <> ' ')
        |> String.Concat
    
    /// <summary>
    /// <para> Construct a string that will be displayed when the mouse is over the region's collapse string </para>
    /// <para> 'removeFunc' is used cut the comment tokens out of the tooltip string </para>
    /// </summary>
    let inline getTooltipOf (lines : string list) (removeFunc : string -> string) = 
        match lines with
        | [] -> ""
        | lns -> 
            lns
            |> List.map (trimLineStart >> removeFunc)
            |> String.concat "\n"
    
    let getDocTooltip lines = getTooltipOf lines removeDoccom
    let getComTooltip lines = getTooltipOf lines removeComment
    let private summary = "<summary>"
    let private ellipsis = "..."
    
    /// Given a list of lines that comprise a documentation comment region, determine 
    /// the string to display when the region is collapsed
    let getDocCollapsed (lines : string list) : string = 
        if lines = [] then ellipsis
        else 
            let fstLineText = lines.[0]
            match lines.[0]
                  |> filterWhitespace
                  |> removeDoccom with
            // if the filtered first line is shorter than summary, use that line
            | ffln when ffln.Length < summary.Length -> fstLineText + ellipsis |> trimLineStart
            // if the first line of the doc comment is only "<summary>", take the next line
            | ffln when ffln = summary && lines.Length > 1 -> (lines.[1]) + ellipsis |> trimLineStart
            // if the description follows the <summary> tag on the same line drop the tag and take the rest
            | ffln when ffln.[0..summary.Length - 1] = summary -> 
                let sumStart = fstLineText.IndexOf(summary) + summary.Length
                doccom + fstLineText.[sumStart..fstLineText.Length - 1] + ellipsis
            // if xml tags aren't used, take the first line of the doc comment
            | _ -> fstLineText + ellipsis |> trimLineStart
    
    /// Given a list of lines that comprise a comment region, determine the string to display when the
    /// region is collapsed
    let getComCollapsed (lines : string list) : string = 
        match lines with
        | [] -> ellipsis
        | _ -> lines.[0] + ellipsis |> trimLineStart
    
    let inline private getStarCollapsed _ = "(*...*)"
    
    let stripLineNums (lines : blockLines) = 
        let folder (text : string list) ((_, ln) : lineNum * string) = ln :: text
        lines
        |> List.rev
        |> List.fold folder []
    
    let inline constructCommentRegion (getCollapsed : string list -> string) (getTooltip : string list -> string) 
               (token : string) (ls : blockLines) = 
        match ls with
        | [] -> None
        | _ -> 
            let (fstLine, fstText) = ls.[0]
            let (lstLine, _) = ls.[ls.Length - 1]
            if fstLine = lstLine then None
            else 
                let text = stripLineNums ls
                let collapsed = getCollapsed text
                let contents = getTooltip text
                Some 
                <| Region(int fstLine, int lstLine, fstText.Length - fstText.IndexOf(token), -1, collapsed, contents)
    
    let private reduceStrList (lines : string list) = 
        let folder (sb : StringBuilder) (ln : string) = sb.Append(ln + "\n")
        let sb = lines |> List.fold folder (StringBuilder())
        sb.ToString()
    
    let constructStarRegion (ls : blockLines) = 
        match ls with
        | [] -> None
        | _ -> 
            let (fstLine, fstText) = ls.[0]
            let (lstLine, lstText) = ls.[ls.Length - 1]
            if fstLine = lstLine then None
            else 
                let text = stripLineNums ls
                let collapsed = getStarCollapsed text
                let contents = reduceStrList text
                
                let sOff = 
                    if fstText.Length = 0 then fstText.Length + 4
                    else fstText.Length + 3
                Some <| Region(int fstLine, int lstLine, sOff, // add three to backtrack to the index before the (*
                               lstText.Length + 2, collapsed, contents)
    
    let private constructStringRegion (ls : blockLines) = 
        match ls with
        | [] -> None
        | _ -> 
            let (fstLine, _) = ls.[0]
            let (lstLine, _) = ls.[ls.Length - 1]
            if fstLine = lstLine then None
            else 
                let collapsed = ""
                let contents = ""
                // the offset of these regions will not be used so we default to 0
                Some <| Region(int fstLine, int lstLine, 0, 0, collapsed, contents)
    
    let buildDocRegion = constructCommentRegion getDocCollapsed getDocTooltip doccom
    let buildComRegion = constructCommentRegion getComCollapsed getComTooltip comment
    let buildStarRegion = constructStarRegion
    
    type private UserState = unit
    
    type private Parser<'t> = Parser<'t, UserState>
    
    let private pullResult (res : ParserResult<'t, unit>) = 
        let _, result = 
            match res with
            | Failure(errMsg, _, _) -> errMsg, None
            | Success(result, _, _) -> "Success", Some result
        match result with
        | None -> failwithf "\n\n >>> Parser did not return a result <<<\n\n%A" res
        | Some r -> r
    
    let private getLineNum : Parser<_> = getPosition |>> fun x -> x.Line - 1L
    let private skipUntil token : Parser<_> = skipManyTill skipAnyChar token
    let private skipToEOF : Parser<_> = skipUntil eof
    let private takeUntil token : Parser<_> = manyCharsTill anyChar token
    let private takeTillLineEnd : Parser<_> = takeUntil newline
    let private attemptMany psr = many (attempt psr)
    let private attemptMany1 psr = many1 (attempt psr)
    let private parseNextMatch psr = (skipManyTill skipAnyChar (lookAhead psr) >>. psr)
    let private parseString str psr = run psr str |> pullResult
    let findInString str psr = attemptMany (parseNextMatch psr) .>> skipToEOF |> parseString str
    let findInStringAsync str psr = async { return findInString str psr }
    
    /// Parse consecutive spaces as a string
    let private getSpaces : Parser<_> = many <| pchar ' ' |>> (fun chls -> String.Concat(Array.ofList chls))
    
    let private dblQuote : Parser<_> = pchar '\"'
    let private notDblQuote : Parser<_> = noneOf [ '"' ]
    let private slash : Parser<_> = pchar '/'
    let private equals : Parser<_> = pchar '='
    let private triSlash : Parser<_> = pstring "///"
    let private dblSlash : Parser<_> = pstring "//"
    let private comOpen : Parser<_> = pstring "(*"
    let private comClose : Parser<_> = pstring "*)"
    let private quoteTri : Parser<_> = pstring "\"\"\""
    let private strToken = dblQuote .>> notFollowedBy dblQuote .>> notFollowedBy dblQuote
    let inline private flattenCom (((a, b), c) : (string * string) * string) = String.Concat [ a; b; c ]
    let inline private appendElm (lns, lstln) = List.append lns [ lstln ]
    let inline private consDbl ((l1, l2), ls) = l1 :: l2 :: ls
    let private stringStart = skipMany1Till (noneOf [ '/' ]) (lookAhead equals) >>. skipChar '=' >>. spaces
    
    // a multiline string must end on a different line than it starts
    let multiString = 
        let multilns = attemptMany1 (getLineNum .>>. (manyCharsTill notDblQuote) newline)
        let lstln = getLineNum .>>. manyCharsTill anyChar (lookAhead dblQuote)
        let multiContent = (multilns .>>. lstln |>> appendElm) |> between strToken strToken
        (stringStart >>. multiContent) |>> constructStringRegion
    
    let inline insertEmptyString _ = String.Empty
    
    let skipMultiString = 
        let multilns = attemptMany1 (getLineNum .>>. (((skipManyTill notDblQuote) newline) |>> insertEmptyString))
        let lstln = getLineNum .>>. (skipManyTill skipAnyChar (lookAhead dblQuote) |>> insertEmptyString)
        let multiContent = (multilns .>>. lstln |>> appendElm) |> between strToken strToken
        stringStart >>. multiContent |>> constructStringRegion
    
    // a multiline triple quote string must end on a different line than it starts
    let triString = 
        let lstln = 
            (getLineNum .>>. (newline |>> fun _ -> "")) <|> (getLineNum .>>. manyCharsTill anyChar (lookAhead quoteTri))
        let trilns = 
            attemptMany1 
                (getLineNum 
                 .>>. (manyCharsTill (anyChar .>> notFollowedBy quoteTri)) (newline .>> notFollowedBy quoteTri))
        let triContent = (trilns .>>. lstln |>> appendElm) |> between quoteTri quoteTri
        stringStart >>. triContent |>> constructStringRegion
    
    let skipTriString = 
        let lstln = 
            (getLineNum .>>. (newline |>> insertEmptyString)) 
            <|> (getLineNum .>>. ((skipManyTill skipAnyChar (lookAhead quoteTri)) |>> insertEmptyString))
        let trilns = 
            attemptMany1 
                (getLineNum 
                 .>>. ((skipManyTill (skipAnyChar .>> notFollowedBy quoteTri)) (newline .>> notFollowedBy quoteTri) 
                       |>> insertEmptyString))
        let triContent = (trilns .>>. lstln |>> appendElm) |> between quoteTri quoteTri
        stringStart >>. triContent |>> constructStringRegion
    
    let private docLine = getLineNum .>>. (getSpaces .>>. triSlash .>>. takeTillLineEnd |>> flattenCom)
    let private comLine = 
        getLineNum .>>. (getSpaces .>>. (dblSlash .>> notFollowedBy slash) .>>. takeTillLineEnd |>> flattenCom)
    // comment blocks must have at least 2 lines
    let docBlock = (docLine .>>. docLine .>>. attemptMany docLine) |>> consDbl |>> buildDocRegion
    let comBlock = (comLine .>>. comLine .>>. attemptMany comLine |>> consDbl) |>> buildComRegion
    
    let starBlock = 
        let comOpen' = 
            (notDblQuote >>. comOpen .>> notDblQuote) <|> (dblQuote >>. comOpen .>> notDblQuote) 
            <|> (notDblQuote >>. comOpen .>> dblQuote)
        let starLines = attemptMany1 (getLineNum .>>. (manyCharsTill (anyChar .>> notFollowedBy comClose) newline))
        let lastLine = getLineNum .>>. (manyCharsTill anyChar (lookAhead comClose))
        let starContent = (starLines .>>. lastLine |>> appendElm) |> between comOpen' comClose
        starContent |>> buildStarRegion
