namespace FSharpVSPowerTools.TaskList

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices

[<StructuralEquality; NoComparison>]
type Comment =
    {
        Text: string
        File: string
        Line: int
        Column: int
        Priority : int
    }
with
    override this.ToString() = sprintf "%A" this


[<StructuralEquality; NoComparison>]
type CommentOption = { Comment : string; Priority : int } with
    static member Default = { Comment = "TODO"; Priority = 2 }


type Pos = {
    Line: int
    Column: int
}

type CommentPos =
    | OnelineCommentPos of Pos
    | MultilineCommentPos of Pos * Pos


[<AutoOpen>]
module private Utils =
    let sourceTok = SourceTokenizer([], "/tmp.fsx")

    let createNewLineTokenizer (lines: string[]) (lineNumber: int) =
        let nextLine = if lines.Length <= (lineNumber + 1) then "" else lines.[lineNumber + 1]
        sourceTok.CreateLineTokenizer(nextLine)

    let scanMultilineComments (lines: string[], lineNumber: int, tokenizer: LineTokenizer, state) (firstToken: TokenInformation) =
        let rec scanMultilineComments' (tokenizer: LineTokenizer) acc state nestLevel lineNumber =
            match tokenizer.ScanToken(state) with
            | Some tok, state ->
                match tok.CharClass with
                | TokenCharKind.Comment ->
                    match lines.[lineNumber].[tok.LeftColumn..tok.RightColumn], nestLevel with
                    | "*)", 0 -> (lineNumber, acc, tokenizer, state)
                    | "*)", _ -> scanMultilineComments' tokenizer ((lineNumber, tok)::acc) state (nestLevel - 1) lineNumber
                    | "(*", _ -> scanMultilineComments' tokenizer ((lineNumber, tok)::acc) state (nestLevel + 1) lineNumber
                    | _ -> scanMultilineComments' tokenizer ((lineNumber, tok)::acc) state nestLevel lineNumber
                | _ ->
                    (lineNumber, acc, tokenizer, state)
            | None, state ->
                let tokenizer = createNewLineTokenizer lines lineNumber
                scanMultilineComments' tokenizer acc state nestLevel (lineNumber + 1)
        let tryFindNotBlankToken lineNumAndTokens =
            lineNumAndTokens
            |> List.tryFind (fun (lineNum, tok) ->
                not <| String.IsNullOrWhiteSpace(lines.[lineNum].Substring(tok.LeftColumn, tok.FullMatchedLength)))

        let endLineNumber, multilineCommentTokens, tokenizer, state =
            scanMultilineComments' tokenizer [] state 0 lineNumber
        let notBlankTokenOpt = tryFindNotBlankToken (List.rev multilineCommentTokens)
        let lineNumber, columnNumber =
            match notBlankTokenOpt with
            | Some (ln, tok) -> (ln, tok.LeftColumn)
            | None -> (lineNumber, (multilineCommentTokens |> List.rev |> List.head |> snd).LeftColumn)
        let beginPos = { Line = lineNumber; Column = columnNumber }
        let endPos = { Line = endLineNumber; Column = (multilineCommentTokens.Head |> snd).RightColumn }
        (beginPos, endPos, tokenizer, state)

    let rec nextCommentPos (lines: string[], lineNumber: int, tokenizer: LineTokenizer, state) =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            match tok.CharClass with
            | TokenCharKind.LineComment ->
                let lineCommentPos = OnelineCommentPos { Line = lineNumber; Column = tok.LeftColumn }
                Some (lineCommentPos, (lines, lineNumber + 1, createNewLineTokenizer lines lineNumber, state))
            | TokenCharKind.Comment ->
                let beginPos, endPos, tokenizer, state = scanMultilineComments (lines, lineNumber, tokenizer, state) tok
                let multilineCommentPos = MultilineCommentPos (beginPos, endPos)
                Some (multilineCommentPos, (lines, lineNumber + (endPos.Line - beginPos.Line), tokenizer, state))
            | _ ->
                nextCommentPos (lines, lineNumber, tokenizer, state)
        | None, state ->
            if lines.Length <= lineNumber + 1 then
                None
            else
                nextCommentPos (lines, lineNumber + 1, createNewLineTokenizer lines lineNumber, state)

    let collectCommentPositions lines = 
        let tokenizerState = 0L
        Seq.unfold nextCommentPos (lines, 0, sourceTok.CreateLineTokenizer(lines.[0]), tokenizerState)


type CommentExtractor(options: CommentOption[]) =
    let priorityByComment =
        options
        |> Array.map (fun o -> (o.Comment.ToLowerInvariant(), o.Priority))
        |> Map.ofArray
    
    let linePattern, multilinePattern =
        let tokensGroup =
            options
            |> Array.map (fun o -> o.Comment)
            |> String.concat "|"

        (sprintf @"^//[/*\s]*((%s)(?:[^a-zA-Z0-9_]+.*|(?:[^a-zA-Z0-9_\s]{0}\s.*))?)$" tokensGroup,
         sprintf @"^\s*(?:\(\*)?[\s*]*((%s)(?:[^a-zA-Z0-9_]+?.*?|(?:[^a-zA-Z0-9_\s]{0}\s.*?)))(?:\*\))?$" tokensGroup)

    let toTaskListComment filePath pattern lineNumber column comment =
        let m = Regex.Match(comment, pattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        if not m.Success then None
        else
            Some({
                    Text = m.Groups.[1].Value.TrimEnd()
                    File = filePath
                    Line = lineNumber
                    Column = column + m.Groups.[1].Index
                    Priority = priorityByComment.[m.Groups.[2].Value.ToLowerInvariant()]
                })

    let extractMultilineComment beginPos endPos (lines: string[]) =
        if beginPos.Line = endPos.Line then
            lines.[beginPos.Line].Substring(beginPos.Column, endPos.Column - beginPos.Column + 1)
        else
            lines.[beginPos.Line].Substring(beginPos.Column)

    let collectTaskListComments filePath lines =
        let commentPositions = collectCommentPositions lines
    
        commentPositions
        |> Seq.choose (function
                       | OnelineCommentPos { Line = line; Column = col } ->
                           let comment = lines.[line].Substring(col)
                           toTaskListComment filePath linePattern line col comment
                       | MultilineCommentPos (beginPos, endPos) ->
                           let comment = extractMultilineComment beginPos endPos lines
                           toTaskListComment filePath multilinePattern beginPos.Line beginPos.Column comment
        )

    member __.GetComments(filePath: string, fileLines: string[]) =
        fileLines
        |> collectTaskListComments filePath
        |> Seq.toArray
        