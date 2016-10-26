namespace FSharp.Editing.Features

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing

[<NoComparison>]
type Comment =
    { Text: string
      File: string
      Line: int
      Column: int
      Priority : int }
    override x.ToString() = sprintf "%A" x

[<NoComparison>]
type CommentOption = 
    { Comment: string
      Priority: int }
    static member Default = { Comment = "TODO"; Priority = 2 }

type private TaskListCommentPos =
    | OnelineTaskListCommentPos of string * Point<FCS>
    | MultilineTaskListCommentPos of string * Point<FCS> * Point<FCS>

[<AutoOpen>]
module private Utils =
    type FSharpTokenInfo with
        member x.Text(lines: string[], lineNumber: int) =
            lines.[lineNumber].Substring(x.LeftColumn, x.FullMatchedLength)

    let sourceTok = SourceTokenizer([], Some "/tmp.fsx")

    let createNewLineTokenizer (lines: string[]) (lineNumber: int) =
        let nextLine =
            if lines.Length <= (lineNumber + 1) then String.Empty
            else lines.[lineNumber + 1]
        sourceTok.CreateLineTokenizer(nextLine)

    let trimChars = [| ' '; '\u3000'; '\t'; '/'; '('; ')'; '*' |]
    let isFirstToken (tokenText: string) =
        tokenText.TrimStart(trimChars) <> String.Empty

    let tryTokenizeFirstToken (tokText: string) =
        let rec tryTokenize (tokenizer: FSharpLineTokenizer) state =
            match tokenizer.ScanToken(state) with
            | Some tok, state when not <| isFirstToken (tok.Text([| tokText |], 0)) -> tryTokenize tokenizer state
            | Some tok, _ -> Some (tok, tokText.Substring(tok.LeftColumn, tok.FullMatchedLength))
            | None, _ -> None
        let tokenizer = sourceTok.CreateLineTokenizer(tokText)
        tryTokenize tokenizer 0L

    let tryFindLineCommentTaskToken tasks (lines: string[], lineNumber: int, tokenizer: FSharpLineTokenizer, state) =
        let rec tryFindLineCommentTaskToken' state =
            match tokenizer.ScanToken(state) with
            | Some tok, state ->
                let tokText = tok.Text(lines, lineNumber)
                let trimmedTokText = tokText.Trim(trimChars).ToLowerInvariant()
                match tryTokenizeFirstToken trimmedTokText with
                | Some (tok2, tokenizedText) ->
                    if isFirstToken tokenizedText && tasks |> Array.exists ((=) tokenizedText) then
                        let pos = Point.make lineNumber (tok.LeftColumn + tok2.LeftColumn + (tokText.Length - trimmedTokText.Length))
                        (Some (tokenizedText, pos), state)
                    elif tok2.CharClass = FSharpTokenCharKind.Identifier then
                        None, state
                    else
                        tryFindLineCommentTaskToken' state
                | None -> tryFindLineCommentTaskToken' state
            | _ -> None, state
        tryFindLineCommentTaskToken' state |> fst

    let rec tryFindMultilineCommentTaskToken tasks (lines: string[], lineNumber: int, tokenizer: FSharpLineTokenizer, state) =
        let rec scanMultilineComments (tokenizer: FSharpLineTokenizer) acc state nestLevel lineNumber =
            match tokenizer.ScanToken(state) with
            | Some tok, state ->
                match tok.CharClass with
                | FSharpTokenCharKind.Comment ->
                    match lines.[lineNumber].[tok.LeftColumn..tok.RightColumn], nestLevel with
                    | "*)", 0 -> (lineNumber, acc, tokenizer, state)
                    | "*)", _ -> scanMultilineComments tokenizer ((lineNumber, tok) :: acc) state (nestLevel - 1) lineNumber
                    | "(*", _ -> scanMultilineComments tokenizer ((lineNumber, tok) :: acc) state (nestLevel + 1) lineNumber
                    | _ -> scanMultilineComments tokenizer ((lineNumber, tok) :: acc) state nestLevel lineNumber
                | _ ->
                    (lineNumber, acc, tokenizer, state)
            | None, state ->
                if lines.Length <= (lineNumber + 1) then
                    (lineNumber, acc, tokenizer, state)
                else
                    let tokenizer = sourceTok.CreateLineTokenizer(lines.[lineNumber + 1])
                    (lineNumber + 1, acc, tokenizer, state)

        let nextLineNumber, lineNumAndTokens, tokenizer, state = scanMultilineComments tokenizer [] state 0 lineNumber
        match lineNumAndTokens |> List.rev |> List.tryFind (fun (ln, tok) -> isFirstToken (tok.Text(lines, ln))) with
        | Some (lineNum, tok) ->
            let tokText = tok.Text(lines, lineNum)
            let trimmedTokText = tokText.TrimStart(trimChars).ToLowerInvariant()
            if tasks |> Array.exists ((=) trimmedTokText) then
                let beginPos = Point.make lineNum (tok.LeftColumn + (tokText.Length - trimmedTokText.Length))
                let endPos = Point.make nextLineNumber ((lineNumAndTokens |> List.head |> snd).RightColumn)
                (Some (trimmedTokText, beginPos, endPos)), nextLineNumber, tokenizer, state
            else
                tryFindMultilineCommentTaskToken tasks (lines, nextLineNumber, tokenizer, state)
        | None ->
            None, nextLineNumber, tokenizer, state

    let rec nextTaskListCommentPos tasks (lines: string[], lineNumber: int, tokenizer: FSharpLineTokenizer, firstState) =
        match tokenizer.ScanToken(firstState) with
        | Some tok, state ->
            match tok.CharClass with
            | FSharpTokenCharKind.LineComment ->
                let tokText = tok.Text(lines, lineNumber)
                if tokText |> String.forall (fun c -> trimChars |> Array.exists ((=)c)) then
                    match tryFindLineCommentTaskToken tasks (lines, lineNumber, tokenizer, state) with
                    | Some (task, pos) ->
                        let pos = OnelineTaskListCommentPos (task, pos)
                        Some (pos, (lines, lineNumber + 1, createNewLineTokenizer lines lineNumber, firstState))
                    | None ->
                        nextTaskListCommentPos tasks (lines, lineNumber + 1, createNewLineTokenizer lines lineNumber, firstState)
                else
                    let tokenizer = createNewLineTokenizer lines lineNumber
                    nextTaskListCommentPos tasks (lines, lineNumber + 1, tokenizer, firstState)
            | FSharpTokenCharKind.Comment ->
                match tryFindMultilineCommentTaskToken tasks (lines, lineNumber, tokenizer, state) with
                | Some (task, beginPos, endPos), lineNumber, tokenizer, state ->
                    let pos = MultilineTaskListCommentPos (task, beginPos, endPos)
                    Some (pos, (lines, lineNumber, tokenizer, state))
                | None, lineNumber, tokenizer, state ->
                    nextTaskListCommentPos tasks (lines, lineNumber, tokenizer, state)
            | _ ->
                nextTaskListCommentPos tasks (lines, lineNumber, tokenizer, state)
        | None, state ->
            if lines.Length <= lineNumber + 1 then
                None
            else
                nextTaskListCommentPos tasks (lines, lineNumber + 1, sourceTok.CreateLineTokenizer(lines.[lineNumber + 1]), state)

    let collectTaskListCommentPositions tasks lines = 
        let tokenizerState = 0L
        Seq.unfold (nextTaskListCommentPos tasks) (lines, 0, sourceTok.CreateLineTokenizer(lines.[0]), tokenizerState)

module CommentExtractor =
    let private getPriorityByTask options =
        options
        |> Array.map (fun o -> (o.Comment.ToLowerInvariant(), o.Priority))
        |> Map.ofArray

    let inline private toTaskListComment filePath (pos: Point<FCS>) comment priority =
        { Text = comment
          File = filePath
          Line = pos.Line
          Column = pos.Column
          Priority = priority }

    let private extractMultilineComment (beginPos: Point<FCS>) (endPos: Point<FCS>) (lines: string[]) =
        if beginPos.Line = endPos.Line then
            lines.[beginPos.Line].Substring(beginPos.Column, endPos.Column - beginPos.Column + 1)
        else
            lines.[beginPos.Line].Substring(beginPos.Column)

    let private collectTaskListComments options filePath lines =
        match lines with
        | [| |] ->
            Seq.empty
        | _ ->
            let priorityByTask = getPriorityByTask options
            let tasks = options |> Array.map (fun option -> option.Comment.ToLowerInvariant())
            let positions = collectTaskListCommentPositions tasks lines
            positions
            |> Seq.map (function
                        | OnelineTaskListCommentPos (task, ({ Line = line; Column = col } as pos)) ->
                            let comment = lines.[line].Substring(col).Trim()
                            toTaskListComment filePath pos comment priorityByTask.[task]
                        | MultilineTaskListCommentPos (task, beginPos, endPos) ->
                            let comment = (extractMultilineComment beginPos endPos lines).Trim()
                            toTaskListComment filePath beginPos comment priorityByTask.[task])

    let getComments options filePath fileLines =
        fileLines
        |> collectTaskListComments options filePath
        |> Seq.toArray
