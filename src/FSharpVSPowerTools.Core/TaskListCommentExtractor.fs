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


type private Pos = {
    Line: int
    Column: int
}

type private TaskListCommentPos =
    | OnelineTaskListCommentPos of string * Pos
    | MultilineTaskListCommentPos of string * Pos * Pos


[<AutoOpen>]
module private Utils =
    type Microsoft.FSharp.Compiler.SourceCodeServices.TokenInformation with
        member this.Text(lines: string[], lineNumber: int) =
            lines.[lineNumber].Substring(this.LeftColumn, this.FullMatchedLength)

    let sourceTok = SourceTokenizer([], "/tmp.fsx")

    let createNewLineTokenizer (lines: string[]) (lineNumber: int) =
        let nextLine = if lines.Length <= (lineNumber + 1) then "" else lines.[lineNumber + 1]
        sourceTok.CreateLineTokenizer(nextLine)

    let isFirstToken (tokenText: string) =
        tokenText.TrimStart([|' '; '\t'; '/'; '*'|]).TrimStart() <> ""

    let tokenizeFirstToken (tokText: string) =
        let rec tokenize (tokenizer: LineTokenizer) state =
            match tokenizer.ScanToken(state) with
            | Some tok, state when tok.Text([|tokText|], 0).TrimStart([|' '; '\t'; '/'; '*'|]) = "" -> tokenize tokenizer state
            | Some tok, _ -> (tok, tokText.Substring(tok.LeftColumn, tok.FullMatchedLength))
            | None, _ -> (Unchecked.defaultof<_>, "")
        let tokenizer = sourceTok.CreateLineTokenizer(tokText)
        tokenize tokenizer 0L

    let rec tryFindLineCommentTaskToken tasks (lines: string[], lineNumber: int, tokenizer: LineTokenizer, state) =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            let tokText = tok.Text(lines, lineNumber).ToLowerInvariant()
            let tok2, tokenizedText = tokenizeFirstToken tokText
            if isFirstToken tokenizedText && tasks |> Array.exists ((=)tokenizedText) then
                let pos = { Line = lineNumber; Column = tok.LeftColumn + tok2.LeftColumn }
                (Some (tokenizedText, pos)), state
            else
                tryFindLineCommentTaskToken tasks (lines, lineNumber, tokenizer, state)
        | _ -> None, state

    let rec tryFindMultilineCommentTaskToken tasks (lines: string[], lineNumber: int, tokenizer: LineTokenizer, state) =
        let rec scanMultilineComments (tokenizer: LineTokenizer) acc state nestLevel lineNumber =
            match tokenizer.ScanToken(state) with
            | Some tok, state ->
                match tok.CharClass with
                | TokenCharKind.Comment ->
                    match lines.[lineNumber].[tok.LeftColumn..tok.RightColumn], nestLevel with
                    | "*)", 0 -> (lineNumber, acc, tokenizer, state)
                    | "*)", _ -> scanMultilineComments tokenizer ((lineNumber, tok)::acc) state (nestLevel - 1) lineNumber
                    | "(*", _ -> scanMultilineComments tokenizer ((lineNumber, tok)::acc) state (nestLevel + 1) lineNumber
                    | _ -> scanMultilineComments tokenizer ((lineNumber, tok)::acc) state nestLevel lineNumber
                | _ ->
                    (lineNumber, acc, tokenizer, state)
            | None, state ->
                let tokenizer = createNewLineTokenizer lines lineNumber
                scanMultilineComments tokenizer acc state nestLevel (lineNumber + 1)

        let nextLineNumber, lineNumAndTokens, tokenizer, state = scanMultilineComments tokenizer [] state 0 lineNumber
        match lineNumAndTokens |> List.rev |> List.tryFind (fun (ln, tok) -> isFirstToken (tok.Text(lines, ln))) with
        | Some (lineNum, tok) ->
            let tokText = tok.Text(lines, lineNum).ToLowerInvariant()
            if tasks |> Array.exists ((=)tokText) then
                let beginPos = { Line = lineNum; Column = tok.LeftColumn }
                let endPos = { Line = nextLineNumber; Column = (lineNumAndTokens |> List.head |> snd).RightColumn }
                (Some (tokText, beginPos, endPos)), nextLineNumber, tokenizer, state
            else
                tryFindMultilineCommentTaskToken tasks (lines, nextLineNumber, tokenizer, state)
        | None ->
            None, nextLineNumber, tokenizer, state

    let rec nextTaskListCommentPos tasks (lines: string[], lineNumber: int, tokenizer: LineTokenizer, state) =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            match tok.CharClass with
            | TokenCharKind.LineComment ->
                let tokText = tok.Text(lines, lineNumber)
                if tokText |> String.forall (function '/' | '*' | ' ' | '\t' -> true | _ -> false) then
                    match tryFindLineCommentTaskToken tasks (lines, lineNumber, tokenizer, state) with
                    | Some (task, pos), state ->
                        let pos = OnelineTaskListCommentPos (task, pos)
                        Some (pos, (lines, lineNumber + 1, createNewLineTokenizer lines lineNumber, state))
                    | None, state ->
                        nextTaskListCommentPos tasks (lines, lineNumber, tokenizer, state)
                else
                    let tokenizer = createNewLineTokenizer lines lineNumber
                    nextTaskListCommentPos tasks (lines, lineNumber + 1, tokenizer, state)
            | TokenCharKind.Comment ->
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
                nextTaskListCommentPos tasks (lines, lineNumber + 1, createNewLineTokenizer lines lineNumber, state)

    let collectTaskListCommentPositions tasks lines = 
        let tokenizerState = 0L
        Seq.unfold (nextTaskListCommentPos tasks) (lines, 0, sourceTok.CreateLineTokenizer(lines.[0]), tokenizerState)


type CommentExtractor(options: CommentOption[]) =
    let priorityByComment =
        options
        |> Array.map (fun o -> (o.Comment.ToLowerInvariant(), o.Priority))
        |> Map.ofArray
    
    let toTaskListComment filePath pos (task: string, comment: string) =
        { Text = comment.Trim()
          File = filePath
          Line = pos.Line
          Column = pos.Column
          Priority = priorityByComment.[task] }

    let extractMultilineComment beginPos endPos (lines: string[]) =
        if beginPos.Line = endPos.Line then
            lines.[beginPos.Line].Substring(beginPos.Column, endPos.Column - beginPos.Column + 1)
        else
            lines.[beginPos.Line].Substring(beginPos.Column)

    let collectTaskListComments filePath lines =
        let tasks = options |> Array.map (fun option -> option.Comment.ToLowerInvariant())
        let positions = collectTaskListCommentPositions tasks lines
        positions
        |> Seq.map (function
                    | OnelineTaskListCommentPos (task, ({ Line = line; Column = col } as pos)) ->
                        let comment = lines.[line].Substring(col)
                        toTaskListComment filePath pos (task, comment)
                    | MultilineTaskListCommentPos (task, beginPos, endPos) ->
                        let comment = extractMultilineComment beginPos endPos lines
                        toTaskListComment filePath beginPos (task, comment))

    member __.GetComments(filePath: string, fileLines: string[]) =
        fileLines
        |> collectTaskListComments filePath
        |> Seq.toArray
        