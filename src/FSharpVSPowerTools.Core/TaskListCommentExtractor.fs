namespace FSharpVSPowerTools.TaskList

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

[<StructuralEquality; NoComparison>]
type CommentOption = { Comment : string; Priority : int } with
    static member Default = { Comment = "TODO"; Priority = 2 }

[<AutoOpen>]
module private Utils =
    let sourceTok = SourceTokenizer([], "/tmp.fsx")

    let rec collectCommentTokens (tokenizer: LineTokenizer) state acc =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            if tok.CharClass = TokenCharKind.LineComment then
                collectCommentTokens tokenizer state (tok :: acc)
            else
                collectCommentTokens tokenizer state acc
        | None, state -> (state, List.rev acc)
    
    let rec collectCommentTokensPerLine state lineNumber lines acc = 
        match lines with
        | line :: lines ->
            let tokenizer = sourceTok.CreateLineTokenizer(line)
            let (state, tokens) = collectCommentTokens tokenizer state []
            
            if tokens.IsEmpty then
                collectCommentTokensPerLine state (lineNumber + 1) lines acc
            else
                collectCommentTokensPerLine state (lineNumber + 1) lines ((lineNumber, tokens) :: acc)
        | [] -> List.rev acc

    
type CommentExtractor(options: CommentOption[]) =
    let priorityByComment =
        options
        |> Array.map (fun o -> (o.Comment.ToLowerInvariant(), o.Priority))
        |> Map.ofArray
    
    let pattern =
        options
        |> Array.map (fun o -> o.Comment)
        |> String.concat "|"
        |> sprintf @"^//[/*\s]*((%s)(?:[^a-zA-Z0-9_]+.*|(?:[^a-zA-Z0-9_\s]{0}\s.*))?)$"

    let toTaskListComment filePath lineNumber column comment =
        let m = Regex.Match(comment, pattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        if not m.Success then None
        else
            Some({
                    Text = m.Groups.[1].Value
                    File = filePath
                    Line = lineNumber
                    Column = column + m.Groups.[1].Index
                    Priority = priorityByComment.[m.Groups.[2].Value.ToLowerInvariant()]
                })

    let collectTaskListComments filePath lines =
        let commentsByLine =
            collectCommentTokensPerLine 0L 0 lines []
            |> List.map (fun (lineNumber, tokens) ->
                             let leftIndex = tokens.[0].LeftColumn
                             let rightIndex = tokens.[tokens.Length - 1].RightColumn
                             
                             (lineNumber,
                              leftIndex,
                              lines.[lineNumber].[leftIndex..rightIndex]))

        commentsByLine
        |> List.map (fun (lineNumber, column, comment) -> toTaskListComment filePath lineNumber column comment)
        |> List.filter Option.isSome
        |> List.map Option.get

    member __.GetComments(filePath: string, fileLines: string[]) =
        fileLines
        |> List.ofArray
        |> collectTaskListComments filePath
        |> Array.ofList
        