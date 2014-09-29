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
with
    override this.ToString() = sprintf "%A" this


[<StructuralEquality; NoComparison>]
type CommentOption = { Comment : string; Priority : int } with
    static member Default = { Comment = "TODO"; Priority = 2 }


[<AutoOpen>]
module private Utils =
    let sourceTok = SourceTokenizer([], "/tmp.fsx")

    let rec collectCommentTokens (line: string) (tokenizer: LineTokenizer) state (acc: TokenInformation list list) prevTokenIsEndComm =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            match tok.CharClass with
            | TokenCharKind.Comment ->
                let currTokStr = line.[tok.LeftColumn..tok.RightColumn]
                if prevTokenIsEndComm then
                    if currTokStr = "(*" then
                        collectCommentTokens line tokenizer state ([ tok ] :: acc) false
                    else
                        collectCommentTokens line tokenizer state ((tok :: acc.Head) :: acc.Tail) (currTokStr = "*)")
                else
                    match acc with
                    | hd :: tl -> collectCommentTokens line tokenizer state ((tok :: hd) :: tl) (currTokStr = "*)")
                    | [] -> collectCommentTokens line tokenizer state ([ tok ] :: acc) (currTokStr = "*)")
            | _ ->
                collectCommentTokens line tokenizer state ([] :: acc) false

        | None, state ->
            let acc =
                acc
                |> List.filter (not << List.isEmpty)
                |> List.map List.rev
                |> List.rev
            (state, acc)

    let rec collectLineCommentTokens (tokenizer: LineTokenizer) state acc =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            match tok.CharClass with
            | TokenCharKind.LineComment ->
                collectLineCommentTokens tokenizer state (tok :: acc)
            | _ ->
                collectLineCommentTokens tokenizer state acc
        | None, state -> (state, List.rev acc)
    
    let rec collectCommentTokensPerLine lineCommAcc multilineCommAcc state lineNumber lines = 
        match lines with
        | line :: lines ->
            let tokenizer = sourceTok.CreateLineTokenizer(line)
            let (_, lineTokens) = collectLineCommentTokens tokenizer state []
            let tokenizer = sourceTok.CreateLineTokenizer(line)
            let (state, multilineTokens) = collectCommentTokens line tokenizer state [] false

            let lineCommAcc =
                match lineTokens with
                | [] -> lineCommAcc
                | _ -> (lineNumber, lineTokens) :: lineCommAcc

            let multilineCommAcc =
                match multilineTokens with
                | [] -> multilineCommAcc
                | _ -> (lineNumber, multilineTokens) :: multilineCommAcc

            collectCommentTokensPerLine lineCommAcc multilineCommAcc state (lineNumber + 1) lines
        | [] -> (List.rev lineCommAcc, List.rev multilineCommAcc)


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

    let collectTaskListComments filePath lines =
        let lineTokens, multilineTokens = collectCommentTokensPerLine [] [] 0L 0 lines
    
        let lineCommentsByLine =
            lineTokens
            |> List.map (fun (lineNumber, tokens) ->
                             let leftIndex = tokens.[0].LeftColumn
                             let rightIndex = tokens.[tokens.Length - 1].RightColumn
                             
                             (lineNumber,
                              leftIndex,
                              lines.[lineNumber].[leftIndex..rightIndex]))
            |> List.choose (fun (lineNumber, column, comment) -> toTaskListComment filePath linePattern lineNumber column comment)
    
        let multilineCommentsByLine =
            multilineTokens
            |> List.collect (fun (lineNumber, tokenLists) ->
                                 tokenLists
                                 |> List.map (fun tokens ->
                                                  let leftIndex = tokens.[0].LeftColumn
                                                  let rightIndex = tokens.[tokens.Length - 1].RightColumn
                                                  
                                                  (lineNumber,
                                                   leftIndex,
                                                   lines.[lineNumber].[leftIndex..rightIndex]))
                                 |> List.choose (fun (lineNumber, column, comment) -> toTaskListComment filePath multilinePattern lineNumber column comment))
    
        lineCommentsByLine @ multilineCommentsByLine

    member __.GetComments(filePath: string, fileLines: string[]) =
        fileLines
        |> List.ofArray
        |> collectTaskListComments filePath
        |> Array.ofList
        