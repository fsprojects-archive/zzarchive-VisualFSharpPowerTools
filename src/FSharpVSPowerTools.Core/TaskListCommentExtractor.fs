namespace FSharpVSPowerTools.TaskList

open System.Text.RegularExpressions

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

type CommentExtractor(options: CommentOption[]) =
    let priorityByComment =
        options
        |> Array.map (fun o -> (o.Comment.ToLowerInvariant(), o.Priority))
        |> Map.ofArray
    
    let pattern =
        options
        |> Array.map (fun o -> o.Comment)
        |> String.concat "|"
        |> sprintf @"\s*//[/*\s]*((%s)(?:[^a-zA-Z0-9_]+.*|(?:[^a-zA-Z0-9_\s]{0}\s.*))?)$"

    let extractComment filePath lineNumber line =
        let m = Regex.Match(line, pattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        if not m.Success then None
        else
            Some({
                    Text = m.Groups.[1].Value
                    File = filePath
                    Line = lineNumber
                    Column = m.Groups.[1].Index
                    Priority = priorityByComment.[m.Groups.[2].Value.ToLowerInvariant()]
                })

    member __.GetComments(filePath: string, fileLines: string[]) =
        let extractFromLineInFile = extractComment filePath

        fileLines
        |> Array.mapi extractFromLineInFile
        |> Array.filter Option.isSome
        |> Array.map Option.get