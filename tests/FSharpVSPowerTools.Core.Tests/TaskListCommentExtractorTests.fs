module FSharpVSPowerTools.Core.Tests.TaskListCommentExtractorTests

open NUnit.Framework
open FSharpVSPowerTools.TaskList
open System

let newlines = [| Environment.NewLine; "\r\n"; "\r"; "\n" |]
let defaultOptions = [| { Comment = "TODO"; Priority = 2 } |]

let (=>) (options, srcFile, srcLines) (expected: (int * string * string * int * int)[]) =
    let expectedRecords =
        expected
        |> Array.map (fun (pri, text, file, line, col) ->
            {
                    Text = text
                    File = file
                    Line = line
                    Column = col
                    Priority = pri
            })
    let actual = (new CommentExtractor(options)).GetComments(srcFile, srcLines)

    assertEqual expectedRecords actual

[<Test>]
let ``should match token case-insensitively``() = 
    (defaultOptions, "File1.fs", [| "// tODo something" |])
    => [| (2, "tODo something", "File1.fs", 0, 3) |]

[<Test>]
let ``should match multiline comments``() = 
    let lines = "(* TODO x *)(* TODO y *) let x = 1
                 (*
                 TODO other things
                 *)".Split(newlines, StringSplitOptions.None)

    (defaultOptions, "File1.fs", lines)
    => [|
        (2, "TODO x", "File1.fs", 0, 3)
        (2, "TODO y", "File1.fs", 0, 15)
        (2, "TODO other things", "File1.fs", 2, 17)
       |]

[<Test>]
let ``only the first task list comment per line is taken``() = 
    (defaultOptions, "File1.fs", [| "// TODO something // TODO something else" |])
    => [| (2, "TODO something // TODO something else", "File1.fs", 0, 3) |]

[<Test>]
let ``task list comments only allow asterisk, backslash, or whitespace between // and token``() = 
    (defaultOptions, "File1.fs", [| "//*/  /* TODO stuff"; "//+ TODO something else" |])
    => [| (2, "TODO stuff", "File1.fs", 0, 9) |]

[<Test>]
let ``tokens can only be immediately followed by chars other than space that aren't alphanumeric or underscore``() = 
    (defaultOptions, "File1.fs", [| "// TODO(-e_3 something"
                                    "// TODO_ something else"
                                    "// TODO1 something else"
                                    "// TODOa something else" |])
    => [| (2, "TODO(-e_3 something", "File1.fs", 0, 3) |]

[<Test>]
let ``comments can have any indentation and any content before them``() = 
    (defaultOptions, "File1.fs", [| "   other stuff// TODO something" |])
    => [| (2, "TODO something", "File1.fs", 0, 17) |]

[<Test>]
let ``comments within strings are not considered``() = 
    let lines = "let str1 = \"// TODO something\"
                 let str2 = \"
                     // TODO other stuff
                 \"".Split(newlines, StringSplitOptions.None)

    (defaultOptions, "File1.fs", lines)
    => [||]
