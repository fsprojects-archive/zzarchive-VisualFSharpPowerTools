module FSharp.Editing.Tests.TaskListCommentExtractorTests

open NUnit.Framework
open System
open FSharp.Editing
open FSharp.Editing.Features
open FSharp.Editing.Features.CommentExtractor

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
    let actual = getComments options srcFile srcLines

    assertEqual expectedRecords actual

[<Test>]
let ``should match token case-insensitively``() = 
    (defaultOptions, "File1.fs", [| "// tODo something" |])
    => [| (2, "tODo something", "File1.fs", 0, 3) |]

[<Test>]
let ``should match multi-line comments``() = 
    let lines = "(* TODO x *)(* TODO y *) let x = 1
                 (*
                 TODO other things
                 *)"
                 |> String.getLines

    (defaultOptions, "File1.fs", lines)
    => [|
        (2, "TODO x", "File1.fs", 0, 3)
        (2, "TODO y", "File1.fs", 0, 15)
        (2, "TODO other things", "File1.fs", 2, 17)
       |]

[<Test>]
let ``should match comments with no spaces``() =
    (defaultOptions, "File1.fs", [| "(*TODO something*)"; "//TODO stuff" |])
    => [| (2, "TODO something", "File1.fs", 0, 2); (2, "TODO stuff", "File1.fs", 1, 2) |]

[<Test>]
let ``should match nested comments``() =
    (defaultOptions, "File1.fs", [| "(* TODO (* (* nested *) nested *) nested *)"; "// TODO something // TODO nested" |])
    => [| (2, "TODO (* (* nested *) nested *) nested", "File1.fs", 0, 3); (2, "TODO something // TODO nested", "File1.fs", 1, 3) |]

[<Test>]
let ``line comments only allow asterisk, slash, parenthesis, or whitespace between // and token``() = 
    (defaultOptions, "File1.fs", [| "//*) // (* TODO stuff"
                                    "//+ TODO something else"
                                    "// *TODO other"
                                    "// another TODO xxx"
                                    "//　TODO full width space" |])
    => [| (2, "TODO stuff", "File1.fs", 0, 11); (2, "TODO other", "File1.fs", 2, 4); (2, "TODO full width space", "File1.fs", 4, 3) |]

[<Test>]
let ``multi-line comments only allow asterisk, slash, parenthesis, or whitespace between line head and first other token``() =
    (defaultOptions, "File1.fs", [| "(*(* // *) TODO stuff*)"
                                    "(*+ TODO something else *)"
                                    "(* *TODO other *)"
                                    "(* another TODO xxx *)"
                                    "(* //TODO more *)" |])
    => [| (2, "TODO stuff", "File1.fs", 0, 11); (2, "TODO other", "File1.fs", 2, 4); (2, "TODO more", "File1.fs", 4, 5) |]

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
let ``mix cases``() =
    (defaultOptions, "File1.fs", [| "//"
                                    "(* TODO a *)"
                                    "(*"; ""; ""; ""; "*)"
                                    "// TODO b" |])
    => [| (2, "TODO a", "File1.fs", 1, 3); (2, "TODO b", "File1.fs", 7, 3) |]

[<Test>]
let ``empty comments``() =
    (defaultOptions, "File1.fs", [| "//"
                                    "// "
                                    "(**)"
                                    "(* *)" |])
    => [||]

[<Test>]
let ``empty todo comments`` () =
    (defaultOptions, "File1.fs", [| "//todo"
                                    "// Todo"
                                    "(*ToDo*)"
                                    "(* TODO *)" |])
    => [| (2, "todo", "File1.fs", 0, 2)
          (2, "Todo", "File1.fs", 1, 3)
          (2, "ToDo", "File1.fs", 2, 2)
          (2, "TODO", "File1.fs", 3, 3) |]

[<Test>]
let ``comments within strings are not considered``() = 
    let lines = "let str1 = \"// TODO something\"
                 let str2 = \"
                     (* TODO work *)
                     // TODO other stuff
                 \""
                 |> String.getLines

    (defaultOptions, "File1.fs", lines)
    => [||]

[<Test>]
let ``no content means no results``() = 
    (defaultOptions, "File1.fs", [| |])
    => [||]
