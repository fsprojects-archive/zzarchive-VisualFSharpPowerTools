module FSharpVSPowerTools.Core.Tests.TaskListCommentExtractorTests

open NUnit.Framework
open FSharpVSPowerTools.TaskList
open System

let newlines = [| Environment.NewLine; "\r\n"; "\r"; "\n" |]

[<Test>]
let ``should match token case-insensitively``() = 
    let sample = "// tODo something"
    let options = [| { Comment = "TODO"; Priority = 2 } |]
    let fileName, fileLines = "File1.fs", [| sample |]
    let expected =
        [| {
            Text = "tODo something"
            File = fileName
            Line = 0
            Column = 3
            Priority = 2
        } |]
    let actual = (new CommentExtractor(options)).GetComments(fileName, fileLines)

    assertEqual expected actual

[<Test>]
let ``should only match single line comments starting with //``() = 
    let sample = "(* TODO something *)"
    let options = [| { Comment = "TODO"; Priority = 2 } |]
    let fileName, fileLines = "File1.fs", [| sample |]
    let expected = [||]
    let actual = (new CommentExtractor(options)).GetComments(fileName, fileLines)

    assertEqual expected actual

[<Test>]
let ``only the first task list comment per line is taken``() = 
    let sample = "// TODO something // TODO something else"
    let options = [| { Comment = "TODO"; Priority = 2 } |]
    let fileName, fileLines = "File1.fs", [| sample |]
    let expected =
        [| {
            Text = "TODO something // TODO something else"
            File = fileName
            Line = 0
            Column = 3
            Priority = 2
        } |]
    let actual = (new CommentExtractor(options)).GetComments(fileName, fileLines)

    assertEqual expected actual

[<Test>]
let ``task list comments only allow asterisk, backslash, or whitespace between // and token``() = 
    let sample = "//*/  /* TODO something
//+ TODO something else"
    let options = [| { Comment = "TODO"; Priority = 2 } |]
    let fileName, fileLines = "File1.fs", sample.Split(newlines, StringSplitOptions.None)
    let expected =
        [| {
            Text = "TODO something"
            File = fileName
            Line = 0
            Column = 9
            Priority = 2
        } |]
    let actual = (new CommentExtractor(options)).GetComments(fileName, fileLines)

    assertEqual expected actual

[<Test>]
let ``tokens can only be immediately followed by chars other than space that aren't alfanumeric or underscore``() = 
    let sample = "// TODO(-e_3 something
// TODO_ something else
// TODO1 something else
// TODOa something else"
    let options = [| { Comment = "TODO"; Priority = 2 } |]
    let fileName, fileLines = "File1.fs", sample.Split(newlines, StringSplitOptions.None)
    let expected =
        [| {
            Text = "TODO(-e_3 something"
            File = fileName
            Line = 0
            Column = 3
            Priority = 2
        } |]
    let actual = (new CommentExtractor(options)).GetComments(fileName, fileLines)

    assertEqual expected actual

[<Test>]
let ``comments can have any indentation and any content before them``() = 
    let sample = "   other stuff// TODO something"
    let options = [| { Comment = "TODO"; Priority = 2 } |]
    let fileName, fileLines = "File1.fs", sample.Split(newlines, StringSplitOptions.None)
    let expected =
        [| {
            Text = "TODO something"
            File = fileName
            Line = 0
            Column = 17
            Priority = 2
        } |]
    let actual = (new CommentExtractor(options)).GetComments(fileName, fileLines)

    assertEqual expected actual