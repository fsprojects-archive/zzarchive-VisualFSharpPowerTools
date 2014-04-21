#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.RecordStubGeneratorTests
#endif

open NUnit.Framework
open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.CompilerBinding
open FSharpVSPowerTools

let args = 
  [|"--noframework"; "--debug-"; "--optimize-"; "--tailcalls-";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"|]

let framework = FSharpTargetFramework.NET_4_5
let languageService = new FSharp.CompilerBinding.LanguageService(fun _ -> ())

// [x] Get the syntax construct that you're interested in
// [x] Get the position P where to insert the generated code
// [x] Get the symbol S on which the caret is
// [x] Get symbol S use to get its type metadata (fields, in particular)
// [x] Generate code at position P
// [ ] Handle case when some fields are already written
// [ ] Handle case when all fields are written

#if INTERACTIVE
#load "../../src/FSharpVSPowerTools.Core/RecordStubGenerator.fs"
#endif

open FSharpVSPowerTools.Core
open FSharpVSPowerTools.Core.CodeGeneration
open Microsoft.FSharp.Compiler.Ast

let srcToLineMap (src: string) =
    src.Split('\n')
    |> Array.mapi (fun i line -> i, line)
    |> Map.ofArray

let splitLines (src: string) = src.Split([|"\r\n"; "\n"|], StringSplitOptions.None)

let getSymbolAtPoint (src: string) (pos: pos) =
    let lines = srcToLineMap src
    let lineIdx0 = pos.Line - 1 
    let line = lines.[lineIdx0]
    Lexer.getSymbol src lineIdx0 pos.Column line args Lexer.queryLexState

let getSymbolUseAtPoint (src: string) (pos: pos) =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]
    let lines = srcToLineMap src
    let lineIdx0 = pos.Line - 1
    let line = lines.[lineIdx0]

    match getSymbolAtPoint src pos with
    | Some symbol ->
        let parseAndCheckResults =
            languageService.ParseAndCheckFileInProject(
                projFileName, fileName, src, files, args, framework, AllowStaleResults.MatchingSource)
            |> Async.RunSynchronously

        // NOTE: we must set <colAtEndOfNames> = symbol.RightColumn
        // and not <pos.Column>, otherwise GetSymbolUseAtLocation won't find it
        parseAndCheckResults.GetSymbolUseAtLocation(pos.Line, symbol.RightColumn, line, [symbol.Text])
        |> Async.RunSynchronously
        |> Option.map (fun s -> s, parseAndCheckResults)
    | _ -> None

let tryFindRecordBindingExpTree (src: string) pos =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]

    let parseResults =
        languageService.ParseFileInProject(projFileName, fileName, src, files, args, framework)
        |> Async.RunSynchronously

    parseResults.ParseTree
    |> Option.bind (RecordStubGenerator.tryFindRecordBinding pos)

let tokenizeLine (src: string) (lineIdx1: int) =
    let lines =
        src.Split('\n')
        |> Array.mapi (fun i line -> i, line)
        |> Map.ofArray

    let lineIdx0 = lineIdx1 - 1 
    let line = lines.[lineIdx0]
    Lexer.tokenizeLine src args (lineIdx0) line Lexer.queryLexState

//type VirtualTextEditor(src: string) =
//    let splitLines (s: string) =
//        s.Split([|"\r\n"; "\n"|], StringSplitOptions.None)
//
//    let mutable lines = new ResizeArray<_>(splitLines src)
//        
//    member this.Line i = lines.[i]
////    member this.InsertAt(lineIdx, col, str) =
////        let line = lines.[lineIdx]
////        assert (col >= 0 && col <= line.Length)
////        let before, after = line.Substring(0, col), line.Substring(col)
////        let lines = splitLines str
////        match lines with
////        | [||] -> ()
////        | [|s0|] -> lines.[lineIdx] <- before + s0
////                    lines.Insert(lineIdx + 1, after)
////        | [|s0; s1|] -> lines.[lineIdx] <- before + s0
////                        lines.Insert(lineIdx + 1, after)
////        | _ -> ()
//
//    member this.GetText() =
//        lines
//        |> Seq.fold (fun state line -> state + Environment.NewLine + line) ""


let getRecordBindingData (src: string) pos =
    let recordBindingExpTree = tryFindRecordBindingExpTree src pos
    let tokens = tokenizeLine src pos.Line
    let column = pos.Column
    let line0 = pos.Line - 1
    let endPosOfLBrace =
        tokens |> List.tryPick (fun (t: TokenInformation) ->
                    if t.CharClass = TokenCharKind.Delimiter &&
                       t.LeftColumn >= column &&
                       t.TokenName = "LBRACE" then
                        Some (Pos.fromZ line0 (t.RightColumn + 1))
                    else None)

    match recordBindingExpTree with
    | Some tree -> Some (tree, endPosOfLBrace)
    | None -> None

let getRecordDefinitionFromPoint (pos: pos) (src: string) =
    maybe {
        let! recordBindingData, endPosOfLBrace' = getRecordBindingData src pos
        let! endPosOfLBrace = endPosOfLBrace'
        let! symbolUse, _ = getSymbolUseAtPoint src pos

        if symbolUse.Symbol :? FSharpEntity then
            let entity = symbolUse.Symbol :?> FSharpEntity
            if entity.IsFSharpRecord then
                return! Some (recordBindingData, symbolUse.DisplayContext, entity, endPosOfLBrace)
            else
                return! None
        else
            return! None
    }

let insertStubFromPos caretPos src =
    let recordDefnFromPt = getRecordDefinitionFromPoint caretPos src
    match recordDefnFromPt with
    | None -> src
    | Some(_, context, entity, insertLocation) ->
        let column = insertLocation.Column
        let fieldValue = "failwith \"\""
        let stub = RecordStubGenerator.formatRecord column 4 fieldValue context entity
        let srcLines = splitLines src
        let line0 = caretPos.Line - 1
        let curLine = srcLines.[line0]
        let before, after = curLine.Substring(0, column), curLine.Substring(column)

        srcLines.[line0] <- before + stub + after

        if srcLines.Length = 0 then
            "" 
        else
            srcLines
            |> Array.reduce (fun line1 line2 -> line1 + "\n" + line2)

let assertSrcAreEqual expectedSrc actualSrc =
    assertEqual (splitLines expectedSrc) (splitLines actualSrc)

[<Test>]
let ``basic single-field record stub generation`` () =
    """
type MyRecord = { Field1: int }
let x: MyRecord = { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int }
let x: MyRecord = { Field1 = failwith "" }"""

[<Test>]
let ``basic multiple-field record stub generation`` () =
    """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = { Field1 = failwith ""
                    Field2 = failwith ""
                    Field3 = failwith "" }"""

[<Test>]
let ``single-field record stub generation in the middle of the file`` () =
    """
module A
type MyRecord = { Field1: int }
let x: MyRecord = { }
do ()"""
    |> insertStubFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """
module A
type MyRecord = { Field1: int }
let x: MyRecord = { Field1 = failwith "" }
do ()"""

[<Test>]
let ``multiple-field record stub generation in the middle of the file`` () =
    """
module A
type MyRecord = { Field1: int; Field2: float }
let x: MyRecord = { }
let y = 3
do ()"""
    |> insertStubFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """
module A
type MyRecord = { Field1: int; Field2: float }
let x: MyRecord = { Field1 = failwith ""
                    Field2 = failwith "" }
let y = 3
do ()"""

[<Test; Ignore "Activate when tokenizer is more elaborate">]
let ``basic single-field stub generation when left brace is on next line`` () =
    """
type MyRecord = { Field1: int }
let x: MyRecord =
    { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int }
let x: MyRecord =
    { Field1 = failwith "" }"""


#if INTERACTIVE
``basic single-field record stub generation`` ()
``basic multiple-field record stub generation`` ()
``single-field record stub generation in the middle of the file`` ()
``multiple-field record stub generation in the middle of the file`` ()
#endif