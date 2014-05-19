#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/ColumnIndentedTextWriter.fs"
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
open FSharpVSPowerTools

let args = 
    [|
        "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"
    |]

let framework = FSharpTargetFramework.NET_4_5
let languageService = LanguageService(fun _ -> ())

// [x] Get the syntax construct that you're interested in
// [x] Get the position P where to insert the generated code
// [x] Get the symbol S on which the caret is
// [x] Get symbol S use to get its type metadata (fields, in particular)
// [x] Generate code at position P
// [x] Handle case when some fields are already written
// [x] Handle case when all fields are written
// [x] Handle pattern: let x = { MyRecord1.Field1 = 0 }
// [ ] Handle pattern: let x = { Field1 = 0 }
// [ ] Add tests for SmartTag generation?

#if INTERACTIVE
#load "../../src/FSharpVSPowerTools.Core/RecordStubGenerator.fs"
#endif

open FSharpVSPowerTools
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.RecordStubGenerator
open Microsoft.FSharp.Compiler.Ast

let srcToLineArray (src: string) = src.Split([|"\r\n"; "\n"|], StringSplitOptions.None)

let getSymbolAtPoint (pos: pos) (src: string) =
    let lines = srcToLineArray src
    let lineIdx0 = pos.Line - 1 
    let line = lines.[lineIdx0]
    Lexer.getSymbol src lineIdx0 pos.Column line args Lexer.queryLexState

let getSymbolUseAtPoint (pos: pos) (src: string) =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]
    let lines = srcToLineArray src
    let lineIdx0 = pos.Line - 1
    let line = lines.[lineIdx0]

    match getSymbolAtPoint pos src with
    | Some symbol ->
        let projectOptions: ProjectOptions =
            { ProjectFileName = projFileName
              ProjectFileNames = [| fileName |]
              ProjectOptions = args
              ReferencedProjects = Array.empty
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = false
              LoadTime = DateTime.UtcNow
              UnresolvedReferences = None }

        let parseAndCheckResults =
            languageService.ParseAndCheckFileInProject(projectOptions, fileName, src, AllowStaleResults.MatchingSource)
            |> Async.RunSynchronously

        // NOTE: we must set <colAtEndOfNames> = symbol.RightColumn
        // and not <pos.Column>, otherwise GetSymbolUseAtLocation won't find it
        parseAndCheckResults.GetSymbolUseAtLocation(pos.Line, symbol.RightColumn, line, [symbol.Text])
        |> Async.RunSynchronously
        |> Option.map (fun s -> s, parseAndCheckResults)
    | _ -> None

let tryFindRecordBindingExpTree (pos: pos) (src: string) =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]

    let projectOptions: ProjectOptions =
            { ProjectFileName = projFileName
              ProjectFileNames = [| fileName |]
              ProjectOptions = args
              ReferencedProjects = Array.empty
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = false
              LoadTime = DateTime.UtcNow
              UnresolvedReferences = None }

    let parseResults =
        languageService.ParseFileInProject(projectOptions, fileName, src)
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


let getRecordBindingData (pos: pos) (src: string) =
    maybe {
        let! recordBindingExpTree = tryFindRecordBindingExpTree pos src
        let caretColumn = pos.Column
        let expr, lBraceLeftColumnCondition =
            match recordBindingExpTree with
            | TypedRecordBinding(_, expr, _) -> expr, (fun (t: TokenInformation) -> t.LeftColumn >= caretColumn)
            | QualifiedFieldRecordBinding(expr, _)
            | NonQualifiedFieldRecordBinding(expr, _) -> expr, (fun (t: TokenInformation) -> t.LeftColumn < caretColumn)

        // Tokenize line where the record expression starts
        let exprStartLine1 = expr.Range.StartLine
        let tokens = tokenizeLine src exprStartLine1
        let exprStartLine0 = exprStartLine1 - 1

        let endPosOfLBrace =
            tokens |> List.tryPick (fun (t: TokenInformation) ->
                        if t.CharClass = TokenCharKind.Delimiter &&
                           // t.LeftColumn >= column: only if the expression is on the
                           // same line as the caret position
                           (pos.Line <> exprStartLine1 || lBraceLeftColumnCondition t) &&
                           t.TokenName = "LBRACE" then
                            Some (Pos.fromZ exprStartLine0 (t.RightColumn + 1))
                        else None)

        return recordBindingExpTree, endPosOfLBrace
    }

let getRecordDefinitionFromPoint (pos: pos) (src: string) =
    maybe {
        let! recordBindingData, endPosOfLBrace' = getRecordBindingData pos src
        let! endPosOfLBrace = endPosOfLBrace'
        let! symbolUse, _ = getSymbolUseAtPoint pos src

        match symbolUse.Symbol with
        | :? FSharpEntity as entity ->
            if entity.IsFSharpRecord then
                return! Some (recordBindingData, symbolUse.DisplayContext, entity, endPosOfLBrace)
            else
                return! None
        | :? FSharpField as field ->
            if field.DeclaringEntity.IsFSharpRecord then
                return! Some (recordBindingData, symbolUse.DisplayContext, field.DeclaringEntity, endPosOfLBrace)
            else
                return! None
        | _ ->
            return! None
    }

let insertStubFromPos caretPos src =
    let recordDefnFromPt = getRecordDefinitionFromPoint caretPos src
    match recordDefnFromPt with
    | None -> src
    | Some(TypedRecordBinding(_, _, fieldsWritten), context, entity, insertPos)
    | Some(QualifiedFieldRecordBinding(_, fieldsWritten), context, entity, insertPos)
    | Some(NonQualifiedFieldRecordBinding(_, fieldsWritten), context, entity, insertPos) ->
        let insertColumn = insertPos.Column
        let fieldValue = "failwith \"\""
        let stub = RecordStubGenerator.formatRecord insertColumn 4 fieldValue context entity fieldsWritten
        let srcLines = srcToLineArray src
        let insertLine0 = insertPos.Line - 1
        let curLine = srcLines.[insertLine0]
        let before, after = curLine.Substring(0, insertColumn), curLine.Substring(insertColumn)

        srcLines.[insertLine0] <- before + stub + after

        if srcLines.Length = 0 then
            "" 
        else
            srcLines
            |> Array.reduce (fun line1 line2 -> line1 + "\n" + line2)

let assertSrcAreEqual expectedSrc actualSrc =
    Collection.assertEqual (srcToLineArray expectedSrc) (srcToLineArray actualSrc)

[<Test>]
let ``single-field record stub generation`` () =
    """
type MyRecord = { Field1: int }
let x: MyRecord = { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int }
let x: MyRecord = { Field1 = failwith "" }"""

[<Test>]
let ``multiple-field record stub generation`` () =
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

[<Test>]
let ``single-field stub generation when left brace is on next line`` () =
    """
type MyRecord = { Field1: int }
let x: MyRecord =

    { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int }
let x: MyRecord =

    { Field1 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation when left brace is on next line`` () =
    """
type MyRecord = { Field1: int; Field2: char }
let x: MyRecord =

    { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int; Field2: char }
let x: MyRecord =

    { Field1 = failwith ""
      Field2 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation when some fields are already written (1)`` () =
    """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = { Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = { Field2 = failwith ""
                    Field3 = failwith ""
                    Field1 = 0 }"""

[<Test>]
let ``multiple-field stub generation when some fields are already written (2)`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = { Field1 = failwith ""
          MyRecord.Field2 = 0 }"""

[<Test>]
let ``multiple-field stub generation when all fields are already written (1)`` () =
    """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0; Field2 = 0.0 }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0; Field2 = 0.0 }"""

[<Test>]
let ``multiple-field stub generation when all fields are already written (2)`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field1 = 0; MyRecord.Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field1 = 0; MyRecord.Field2 = 0 }"""

[<Test>]
let ``multiple-field stub generation with some qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = Some { MyRecord.Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 16)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = Some { Field1 = failwith ""
               MyRecord.Field2 = 0 }"""

[<Test>]
let ``multiple-field stub generation with all qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x: MyRecord = { MyRecord.Field1 = 0;
                    MyRecord.Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x: MyRecord = { MyRecord.Field1 = 0;
                    MyRecord.Field2 = 0 }"""

[<Test>]
let ``multiple-field stub generation with some non-qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = 
    let y = 0
    { Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = 
    let y = 0
    { Field1 = failwith ""
      Field2 = 0 }"""

[<Test>]
let ``multiple-field stub generation with all non-qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = { Field1 = 0; Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 10)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = { Field1 = 0; Field2 = 0 }"""

[<Test>]
let ``support record fields that are also records`` () =
    """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1; Field22: int}
let x = { Field21 = { Field11 = 0 } }"""
    |> insertStubFromPos (Pos.fromZ 3 10)
    |> assertSrcAreEqual """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1; Field22: int}
let x = { Field22 = failwith ""
          Field21 = { Field11 = 0 } }"""

[<Test; Ignore "This feature should insert fields based on cursor positions">]
let ``support record fields nested inside other records`` () =
    """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1; Field22: int}
let x = { Field21 = { Field11 = 0 } }"""
    |> insertStubFromPos (Pos.fromZ 3 22)
    |> assertSrcAreEqual """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1; Field22: int}
let x = { Field22 = failwith ""
          Field21 = { Field11 = 0 } }"""

[<Test>]
let ``print fully-qualified field names on fully-qualified records`` () =
    """
[<RequireQualifiedAccess>]
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 3 10)
    |> assertSrcAreEqual """
[<RequireQualifiedAccess>]
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field2 = failwith ""
          MyRecord.Field1 = 0 }"""

#if INTERACTIVE
``single-field record stub generation`` ()
``multiple-field record stub generation`` ()
``single-field record stub generation in the middle of the file`` ()
``multiple-field record stub generation in the middle of the file`` ()
``single-field stub generation when left brace is on next line`` ()
``multiple-field stub generation when left brace is on next line`` ()
``multiple-field stub generation when some fields are already written (1)`` ()
``multiple-field stub generation when some fields are already written (2)`` ()
``multiple-field stub generation when all fields are already written (1)`` ()
``multiple-field stub generation when all fields are already written (2)`` ()
``multiple-field stub generation with some qualified fields already written`` ()
``multiple-field stub generation with all qualified fields already written`` ()
``multiple-field stub generation with some non-qualified fields already written`` ()
``multiple-field stub generation with all non-qualified fields already written`` ()
#endif