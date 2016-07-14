#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
      "../../src/FSharpVSPowerTools.Core/RecordStubGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharp.Editing.Tests.RecordStubGeneratorTests
#endif

open NUnit.Framework
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open TestHelpers.LanguageServiceTestHelper
open FSharp.Editing
open FSharp.Editing.Features.RecordStubGenerator
open FSharp.Editing.Features
open CodeGenerationTestInfrastructure

let args = LanguageServiceTestHelper.args
let languageService = LanguageService()

// [ ] write design doc
// [x] Get the syntax construct that you're interested in
// [x] Get the position P where to insert the generated code
// [x] Get the symbol S on which the caret is
// [x] Get symbol S use to get its type metadata (fields, in particular)
// [x] Generate code at position P
// [x] Handle case when some fields are already written
// [x] Handle case when all fields are written
// [x] Handle pattern: let x = { MyRecord1.Field1 = 0 }
// [x] Handle pattern: let x = { Field1 = 0 }
// [x] Handle copy-and-update expression
// [x] Test that most unparseable expression don't trigger code gen
// [ ] Handle record pattern matching: let { Field1 = _; Field2 = _ } = x

let tryFindRecordDefinitionFromPos codeGenInfra (pos: pos) (document: IDocument) =
    tryFindRecordDefinitionFromPos codeGenInfra (projectOptions document.FullName) pos document
    |> Async.RunSynchronously

[<NoComparison; NoEquality>]
type CodeGenDiagnostic = {
    mutable Range: range option
    mutable RecordExpr: RecordExpr option
    mutable InsertionParams: RecordStubsInsertionParams option
}

let diagnoseCodeGenParams (pos: pos) src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)
    let project = projectOptions document.FullName

    let diagnostic = {
        Range = None
        RecordExpr = None
        InsertionParams = None
    }

    asyncMaybe {
        let! recordExpr = tryFindRecordExprInBufferAtPos codeGenService project pos document
        diagnostic.Range <- Some recordExpr.Expr.Range
        diagnostic.RecordExpr <- Some recordExpr

        let! _, insertionParams = tryFindStubInsertionParamsAtPos codeGenService project pos document
        diagnostic.InsertionParams <- Some insertionParams
    }
    |> Async.Ignore
    |> Async.RunSynchronously

    diagnostic

let insertStubFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)
    let recordDefFromPos = tryFindRecordDefinitionFromPos codeGenService caretPos document
    match recordDefFromPos with
    | Some(_, recordExprData, entity, insertPos) ->
        let fieldsWritten = recordExprData.FieldExprList
        let insertColumn = insertPos.InsertionPos.Column
        let fieldValue = "failwith \"\""
        let stub = RecordStubGenerator.formatRecord insertPos fieldValue entity fieldsWritten
        let srcLines = srcToLineArray src
        let insertLine0 = insertPos.InsertionPos.Line - 1
        let curLine = srcLines.[insertLine0]
        let before, after = curLine.Substring(0, insertColumn), curLine.Substring(insertColumn)

        srcLines.[insertLine0] <- before + stub + after

        if srcLines.Length = 0 then
            "" 
        else
            srcLines
            |> Array.reduce (fun line1 line2 -> line1 + "\n" + line2)
    | _ -> src

[<Test>]
let ``single-field typed record stub generation`` () =
    """
type MyRecord = { Field1: int }
let x: MyRecord = { }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int }
let x: MyRecord = { Field1 = failwith "" }"""

[<Test>]
let ``multiple-field typed record stub generation 1`` () =
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
let ``multiple-field typed record stub generation 2`` () =
    """
type Record = { Field1: int; Field2: int }
let x = { } : Record"""
    |> insertStubFromPos (Pos.fromZ 2 14)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }
let x = { Field1 = failwith ""
          Field2 = failwith "" } : Record"""

[<Test>]
let ``multiple-field typed record stub generation 3`` () =
    """
type Record = { Field1: int; Field2: int }
let x = { Field1 = 0; Field2 = 0 }
let y = { x with } : Record"""
    |> insertStubFromPos (Pos.fromZ 3 21)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }
let x = { Field1 = 0; Field2 = 0 }
let y = { x with Field1 = failwith ""
                 Field2 = failwith "" } : Record"""

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
let ``multiple-field stub generation when some fields are already written 1`` () =
    """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = { Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = { Field1 = 0
                    Field2 = failwith ""
                    Field3 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation when some fields are already written 2`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field1 = 0
          Field2 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation when all fields are already written`` () =
    [
        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0; Field2 = 0.0 }"""

        """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field1 = 0; MyRecord.Field2 = 0 }"""
    ]
    |> List.map (getSrcBeforeAndAfterCodeGen (insertStubFromPos (Pos.fromZ 2 11)))
    |> List.iter assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``multiple-field stub generation with some qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = Some { MyRecord.Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 16)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = Some { MyRecord.Field1 = 0
               Field2 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation with all qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x: MyRecord = { MyRecord.Field1 = 0;
                    MyRecord.Field2 = 0 }"""
    |> getSrcBeforeAndAfterCodeGen (insertStubFromPos (Pos.fromZ 2 7))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``multiple-field stub generation with some non-qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = 
    let y = 0
    { Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = 
    let y = 0
    { Field1 = 0
      Field2 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation with all non-qualified fields already written`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = { Field1 = 0; Field2 = 0 }"""
    |> getSrcBeforeAndAfterCodeGen (insertStubFromPos (Pos.fromZ 2 10))
    |> assertSrcWasNotChangedAfterCodeGen

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
let x = { Field21 = { Field11 = 0 }
          Field22 = failwith "" }"""

[<Test>]
let ``support record fields nested inside other records 1`` () =
    """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1; Field22: int}
let x = { Field21 = { Field11 = 0 } }"""
    |> insertStubFromPos (Pos.fromZ 3 22)
    |> assertSrcAreEqual """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1; Field22: int}
let x = { Field21 = { Field11 = 0
                      Field12 = failwith "" } }"""

[<Test>]
let ``support record fields nested inside other records 2`` () =
    """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1}
let x = { Field21 = { 
                     Field11 = 0
                        } }"""
    |> insertStubFromPos (Pos.fromZ 4 21)
    |> assertSrcAreEqual """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1}
let x = { Field21 = { 
                     Field11 = 0
                     Field12 = failwith ""
                        } }"""

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
let x = { MyRecord.Field1 = 0
          MyRecord.Field2 = failwith "" }"""

[<Test>]
let ``multiple-field record stub generation with record pattern in let binding`` () =
    """
type Record = { Field1: int; Field2: int }
let { Field1 = a; Field2 = b }: Record = { }"""
    |> insertStubFromPos (Pos.fromZ 2 34)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }
let { Field1 = a; Field2 = b }: Record = { Field1 = failwith ""
                                           Field2 = failwith "" }"""

[<Test>]
let ``support fields with extra-space before them`` () =
    """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1}
let x = { Field21 = {  Field11 = 0 } }"""
    |> insertStubFromPos (Pos.fromZ 3 24)
    |> assertSrcAreEqual """
type Record1 = {Field11: int; Field12: int}
type Record2 = {Field21: Record1}
let x = { Field21 = {  Field11 = 0
                       Field12 = failwith "" } }"""

[<Test>]
let ``support copy-and-update expression`` () =
    """
type Record = { Field1: int; Field2: int }

let x = { Field1 = 0; Field2 = 0 }
let y = { x with Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 4 17)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }

let x = { Field1 = 0; Field2 = 0 }
let y = { x with Field1 = 0
                 Field2 = failwith "" }"""

[<Test>]
let ``support typed-record binding with non-empty copy-and-update expression`` () =
    """
type Record = { Field1: int; Field2: int }

let x = { Field1 = 0; Field2 = 0 }
let y: Record = { x with Field1 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 4 7)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }

let x = { Field1 = 0; Field2 = 0 }
let y: Record = { x with Field1 = 0
                         Field2 = failwith "" }"""

[<Test>]
let ``support typed-record binding with empty copy-and-update expression`` () =
    """
type Record = { Field1: int; Field2: int }

let x = { Field1 = 0; Field2 = 0 }
let y: Record = { x with }"""
    |> insertStubFromPos (Pos.fromZ 4 7)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }

let x = { Field1 = 0; Field2 = 0 }
let y: Record = { x with Field1 = failwith ""
                         Field2 = failwith "" }"""

[<Test>]
let ``uses correct indenting when inserting new line`` () =
    [
        """
type Record1 = {Field1: int; Field2: int; Field3: int}
let x = { Field1 = 0; Field2 = 0 }"""

        """
type Record1 = {Field1: int; Field2: int; Field3: int}
let x = { Field1 = 0; Field2 = 0; }"""
    ]
    |> List.map (insertStubFromPos (Pos.fromZ 2 10))
    |> assertSrcSeqAreEqual [
        """
type Record1 = {Field1: int; Field2: int; Field3: int}
let x = { Field1 = 0; Field2 = 0
          Field3 = failwith "" }"""

        """
type Record1 = {Field1: int; Field2: int; Field3: int}
let x = { Field1 = 0; Field2 = 0;
          Field3 = failwith "" }"""
    ]

[<Test>]
let ``doesn't trigger code gen when caret is on a field name in a field assignment expr`` () =
    [
        """
type Pos = { Line: int; Col: int }
let pos = { Line = 0; Col = 0 }

let pos2: Pos =
    { pos with Line = pos.Line }"""

        """
type Pos = { Line: int; Col: int }
let pos = { Line = 0; Col = 0 }

let pos2: Pos =
    { pos with Line = match true with _ -> pos.Line }"""
    ]
    |> List.zip [
        Pos.fromZ 5 26
        Pos.fromZ 5 47
    ]
    |> List.map (fun (pos, src) ->
        getSrcBeforeAndAfterCodeGen (insertStubFromPos pos) src
    )
    |> List.iter assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``doesn't trigger code gen when record expr doesn't end by }`` () =
    [
        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = {"""

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Fiel"""

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1"""

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 ="""

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0"""
    ]
    |> List.map (getSrcBeforeAndAfterCodeGen (insertStubFromPos (Pos.fromZ 2 7)))
    |> List.iter (assertSrcWasNotChangedAfterCodeGen)

[<Test>]
let ``doesn't trigger code gen when record expr doesn't end by } when there's code after`` () =
    [
        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = {
let y = seq { yield 0 } """

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Fiel
let y = seq { yield 0 } """

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1
let y = seq { yield 0 } """

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 =
let y = seq { yield 0 } """

        """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0
let y = seq { yield 0 } """
    ]
    |> List.map (getSrcBeforeAndAfterCodeGen (insertStubFromPos (Pos.fromZ 2 7)))
    |> List.iter (assertSrcWasNotChangedAfterCodeGen)


#if INTERACTIVE
``single-field typed record stub generation`` ()
``multiple-field typed record stub generation 1`` ()
``multiple-field typed record stub generation 2`` ()
``multiple-field typed record stub generation 3`` ()
``single-field record stub generation in the middle of the file`` ()
``multiple-field record stub generation in the middle of the file`` ()
``single-field stub generation when left brace is on next line`` ()
``multiple-field stub generation when left brace is on next line`` ()
``multiple-field stub generation when some fields are already written 1`` ()
``multiple-field stub generation when some fields are already written 2`` ()
``multiple-field stub generation when all fields are already written`` ()
``multiple-field stub generation with some qualified fields already written`` ()
``multiple-field stub generation with all qualified fields already written`` ()
``multiple-field stub generation with some non-qualified fields already written`` ()
``multiple-field stub generation with all non-qualified fields already written`` ()
``support record fields that are also records`` ()
``support record fields nested inside other records 1`` ()
``support record fields nested inside other records 2`` ()
``print fully-qualified field names on fully-qualified records`` ()
``multiple-field record stub generation with record pattern in let binding`` ()
``support fields with extra-space before them`` ()
``support copy-and-update expression`` ()
``support typed-record binding with non-empty copy-and-update expression`` ()
``support typed-record binding with empty copy-and-update expression`` ()
``doesn't trigger code gen when record expr doesn't end by }`` ()
``doesn't trigger code gen when record expr doesn't end by } when there's code after`` ()
#endif