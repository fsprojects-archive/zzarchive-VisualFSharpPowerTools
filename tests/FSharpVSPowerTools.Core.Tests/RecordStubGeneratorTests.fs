#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
      "../../src/FSharpVSPowerTools.Core/RecordStubGenerator.fs"
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
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.RecordStubGenerator
open Microsoft.FSharp.Compiler.Ast

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

let languageService = LanguageService(fun _ -> ())
let project: ProjectOptions =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]
    { ProjectFileName = projFileName
      ProjectFileNames = [| fileName |]
      ProjectOptions = args
      ReferencedProjects = Array.empty
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.UtcNow
      UnresolvedReferences = None }

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
// [ ] Handle record pattern maching: let { Field1 = _; Field2 = _ } = x
// [ ] Add tests for SmartTag generation?

type pos with
    member x.Line0: int<Line0> = LanguagePrimitives.Int32WithMeasure(x.Line - 1)
    member x.Line1: int<Line1> = LanguagePrimitives.Int32WithMeasure(x.Line)
    member x.Column0 = x.Column
    member x.Column1 = x.Column + 1

type Range = {
    StartLine: int<Line1>
    StartColumn: int
    EndLine: int<Line1>
    EndColumn: int
}
with
    static member FromSymbol(symbol: Symbol) =
        let startLine, startColumn, endLine, endColumn = symbol.Range
        { StartLine = LanguagePrimitives.Int32WithMeasure startLine
          StartColumn = startColumn
          EndLine = LanguagePrimitives.Int32WithMeasure endLine
          EndColumn = endColumn }


type MockDocument(src: string) =
    let lines =
        ResizeArray<_>(src.Split([|"\r\n"; "\n"|], StringSplitOptions.None))

    interface IDocument with
        member x.FullName = @"C:\file.fs"
        member x.GetText() = src
        member x.GetLineText0(line0: int<Line0>) = lines.[int line0]
        member x.GetLineText1(line1: int<Line1>) = lines.[int line1 - 1]

type CodeGenerationTestService() =
    interface ICodeGenerationService<ProjectOptions, pos, Range> with
        member x.GetSymbolAtPosition(_project, snapshot, pos) =
            let lineText = snapshot.GetLineText0 pos.Line0
            let src = snapshot.GetText()
            maybe {
                let! symbol =
                    Lexer.getSymbol src (int pos.Line0) (int pos.Column0) lineText args Lexer.queryLexState

                return Range.FromSymbol symbol, symbol
            }

        member x.GetSymbolAndUseAtPositionOfKind(project, snapshot, pos, kind) =
            asyncMaybe {
                let x = x :> ICodeGenerationService<_, _, _>
                let! range, symbol = x.GetSymbolAtPosition(project, snapshot, pos) |> liftMaybe
                let src = snapshot.GetText()
                let line = snapshot.GetLineText1 pos.Line1
                let! parseAndCheckResults =
                    languageService.ParseAndCheckFileInProject(project, snapshot.FullName, src, AllowStaleResults.MatchingSource)
                    |> liftAsync

                match symbol.Kind with
                | k when k = kind ->
                    // NOTE: we must set <colAtEndOfNames> = symbol.RightColumn
                    // and not <pos.Column>, otherwise GetSymbolUseAtLocation won't find it
                    let! symbolUse =
                        parseAndCheckResults.GetSymbolUseAtLocation(pos.Line, symbol.RightColumn, line, [symbol.Text])
                    return range, symbol, symbolUse
                | _ -> return! None |> liftMaybe
            }

        member x.ParseFileInProject(snapshot, projectOptions) =
            languageService.ParseFileInProject(projectOptions, snapshot.FullName, snapshot.GetText())

        member x.ExtractFSharpPos(pos) = pos

let srcToLineArray (src: string) = src.Split([|"\r\n"; "\n"|], StringSplitOptions.None)

let codeGenInfra: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService()

let asDocument (src: string) = MockDocument(src) :> IDocument
let getSymbolAtPoint (pos: pos) (document: IDocument) =
    codeGenInfra.GetSymbolAtPosition(project, document, pos)

let getSymbolAndUseAtPoint (pos: pos) (document: IDocument) =
    codeGenInfra.GetSymbolAndUseAtPositionOfKind(project, document, pos, SymbolKind.Ident)
    |> Async.RunSynchronously

let tryFindRecordExpr (pos: pos) (snapshot: IDocument) =
    tryFindRecordExprInBufferAtPos codeGenInfra project pos snapshot
    |> Async.RunSynchronously

let tryGetRecordStubGenerationParams (pos: pos) (document: IDocument) =
    tryGetRecordStubGenerationParamsAtPos codeGenInfra project pos document
    |> Async.RunSynchronously

let tryGetRecordDefinitionFromPos (pos: pos) (document: IDocument) =
    tryGetRecordDefinitionFromPos codeGenInfra project pos document
    |> Async.RunSynchronously

let insertStubFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let recordDefFromPos = tryGetRecordDefinitionFromPos caretPos document
    match recordDefFromPos with
    | None -> src
    | Some(_, recordExprData, entity, insertPos) ->
        let fieldsWritten = recordExprData.FieldExprList
        let insertColumn = insertPos.Position.Column
        let fieldValue = "failwith \"\""
        let stub = RecordStubGenerator.formatRecord insertPos 4 fieldValue entity fieldsWritten
        let srcLines = srcToLineArray src
        let insertLine0 = insertPos.Position.Line - 1
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
let x: MyRecord = { Field2 = failwith ""
                    Field3 = failwith ""
                    Field1 = 0 }"""

[<Test>]
let ``multiple-field stub generation when some fields are already written 2`` () =
    """
type MyRecord = {Field1: int; Field2: int}
let x = { MyRecord.Field2 = 0 }"""
    |> insertStubFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: int}
let x = { Field1 = failwith ""
          MyRecord.Field2 = 0 }"""

[<Test>]
let ``multiple-field stub generation when all fields are already written 1`` () =
    """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0; Field2 = 0.0 }"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: float}
let x: MyRecord = { Field1 = 0; Field2 = 0.0 }"""

[<Test>]
let ``multiple-field stub generation when all fields are already written 2`` () =
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
let x = { Field21 = { Field12 = failwith ""
                      Field11 = 0 } }"""

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
                     Field12 = failwith ""
                     Field11 = 0 
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
let x = { MyRecord.Field2 = failwith ""
          MyRecord.Field1 = 0 }"""

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
let x = { Field21 = {  Field12 = failwith ""
                       Field11 = 0 } }"""

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
let y = { x with Field2 = failwith ""
                 Field1 = 0 }"""

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
let y: Record = { x with Field2 = failwith ""
                         Field1 = 0 }"""

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
``multiple-field stub generation when all fields are already written 1`` ()
``multiple-field stub generation when all fields are already written 2`` ()
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
#endif