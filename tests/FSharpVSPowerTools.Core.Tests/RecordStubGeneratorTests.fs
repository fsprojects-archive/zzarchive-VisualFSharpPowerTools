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
// [x] Handle pattern: let x = { Field1 = 0 }
// [ ] Add tests for SmartTag generation?
// [ ] Handle record pattern maching: let { Field1 = _; Field2 = _ } = x
// [ ] Handle copy-and-update expression

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

let tryGetLeftPosOfFirstRecordField recordExprCategory =
    match recordExprCategory with
    | TypedRecordBinding(_, _, [])
    | QualifiedFieldRecordBinding(_, [])
    | NonQualifiedFieldRecordBinding(_, []) -> None
    | TypedRecordBinding(_, _, fstFieldInfo :: _)
    | QualifiedFieldRecordBinding(_, fstFieldInfo :: _)
    | NonQualifiedFieldRecordBinding(_, fstFieldInfo :: _) ->
        let (fieldIdentifier, _), _, _ = fstFieldInfo
        Some (fieldIdentifier.Range.Start)

let getRecordBindingData (pos: pos) (src: string) =
    maybe {
        let! recordBindingExprTree = tryFindRecordBindingExpTree pos src
        let caretColumn = pos.Column
        let expr =
            match recordBindingExprTree with
            | TypedRecordBinding(_, expr, _)
            | QualifiedFieldRecordBinding(expr, _)
            | NonQualifiedFieldRecordBinding(expr, _) -> expr

        let insertionPos =
            let lPosOfFirstField = tryGetLeftPosOfFirstRecordField recordBindingExprTree

            match lPosOfFirstField with
            | Some _ -> lPosOfFirstField
            | None ->
                let lBraceLeftColumnCondition =
                    match recordBindingExprTree with
                    | TypedRecordBinding(_, expr, _) ->
                        (fun (t: TokenInformation) -> t.LeftColumn >= caretColumn)

                    | QualifiedFieldRecordBinding(expr, _)
                    | NonQualifiedFieldRecordBinding(expr, _) ->
                        let isLBraceInExpressionRange (t: TokenInformation) =
                            expr.Range.StartColumn <= t.LeftColumn && t.LeftColumn < caretColumn

                        isLBraceInExpressionRange

                // Tokenize line where the record expression starts
                let exprStartLine1 = expr.Range.StartLine
                let tokens = tokenizeLine src exprStartLine1
                let exprStartLine0 = exprStartLine1 - 1

                let endPosOfLBrace =
                    tokens |> List.tryPick (fun (t: TokenInformation) ->
                                if t.CharClass = TokenCharKind.Delimiter &&
                                   // t.LeftColumn >= column: only if the expression is on the
                                   // same line as the caret position
                                   t.TokenName = "LBRACE" &&
                                   (pos.Line <> exprStartLine1 || lBraceLeftColumnCondition t) then
                                   Some (Pos.fromZ exprStartLine0 (t.RightColumn + 1))
                                else None)
                endPosOfLBrace

        return recordBindingExprTree, insertionPos
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
let x: MyRecord = {}"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int }
let x: MyRecord = {Field1 = failwith ""}"""

[<Test>]
let ``multiple-field record stub generation`` () =
    """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = {}"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = {Field1: int; Field2: float; Field3: float}
let x: MyRecord = {Field1 = failwith ""
                   Field2 = failwith ""
                   Field3 = failwith ""}"""

[<Test>]
let ``single-field record stub generation in the middle of the file`` () =
    """
module A
type MyRecord = { Field1: int }
let x: MyRecord = {}
do ()"""
    |> insertStubFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """
module A
type MyRecord = { Field1: int }
let x: MyRecord = {Field1 = failwith ""}
do ()"""

[<Test>]
let ``multiple-field record stub generation in the middle of the file`` () =
    """
module A
type MyRecord = { Field1: int; Field2: float }
let x: MyRecord = {}
let y = 3
do ()"""
    |> insertStubFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """
module A
type MyRecord = { Field1: int; Field2: float }
let x: MyRecord = {Field1 = failwith ""
                   Field2 = failwith ""}
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

    {Field1 = failwith "" }"""

[<Test>]
let ``multiple-field stub generation when left brace is on next line`` () =
    """
type MyRecord = { Field1: int; Field2: char }
let x: MyRecord =

    {}"""
    |> insertStubFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """
type MyRecord = { Field1: int; Field2: char }
let x: MyRecord =

    {Field1 = failwith ""
     Field2 = failwith ""}"""

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

[<Test>]
let ``support record fields nested inside other records (1)`` () =
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
let ``support record fields nested inside other records (2)`` () =
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
let { Field1 = a; Field2 = b }: Record = {}"""
    |> insertStubFromPos (Pos.fromZ 2 34)
    |> assertSrcAreEqual """
type Record = { Field1: int; Field2: int }
let { Field1 = a; Field2 = b }: Record = {Field1 = failwith ""
                                          Field2 = failwith ""}"""

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

[<Test; Ignore("Reactivate when proper support of copy-and-update is implemented")>]
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
                         Field2 = failwith ""}"""

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
``support record fields that are also records`` ()
``support record fields nested inside other records (1)`` ()
``support record fields nested inside other records (2)`` ()
``print fully-qualified field names on fully-qualified records`` ()
``multiple-field record stub generation with record pattern in let binding`` ()
``support fields with extra-space before them`` ()
``support copy-and-update expression`` ()
``support typed-record binding with non-empty copy-and-update expression`` ()
#endif