#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
      "../../src/FSharpVSPowerTools.Core/UnionMatchCaseGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestService.fs"
#else
module FSharpVSPowerTools.Core.Tests.UnionMatchCaseGeneratorTests
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
open FSharpVSPowerTools.CodeGeneration.UnionMatchCaseGenerator
open FSharpVSPowerTools.Core.Tests
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

type MockDocument(src: string) =
    let lines =
        ResizeArray<_>(src.Split([|"\r\n"; "\n"|], StringSplitOptions.None))

    interface IDocument with
        member x.FullName = @"C:\file.fs"
        member x.GetText() = src
        member x.GetLineText0(line0: int<Line0>) = lines.[int line0]
        member x.GetLineText1(line1: int<Line1>) = lines.[int line1 - 1]

let srcToLineArray (src: string) = src.Split([|"\r\n"; "\n"|], StringSplitOptions.None)

let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)

let asDocument (src: string) = MockDocument(src) :> IDocument
let getSymbolAtPoint (pos: pos) (document: IDocument) =
    codeGenService.GetSymbolAtPosition(project, document, pos)

let getSymbolAndUseAtPoint (pos: pos) (document: IDocument) =
    codeGenService.GetSymbolAndUseAtPositionOfKind(project, document, pos, SymbolKind.Ident)
    |> Async.RunSynchronously

let tryFindMatchExpr (pos: pos) (document: IDocument) =
    tryFindMatchExprInBufferAtPos codeGenService project pos document
    |> Async.RunSynchronously

let tryFindUnionTypeDefinition (pos: pos) document =
    tryFindUnionTypeDefinitionFromPos codeGenService project pos document
    |> Async.RunSynchronously

let tryFindMatchCaseGenerationParam pos document =
    tryFindMatchCaseInsertionParamsAtPos codeGenService project pos document
    |> Async.RunSynchronously

let insertCasesFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let unionTypeDefFromPos = tryFindUnionTypeDefinition caretPos document
    match unionTypeDefFromPos with
    | None -> src
    | Some(range, matchExpr, entity, insertionParams) ->
        let insertionPos = insertionParams.InsertionPos
        let insertColumn = insertionPos.Column
        let caseValue = "failwith \"\""
        let stub = UnionMatchCaseGenerator.formatMatchExpr insertionParams caseValue matchExpr entity
        let srcLines = srcToLineArray src
        let insertLine0 = insertionPos.Line - 1
        let curLine = srcLines.[insertLine0]
        let before, after = curLine.Substring(0, insertColumn), curLine.Substring(insertColumn)

        srcLines.[insertLine0] <- before + stub + after

        if srcLines.Length = 0 then
            "" 
        else
            srcLines
            |> Array.reduce (fun line1 line2 -> line1 + "\n" + line2)

// [x] Handle case where first case doesn't start with '|'

// TODO: dedup from RecordStubGeneratorTests.fs
let assertSrcAreEqual expectedSrc actualSrc =
    Collection.assertEqual (srcToLineArray expectedSrc) (srcToLineArray actualSrc)

let tryGetWrittenCases (pos: pos) (src: string) =
    src
    |> asDocument
    |> tryFindMatchExpr pos
    |> Option.map (getWrittenCases)
    |> Option.getOrElse Set.empty

module ClausesAnalysisTests =
    [<Test>]
    let ``OR patterns with constants and identifiers`` () =
        """type Union = Case1 | Case2 | Case3 of bool | Case4 of int * int
let f union = match union with
    | Case3 true
    | Case3(i)
    | Case4(3, _) -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set ["Case3"])

    [<Test>]
    let ``OR patterns with wildcards and qualified identifiers`` () =
        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
let f union = match union with
    | Case2 | Union.Case2 | Case4(_,_)
    | Case3 _
    | Union.Case4 _ -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set ["Case2"; "Case3"; "Case4"])

    [<Test>]
    let ``redundant simple AND pattern`` () =
        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
let f union = match union with
    | Case2 & Case2 -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set ["Case2"])

    [<Test>]
    let ``AND pattern with wildcards`` () =
        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
let f union = match union with
    | Case2 & Case4(_,_) -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set [])


[<Test>]
let ``single union match case generation when the unique case is written`` () =
    """
type Union = Case1

let f union =
    match union with | Case1 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1

let f union =
    match union with | Case1 -> ()"""


[<Test>]
let ``union match case generation when all cases are written 1`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1 -> ()
    | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1 -> ()
    | Case2 -> ()"""

[<Test>]
let ``union match case generation when all cases are written 2`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with Case1 -> () | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 4)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with Case1 -> () | Case2 -> ()"""

[<Test>]
let ``union match case generation when first clause doesn't start with '|'`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 4)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    Case1 -> failwith ""
    | Case2 -> ()"""

[<Test>]
let ``union match case generation with multiple-argument constructors`` () =
    """
type Union = Case1 | Case2 | Case3 of int | Case4 of int * int

let f union =
    match union with
    | Case1 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2 | Case3 of int | Case4 of int * int

let f union =
    match union with
    | Case2 -> failwith ""
    | Case3(_) -> failwith ""
    | Case4(_, _) -> failwith ""
    | Case1 -> ()"""

[<Test>]
let ``union match case generation with required qualified access`` () =
    """
[<RequireQualifiedAccess>]
type Union = Case1 | Case2

let f union =
    match union with
    | Union.Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 6 6)
    |> assertSrcAreEqual """
[<RequireQualifiedAccess>]
type Union = Case1 | Case2

let f union =
    match union with
    | Union.Case1 -> failwith ""
    | Union.Case2 -> ()"""


[<Test>]
let ``union match case generation with combined clauses`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1
    | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1
    | Case2 -> ()"""

[<Test>]
let ``union match case generation with parenthesized cases`` () =
    """
type Union = Case1 | Case2 of int

let f union =
    match union with
    | (Case2(x))
    | ((((Case1)))) -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 7)
    |> assertSrcAreEqual """
type Union = Case1 | Case2 of int

let f union =
    match union with
    | (Case2(x))
    | ((((Case1)))) -> ()"""

[<Test>]
let ``union match case generation with combined clauses with multiple args`` () =
    """
type Union = Case1 | Case2 of int

let f union =
    match union with
    | Case1
    | Case2 _ -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2 of int

let f union =
    match union with
    | Case1
    | Case2 _ -> ()"""

[<Test>]
let ``union match case generation with guards written`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    | Case2 when 1 = 2 -> ()
    | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1 -> failwith ""
    | Case2 when 1 = 2 -> ()
    | Case2 -> ()"""

[<Test>]
let ``nested union match case generation`` () =
    """
type Union1 = Case1 | Case2
type Union2 = Case3 * int | Case4 of Union1 | Case5

let f union2 =
    match union2 with
    | Case3 _ -> ()
    | Case4(union1) ->
        match union1 with
        | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 9 10)
    |> assertSrcAreEqual """
type Union1 = Case1 | Case2
type Union2 = Case3 * int | Case4 of Union1 | Case5

let f union2 =
    match union2 with
    | Case3 _ -> ()
    | Case4(union1) ->
        match union1 with
        | Case1 -> failwith ""
        | Case2 -> ()"""

[<Test>]
let ``union match case generation is inactive on tuples`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1, Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 7)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1, Case2 -> ()"""

[<Test>]
let ``union match case generation is inactive on lists`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | [Case1] -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 7)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f u =
    match u with
    | [Case1] -> ()"""

[<Test>]
let ``union match case generation is inactive on list cons pattern`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1 :: [Case1] -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 7)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1 :: [Case1] -> ()"""

[<Test>]
let ``union match case generation is inactive on chained list cons pattern`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1 :: x :: Case2 :: [Case1] -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 7)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1 :: x :: Case2 :: [Case1] -> ()"""

[<Test>]
let ``union match case generation is inactive on arrays`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | [|Case1|] -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 8)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f u =
    match u with
    | [|Case1|] -> ()"""

[<Test>]
let ``generate identifier names when a union field has one`` () =
    """
type Union =
    | Case1 of arg1:int * int * arg3:string
    | Case2 of arg1:int * arg2:string
    | Case3

let f x =
    match x with
    | Case3 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 8 6)
    |> assertSrcAreEqual """
type Union =
    | Case1 of arg1:int * int * arg3:string
    | Case2 of arg1:int * arg2:string
    | Case3

let f x =
    match x with
    | Case1(arg1, _, arg3) -> failwith ""
    | Case2(arg1, arg2) -> failwith ""
    | Case3 -> ()"""

[<Test>]
let ``union match lambda case generation when first clause starts with '|'`` () =
    """
type Union = Case1 | Case2

let f = function
    | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f = function
    | Case1 -> failwith ""
    | Case2 -> ()"""

[<Test>]
let ``union match lambda case generation when first clause doesn't start with '|'`` () =
    """
type Union = Case1 | Case2

let f = function
    Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f = function
    Case1 -> failwith ""
    | Case2 -> ()"""


// Union match case without argument patterns
//// SynPat.LongIdent(_longIdentWithDots, _identOption, _synVarTyplDecl, _synConstrArg, _synAccessOpt, _range
//// LongIdent(LongIdentWithDots[(Case2, rangeList: [])], identOption: null, typlDecl: null, constrArgs: Pats [], accessOpt: null, _range)
//expr.[0].RangeOfGuardAndRhs

// Union match case with argument patterns
//// SynPat.LongIdent(_longIdentWithDots, _identOption, _synVarTyplDecl, _synConstrArg, _synAccessOpt, _range
//// New: _synConstrArg
//// LongIdent(LongIdentWithDots[(Case3, rangeList: [])], identOption: null, typlDecl: null, constrArgs: Pats [Wild...], accessOpt: null, _range)
//expr.[1].Pattern