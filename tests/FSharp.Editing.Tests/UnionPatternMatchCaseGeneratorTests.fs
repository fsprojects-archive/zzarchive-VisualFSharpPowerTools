#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/UntypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
      "../../src/FSharpVSPowerTools.Core/UnionPatternMatchCaseGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharp.Editing.Tests.UnionPatternMatchCaseGeneratorTests
#endif

open NUnit.Framework
open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open TestHelpers.LanguageServiceTestHelper
open FSharp.Editing
open FSharp.Editing.Features
open CodeGenerationTestInfrastructure
open FSharp.Editing.Features.UnionPatternMatchCaseGenerator

let languageService = LanguageService()

let tryFindUnionDefinition codeGenService (pos: pos) (document: IDocument) =
    tryFindUnionDefinitionFromPos codeGenService (projectOptions document.FullName) pos document
    |> Async.RunSynchronously

let insertCasesFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, LanguageServiceTestHelper.args)
    let unionTypeDefFromPos = tryFindUnionDefinition codeGenService caretPos document
    match unionTypeDefFromPos with
    | None -> src
    | Some(_range, matchExpr, entity, insertionParams) ->
        let insertionPos = insertionParams.InsertionPos
        let insertColumn = insertionPos.Column
        let caseValue = "failwith \"\""
        let stub = UnionPatternMatchCaseGenerator.formatMatchExpr
                        insertionParams caseValue matchExpr entity
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

module ClausesAnalysisTests =
    open TestHelpers.LanguageServiceTestHelper

    let tryFindPatternMatchExpr codeGenService (pos: pos) (document: IDocument) =
        tryFindPatternMatchExprInBufferAtPos codeGenService (projectOptions document.FullName) pos document
        |> Async.RunSynchronously


    let tryGetWrittenCases (pos: pos) (src: string) =
        let codeGenService: ICodeGenerationService<_, _, _> = 
            upcast CodeGenerationTestService(LanguageService(), LanguageServiceTestHelper.args)
        src 
        |> asDocument
        |> tryFindPatternMatchExpr codeGenService pos
        |> Option.map (getWrittenCases)
        |> Option.getOrElse Set.empty

    [<Test>]
    let ``OR patterns with constants and identifiers`` () =
        """type Union = Case1 | Case2 | Case3 of bool | Case4 of int * int
let f union = match union with
    | Case3 true
    | Case3(i)
    | Case4(3, _) -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set ["Case3", []])

    [<Test>]
    let ``OR patterns with wildcards and qualified identifiers`` () =
        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
let f union = match union with
    | Case2 | Union.Case2 | Case4(_,_)
    | Case3 _
    | Union.Case4 _ -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set ["Case2", []; "Case2", ["Union"]; "Case4", []; "Case3", []; "Case4", ["Union"]])

    [<Test>]
    let ``redundant simple AND pattern`` () =
        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
let f union = match union with
    | Case2 & Case2 -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set ["Case2", []])

    [<Test>]
    let ``AND pattern with wildcards`` () =
        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
let f union = match union with
    | Case2 & Case4(_,_) -> ()"""
        |> tryGetWrittenCases (Pos.fromZ 2 6)
        |> assertEqual (set [])

    [<Test>]
    let ``union cases in different modules and namespaces are respected`` () =
        """
namespace TestNamespace

module M =
    type DU = | C1 of string | C2 of int | C3 of double

namespace TestNamespace2

module M =
    let f x =
        match x with
        | TestNamespace.M.DU.C1 s -> ()
"""
        |> tryGetWrittenCases (Pos.fromZ 11 12)
        |> assertEqual (set ["C1", ["TestNamespace"; "M"; "DU"]])

    [<Test>]
    let ``union cases specified in different styles are correctly captured`` () =
        """
namespace TestNamespace

module M =
    type DU = | C1 of string | C2 of int | C3 of double

namespace TestNamespace2
open TestNamespace.M
module M =
    let f x =
        match x with
        | TestNamespace.M.DU.C1 s -> ()
        | C2 -> ()
"""
        |> tryGetWrittenCases (Pos.fromZ 11 12)
        |> assertEqual (set ["C1", ["TestNamespace"; "M"; "DU"]; "C2", []])

[<Test>]
let ``single union match case generation when the unique case is written`` () =
    """
type Union = Case1

let f union =
    match union with | Case1 -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 6))
    |> assertSrcWasNotChangedAfterCodeGen


[<Test>]
let ``union match case generation when all cases are written 1`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1 -> ()
    | Case2 -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 6))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation when all cases are written 2`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with Case1 -> () | Case2 -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 4))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation when first clause doesn't start with '|'`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 5 4)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    Case1 -> ()
    | Case2 -> failwith ""
"""

[<Test>]
let ``union match case generation with multiple-argument constructors`` () =
    """
type Union = Case1 | Case2 | Case3 of int | Case4 of int * int

let f union =
    match union with
    | Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2 | Case3 of int | Case4 of int * int

let f union =
    match union with
    | Case1 -> ()
    | Case2 -> failwith ""
    | Case3(_) -> failwith ""
    | Case4(_, _) -> failwith ""
"""

[<Test>]
let ``union match case generation with required qualified access`` () =
    """
[<RequireQualifiedAccess>]
type Union = Case1 | Case2

let f union =
    match union with
    | Union.Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 6 6)
    |> assertSrcAreEqual """
[<RequireQualifiedAccess>]
type Union = Case1 | Case2

let f union =
    match union with
    | Union.Case1 -> ()
    | Union.Case2 -> failwith ""
"""


[<Test>]
let ``union match case generation with combined clauses`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1
    | Case2 -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 6))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation with parenthesized cases`` () =
    """
type Union = Case1 | Case2 of int

let f union =
    match union with
    | (Case2(x))
    | ((((Case1)))) -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 7))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation with combined clauses with multiple args`` () =
    """
type Union = Case1 | Case2 of int

let f union =
    match union with
    | Case1
    | Case2 _ -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 6))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation with guards written`` () =
    """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1 -> ()
    | Case1 when 1 = 2 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    | Case1 -> ()
    | Case1 when 1 = 2 -> ()
    | Case2 -> failwith ""
"""

[<Test>]
let ``union match case generation with single case + nested constant binding`` () =
    """
type Name = { First:string; Last:string }
type Person = 
    | Basic of Name * int

let f1 x =
    match x with
    | Basic({First = "joe"}, _) -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 7 6)
    |> assertSrcAreEqual """
type Name = { First:string; Last:string }
type Person = 
    | Basic of Name * int

let f1 x =
    match x with
    | Basic({First = "joe"}, _) -> ()
    | Basic(_, _) -> failwith ""
"""

[<Test>]
let ``union match case generation with single case + guard`` () =
    """
type Name = { First:string; Last:string }
type Person = 
    | Basic of Name * int

let f1 x = 
    match x with
    | Basic(name, age) when age > 10 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 7 6)
    |> assertSrcAreEqual """
type Name = { First:string; Last:string }
type Person = 
    | Basic of Name * int

let f1 x = 
    match x with
    | Basic(name, age) when age > 10 -> ()
    | Basic(_, _) -> failwith ""
"""

[<Test>]
let ``handle abbreviated option types`` () =
    """
let f (i: int option) =
    match i with
    | Some _ -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 3 6)
    |> assertSrcAreEqual """
let f (i: int option) =
    match i with
    | Some _ -> ()
    | None -> failwith ""
"""

[<Test>]
let ``handle abbreviated option types with qualified access`` () =
    """
[<RequireQualifiedAccess>]
type Union1 = Case1 | Case2
type Abbrev = Union1

let f union1 =
    match union1 with
    | Abbrev.Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 7 6)
    |> assertSrcAreEqual """
[<RequireQualifiedAccess>]
type Union1 = Case1 | Case2
type Abbrev = Union1

let f union1 =
    match union1 with
    | Abbrev.Case1 -> ()
    | Abbrev.Case2 -> failwith ""
"""

[<Test>]
let ``handle uppercase field names`` () =
    """
let _ =
    match None with
    | None -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 3 6)
    |> assertSrcAreEqual """
let _ =
    match None with
    | None -> ()
    | Some(value) -> failwith ""
"""

[<Test>]
let ``handle really similar field names`` () =
    """
type A = Case1 | Case2 of value2: int * Value: int * value: int * value1: int
let _ =
    match Case1 with
    | Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type A = Case1 | Case2 of value2: int * Value: int * value: int * value1: int
let _ =
    match Case1 with
    | Case1 -> ()
    | Case2(value2, value1, value3, value4) -> failwith ""
"""
    

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
        | Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 9 10)
    |> assertSrcAreEqual """
type Union1 = Case1 | Case2
type Union2 = Case3 * int | Case4 of Union1 | Case5

let f union2 =
    match union2 with
    | Case3 _ -> ()
    | Case4(union1) ->
        match union1 with
        | Case1 -> ()
        | Case2 -> failwith ""
"""

[<Test>]
let ``union match case generation is inactive on tuples`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1, Case2 -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 7))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation is inactive on lists`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | [Case1] -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 7))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation is inactive on list cons pattern`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1 :: [Case1] -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 7))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation is inactive on chained list cons pattern`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | Case1 :: x :: Case2 :: [Case1] -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 7))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match case generation is inactive on arrays`` () =
    """
type Union = Case1 | Case2

let f u =
    match u with
    | [|Case1|] -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 8))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``generate identifier names when a union field has one`` () =
    """
type Union =
    | Case1
    | Case2 of arg1:int * int * arg3:string
    | Case3 of arg1:int * arg2:string

let f x =
    match x with
    | Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 8 6)
    |> assertSrcAreEqual """
type Union =
    | Case1
    | Case2 of arg1:int * int * arg3:string
    | Case3 of arg1:int * arg2:string

let f x =
    match x with
    | Case1 -> ()
    | Case2(arg1, _, arg3) -> failwith ""
    | Case3(arg1, arg2) -> failwith ""
"""

[<Test>]
let ``correctly generates wildcards for arguments at position n >= 10`` () =
    """
type Union =
    | Case0
    | Case1 of int * int * int * int * int * int * int * int * int * int * int

let f x =
    match x with
    | Case0 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 7 6)
    |> assertSrcAreEqual """
type Union =
    | Case0
    | Case1 of int * int * int * int * int * int * int * int * int * int * int

let f x =
    match x with
    | Case0 -> ()
    | Case1(_, _, _, _, _, _, _, _, _, _, _) -> failwith ""
"""

[<Test>]
let ``doesn't trigger code generation for cases with argument count >= 10`` () =
    """
type Union =
    | Case0
    | Case1 of int * int * int * int * int * int * int * int * int * int * int

let f x =
    match x with
    | Case0 -> ()
    | Case1(_, _, _, _, _, _, _, _, _, _, _) -> failwith ""
"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 8 6))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``union match lambda case generation when first clause starts with '|'`` () =
    """
type Union = Case1 | Case2

let f = function
    | Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f = function
    | Case1 -> ()
    | Case2 -> failwith ""
"""

[<Test>]
let ``union match lambda case generation when first clause doesn't start with '|'`` () =
    """
type Union = Case1 | Case2

let f = function
    Case1 -> ()
"""
    |> insertCasesFromPos (Pos.fromZ 4 6)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f = function
    Case1 -> ()
    | Case2 -> failwith ""
"""

[<Test>]
let ``uses correct indenting when inserting new line when first pattern starts on a new line`` () =
    [
        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with
    | Case1 -> () | Case2 -> ()
"""

        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with
    | Case1 -> ()
         |
     Case2 -> ()
"""

        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with
    | Case1 -> ()
         | Case2 -> ()
"""
    ]
    |> List.map (insertCasesFromPos (Pos.fromZ 4 6))
    |> assertSrcSeqAreEqual [
        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with
    | Case1 -> () | Case2 -> ()
    | Case3 -> failwith ""
"""

        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with
    | Case1 -> ()
         |
     Case2 -> ()
     | Case3 -> failwith ""
"""

        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with
    | Case1 -> ()
         | Case2 -> ()
         | Case3 -> failwith ""
"""
    ]

[<Test>]
let ``uses correct indenting when inserting new line when first pattern starts on the same line`` () =
    [
        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with | Case1 -> () | Case2 -> ()
"""

        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with Case1 -> () | Case2 -> ()
"""
    ]
    |> List.map (insertCasesFromPos (Pos.fromZ 3 19))
    |> assertSrcSeqAreEqual [
        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with | Case1 -> () | Case2 -> ()
                 | Case3 -> failwith ""
"""

        """
type Union = Case1 | Case2 | Case3
let f x =
    match x with Case1 -> () | Case2 -> ()
                 | Case3 -> failwith ""
"""
]

[<Test>]
let ``generation is not triggered when caret on union type identifier but not on pattern match clause`` () =
    """
[<RequireQualifiedAccess>]
type Union = Case1 | Case2

let f x =
    match x with
    | Some 0 -> Union.Case2"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 6 16))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``generation is not triggered when caret on union case identifier but not on pattern match clause`` () =
    """
type Union = Case1 | Case2

let f x =
    match x with
    | Some 0 -> Case2"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 5 16))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``generation is not triggered when caret on nested patterns of a pattern match clause`` () =
    """
type A = A1 | A2
type B = B1 | B2 of A
let f x =
    match x with
    | B1 -> ()
    | B2 (A1 | A2) -> ()"""
    |> getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 6 12))
    |> assertSrcWasNotChangedAfterCodeGen

[<Test>]
let ``code gen isn't triggered when last pattern match clause is incomplete`` () =
    [
        """type Union = Case1 | Case2
let f =
    match x with
    Case1 -> """

        """type Union = Case1 | Case2
let f =
    match x with
    Case1"""

        """type Union = Case1 | Case2
let f =
    function
    Case1 -> """

        """type Union = Case1 | Case2
let f =
    function
    Case1"""
    ]
    |> List.map (getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 2 7)))
    |> List.iter (assertSrcWasNotChangedAfterCodeGen)


[<Test>]
let ``code gen isn't triggered when last pattern match clause is incomplete but is followed by code`` () =
    [
        """type Union = Case1 | Case2
let f =
    match x with
    Case1 ->
    
let y = 3"""

        """type Union = Case1 | Case2
let f =
    match x with
    Case1

let y = 3"""

        """type Union = Case1 | Case2
let f =
    match x with
    Case1 ->

    let y = 3"""

        """type Union = Case1 | Case2
let f =
    match x with
    Case1

    let y = 3"""

        """type Union = Case1 | Case2
let f =
    match x with
    Case1 ->

    let y = 3
    y"""

        """type Union = Case1 | Case2
let f =
    match x with
    Case1

    let y = 3
    y"""
    ]
    |> List.map (getSrcBeforeAndAfterCodeGen (insertCasesFromPos (Pos.fromZ 2 7)))
    |> List.iter (assertSrcWasNotChangedAfterCodeGen)
