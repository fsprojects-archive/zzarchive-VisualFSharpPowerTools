#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
      "../../src/FSharpVSPowerTools.Core/UnionPatternMatchCaseGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharpVSPowerTools.Core.Tests.UnionPatternMatchCaseGeneratorTests
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
open FSharpVSPowerTools.CodeGeneration.UnionPatternMatchCaseGenerator
open FSharpVSPowerTools.Core.Tests.CodeGenerationTestInfrastructure
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
let project() =
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

let tryFindUnionDefinition codeGenService (pos: pos) (document: IDocument) =
    tryFindUnionDefinitionFromPos codeGenService (project()) pos document
    |> Async.RunSynchronously

let insertCasesFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)
    let unionTypeDefFromPos = tryFindUnionDefinition codeGenService caretPos document
    match unionTypeDefFromPos with
    | None -> src
    | Some(range, matchExpr, entity, insertionParams) ->
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
    let tryFindPatternMatchExpr codeGenService (pos: pos) (document: IDocument) =
        tryFindPatternMatchExprInBufferAtPos codeGenService (project()) pos document
        |> Async.RunSynchronously


    let private tryGetWrittenCases (pos: pos) (src: string) =
        let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(LanguageService(fun _ -> ()), args)
        src
        |> asDocument
        |> tryFindPatternMatchExpr codeGenService pos
        |> Option.map (getWrittenCases)
        |> Option.getOrElse Set.empty

//    [<Test>]
//    let ``OR patterns with constants and identifiers`` () =
//        """type Union = Case1 | Case2 | Case3 of bool | Case4 of int * int
//let f union = match union with
//    | Case3 true
//    | Case3(i)
//    | Case4(3, _) -> ()"""
//        |> tryGetWrittenCases (Pos.fromZ 2 6)
//        |> assertEqual (set ["Case3"])
//
//    [<Test>]
//    let ``OR patterns with wildcards and qualified identifiers`` () =
//        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
//let f union = match union with
//    | Case2 | Union.Case2 | Case4(_,_)
//    | Case3 _
//    | Union.Case4 _ -> ()"""
//        |> tryGetWrittenCases (Pos.fromZ 2 6)
//        |> assertEqual (set ["Case2"; "Case3"; "Case4"])
//
//    [<Test>]
//    let ``redundant simple AND pattern`` () =
//        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
//let f union = match union with
//    | Case2 & Case2 -> ()"""
//        |> tryGetWrittenCases (Pos.fromZ 2 6)
//        |> assertEqual (set ["Case2"])
//
//    [<Test>]
//    let ``AND pattern with wildcards`` () =
//        """type Union = Case1 | Case2 | Case3 of int | Case4 of int * int
//let f union = match union with
//    | Case2 & Case4(_,_) -> ()"""
//        |> tryGetWrittenCases (Pos.fromZ 2 6)
//        |> assertEqual (set [])


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
let ``uses correct indenting when inserting new line, when first pattern starts on a new line`` () =
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
let ``uses correct indenting when inserting new line, when first pattern starts on the same line`` () =
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
