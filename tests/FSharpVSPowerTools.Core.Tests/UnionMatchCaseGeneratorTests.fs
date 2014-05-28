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
#load "TestHelpers.fs"
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
    interface IRange with
        member x.EndColumn: int = x.EndColumn
        member x.EndLine: int<Line1> = x.EndLine
        member x.StartColumn: int = x.StartColumn
        member x.StartLine: int<Line1> = x.StartLine

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
        member x.CreateRange(startLine: int<Line1>, startColumn: int, endLine: int<Line1>, endColumn: int): Range = 
            { StartLine = startLine
              StartColumn = startColumn
              EndLine = endLine
              EndColumn = endColumn }
        
        member x.TokenizeLine(_project, document: IDocument, line1: int<Line1>): TokenInformation list = 
                let line0 = int line1 - 1 
                let line = document.GetLineText1(line1)
                Lexer.tokenizeLine (document.GetText()) args line0 line Lexer.queryLexState
        
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

let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService()

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

let tokenizeLine (document: IDocument) (lineIdx1: int<Line1>) =
    let lineIdx0 = int lineIdx1 - 1 
    let line = document.GetLineText1(lineIdx1)
    Lexer.tokenizeLine (document.GetText()) args (lineIdx0) line Lexer.queryLexState

let insertCasesFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let unionTypeDefFromPos = tryFindUnionTypeDefinition caretPos document
    match unionTypeDefFromPos with
    | None -> src
    | Some(range, matchExpr, entity, insertionParams) ->
//        let indentValue = getIndentValue matchExpr
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

// [ ] Handle case where first case doesn't start with '|'

// TODO: dedup from RecordStubGeneratorTests.fs
let assertSrcAreEqual expectedSrc actualSrc =
    Collection.assertEqual (srcToLineArray expectedSrc) (srcToLineArray actualSrc)



do
    """
type Union = Case1

let f union =
    match union with
    | Case1 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
//    |> (fun document ->
//        maybe {
//            let! range = tryGetRangeBetweenWithAndFirstClause (Pos.fromZ 5 6) document
//            return! tryFindTokenInRange
//                        range
//                        (fun tokenInfo -> tokenInfo.TokenName = "BAR")
//                        document
//        }
//    )
    |> ignore

[<Test>]
let ``single union match case generation when the unique case is written`` () =
    """
type Union = Case1

let f union =
    match union with
    | Case1 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 6)
    |> assertSrcAreEqual """
type Union = Case1

let f union =
    match union with
    | Case1 -> ()"""

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
    match union with
    Case1 -> ()
    | Case2 -> ()"""
    |> insertCasesFromPos (Pos.fromZ 5 4)
    |> assertSrcAreEqual """
type Union = Case1 | Case2

let f union =
    match union with
    Case1 -> ()
    | Case2 -> ()"""

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


[<Test; Ignore("Reactivate when capable of identifying combined clauses")>]
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

[<Test; Ignore("Reactivate when capable of identifying combined clauses with arguments")>]
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


// Union match case without argument patterns
//// SynPat.LongIdent(_longIdentWithDots, _identOption, _synVarTyplDecl, _synConstrArg, _synAccessOpt, _range
//// LongIdent(LongIdentWithDots[(Case2, rangeList: [])], identOption: null, typlDecl: null, constrArgs: Pats [], accessOpt: null, _range)
//expr.[0].RangeOfGuardAndRhs

// Union match case with argument patterns
//// SynPat.LongIdent(_longIdentWithDots, _identOption, _synVarTyplDecl, _synConstrArg, _synAccessOpt, _range
//// New: _synConstrArg
//// LongIdent(LongIdentWithDots[(Case3, rangeList: [])], identOption: null, typlDecl: null, constrArgs: Pats [Wild...], accessOpt: null, _range)
//expr.[1].Pattern