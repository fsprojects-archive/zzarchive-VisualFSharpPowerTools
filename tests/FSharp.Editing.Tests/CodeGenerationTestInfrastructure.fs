module FSharp.Editing.Tests.CodeGenerationTestInfrastructure

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing.Features
open FSharp.Editing
open FSharp.Editing.AsyncMaybe

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
        let range = symbol.Range
        { StartLine = LanguagePrimitives.Int32WithMeasure range.Start.Line
          StartColumn = range.Start.Column
          EndLine = LanguagePrimitives.Int32WithMeasure range.End.Line
          EndColumn = range.End.Column }

type CodeGenerationTestService(languageService: LanguageService, compilerOptions: string[]) =
    interface ICodeGenerationService<FSharpProjectOptions, pos, Range> with
        member __.TokenizeLine(_project, document: IDocument, line1: int<Line1>): FSharpTokenInfo list option = 
                let line0 = int line1 - 1 
                let line = document.GetLineText1(line1)
                Lexer.tokenizeLine (document.GetText()) compilerOptions line0 line Lexer.queryLexState |> Some
        
        member __.GetSymbolAtPosition(_project, snapshot, pos) =
            let lineText = snapshot.GetLineText0 pos.Line0
            let src = snapshot.GetText()
            maybe {
                let! symbol = 
                    Lexer.getSymbol src (int pos.Line0) (int pos.Column0) lineText SymbolLookupKind.Fuzzy 
                                    compilerOptions Lexer.queryLexState
                return Range.FromSymbol symbol, symbol
            } 

        member x.GetSymbolAndUseAtPositionOfKind(project, snapshot, pos, kind) =
            asyncMaybe {
                let x = x :> ICodeGenerationService<_, _, _>
                let! range, symbol = x.GetSymbolAtPosition(project, snapshot, pos)
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
                | _ -> return! None
            }

        member __.ParseFileInProject(snapshot, projectOptions) =
            languageService.ParseFileInProject(projectOptions, snapshot.FullName, snapshot.GetText()) |> AsyncMaybe.liftAsync

        member __.ExtractFSharpPos(pos) = pos


type MockDocument(src: string) =
    let lines =
        ResizeArray<_>(String.getLines src)

    interface IDocument with
        member __.FullName = "/File.fs"
        member __.LineCount = lines.Count
        member __.GetText() = src
        member __.GetLineText0(line0: int<Line0>) = lines.[int line0]
        member __.GetLineText1(line1: int<Line1>) = lines.[int line1 - 1]


[<AutoOpen>]
module Helpers =
    let srcToLineArray (src: string) = 
        String.getLines src
        |> Array.map (fun line -> if line.Trim() = "" then "" else line)

    let assertSrcAreEqual expectedSrc actualSrc =
        Collection.assertEqual (srcToLineArray expectedSrc) (srcToLineArray actualSrc)

    let assertSrcAreEqualForFirstLines lineCount expectedSrc actualSrc =
        let firstLinesFromActualSrc =
            srcToLineArray actualSrc
            |> Seq.take lineCount
            |> Seq.toArray

        let firstLinesFromExpectedSrc =
            srcToLineArray expectedSrc
            |> Seq.take lineCount
            |> Seq.toArray

        Collection.assertEqual firstLinesFromExpectedSrc firstLinesFromActualSrc

    let assertSrcSeqAreEqual expectedSrcSeq actualSrcSeq =
        Seq.zip expectedSrcSeq actualSrcSeq
        |> Seq.iter (fun (expectedSrc, actualSrc) -> assertSrcAreEqual expectedSrc actualSrc)

    let asDocument (src: string) = MockDocument(src) :> IDocument

    let getSrcBeforeAndAfterCodeGen (generateCode: string -> string) (src: string) =
        src, generateCode src

    let assertSrcWasNotChangedAfterCodeGen (srcBefore, srcAfter) =
        assertSrcAreEqual srcBefore srcAfter

    let getSymbolAt languageService projectOptions args caretPos src =
        let document: IDocument = upcast MockDocument(src)
        let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)

        asyncMaybe {
            let! _range, symbolAtPos = codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
            let! _range, _symbol, symbolUse = 
                codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)

            return symbolUse.Symbol
        }
        |> Async.RunSynchronously
        |> Option.get

    let getDisplayContextAt languageService projectOptions args caretPos src =
        let document: IDocument = upcast MockDocument(src)
        let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)

        asyncMaybe {
            let! _range, symbolAtPos = codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
            let! _range, _symbol, symbolUse = 
                codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)

            return symbolUse.DisplayContext
        }
        |> Async.RunSynchronously
        |> Option.get
