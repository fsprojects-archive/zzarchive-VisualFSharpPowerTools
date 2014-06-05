namespace FSharpVSPowerTools.Core.Tests

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

[<AutoOpen>]
module PosHelpers =
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

type CodeGenerationTestService(languageService: LanguageService, compilerOptions: string[]) =
    interface ICodeGenerationService<ProjectOptions, pos, Range> with
        member x.CreateIRange(range: Range) = range :> IRange
        
        member x.TokenizeLine(_project, document: IDocument, line1: int<Line1>): TokenInformation list = 
                let line0 = int line1 - 1 
                let line = document.GetLineText1(line1)
                Lexer.tokenizeLine (document.GetText()) compilerOptions line0 line Lexer.queryLexState
        
        member x.GetSymbolAtPosition(_project, snapshot, pos) =
            let lineText = snapshot.GetLineText0 pos.Line0
            let src = snapshot.GetText()
            maybe {
                let! symbol =
                    Lexer.getSymbol src (int pos.Line0) (int pos.Column0) lineText compilerOptions Lexer.queryLexState

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