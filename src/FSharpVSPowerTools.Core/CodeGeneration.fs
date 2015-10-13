﻿namespace FSharpVSPowerTools.CodeGeneration

open System
open System.IO
open System.CodeDom.Compiler
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<Measure>] type Line0
[<Measure>] type Line1

type IDocument =
    abstract FullName: string
    abstract GetText: unit -> string
    abstract GetLineText0: int<Line0> -> string
    abstract GetLineText1: int<Line1> -> string
    abstract LineCount: int 

type IRange =
    abstract StartLine: int<Line1>
    abstract StartColumn: int
    abstract EndLine: int<Line1>
    abstract EndColumn: int

type ICodeGenerationService<'Project, 'Pos, 'Range> =
    abstract TokenizeLine: 'Project * IDocument * int<Line1> -> FSharpTokenInfo list
    abstract GetSymbolAtPosition: 'Project * IDocument * pos:'Pos -> option<'Range * Symbol>
    abstract GetSymbolAndUseAtPositionOfKind: 'Project * IDocument * 'Pos * SymbolKind -> Async<option<'Range * Symbol * FSharpSymbolUse>>
    abstract ParseFileInProject: IDocument * 'Project -> Async<FSharpParseFileResults>
    // TODO: enhance this clumsy design
    abstract ExtractFSharpPos: 'Pos -> pos


[<AutoOpen>]
module internal Utils =
    open Microsoft.FSharp.Compiler.SourceCodeServices.PrettyNaming

    type ColumnIndentedTextWriter() =
        let stringWriter = new StringWriter()
        let indentWriter = new IndentedTextWriter(stringWriter, " ")

        member __.Write(s: string) =
            indentWriter.Write("{0}", s)

        member __.Write(s: string, [<ParamArray>] objs: obj []) =
            indentWriter.Write(s, objs)

        member __.WriteLine(s: string) =
            indentWriter.WriteLine("{0}", s)

        member __.WriteLine(s: string, [<ParamArray>] objs: obj []) =
            indentWriter.WriteLine(s, objs)

        member x.WriteBlankLines count =
            for _ in 0 .. count - 1 do
                x.WriteLine ""

        member __.Indent i = 
            indentWriter.Indent <- indentWriter.Indent + i

        member __.Unindent i = 
            indentWriter.Indent <- max 0 (indentWriter.Indent - i)

        member __.Dump() =
            indentWriter.InnerWriter.ToString()

        interface IDisposable with
            member __.Dispose() =
                stringWriter.Dispose()
                indentWriter.Dispose()

    let (|IndexerArg|) = function
        | SynIndexerArg.Two(e1, e2) -> [e1; e2]
        | SynIndexerArg.One e -> [e]

    let (|IndexerArgList|) xs =
        List.collect (|IndexerArg|) xs

    let hasAttribute<'T> (attrs: seq<FSharpAttribute>) =
        attrs |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'T>.Name)

    let tryFindTokenLPosInRange
        (codeGenService: ICodeGenerationService<'Project, 'Pos, 'Range>) project
        (range: range) (document: IDocument) (predicate: FSharpTokenInfo -> bool) =
        // Normalize range
        // NOTE: FCS compiler sometimes returns an invalid range. In particular, the
        // range end limit can exceed the end limit of the document
        let range =
            if range.EndLine > document.LineCount then
                let newEndLine = document.LineCount
                let newEndColumn = document.GetLineText1(document.LineCount * 1<Line1>).Length
                let newEndPos = mkPos newEndLine newEndColumn
                
                mkFileIndexRange (range.FileIndex) range.Start newEndPos
            else
                range
            
        let lineIdxAndTokenSeq = seq {
            for lineIdx = range.StartLine to range.EndLine do
                yield!
                    codeGenService.TokenizeLine(project, document, (lineIdx * 1<Line1>))
                    |> List.map (fun tokenInfo -> lineIdx * 1<Line1>, tokenInfo)
        }

        lineIdxAndTokenSeq
        |> Seq.tryFind (fun (line1, tokenInfo) ->
            if range.StartLine = range.EndLine then
                tokenInfo.LeftColumn >= range.StartColumn &&
                tokenInfo.RightColumn < range.EndColumn &&
                predicate tokenInfo
            elif range.StartLine = int line1 then
                tokenInfo.LeftColumn >= range.StartColumn &&
                predicate tokenInfo
            elif int line1 = range.EndLine then
                tokenInfo.RightColumn < range.EndColumn &&
                predicate tokenInfo
            else
                predicate tokenInfo
        )
        |> Option.map (fun (line1, tokenInfo) ->
            tokenInfo, (Pos.fromZ (int line1 - 1) tokenInfo.LeftColumn)
        )

    /// Represent environment where a captured identifier should be renamed
    type NamesWithIndices = Map<string, Set<int>>

    let keywordSet = set KeywordNames

    /// Rename a given argument if the identifier has been used
    let normalizeArgName (namesWithIndices: NamesWithIndices) nm =
        match nm with
        | "()" -> nm, namesWithIndices
        | _ ->
            let nm = String.lowerCaseFirstChar nm
            let nm, index = String.extractTrailingIndex nm
                
            let index, namesWithIndices =
                match namesWithIndices |> Map.tryFind nm, index with
                | Some indexes, index ->
                    let rec getAvailableIndex idx =
                        if indexes |> Set.contains idx then 
                            getAvailableIndex (idx + 1)
                        else idx
                    let index = index |> Option.getOrElse 1 |> getAvailableIndex
                    Some index, namesWithIndices |> Map.add nm (indexes |> Set.add index)
                | None, Some index -> Some index, namesWithIndices |> Map.add nm (Set.ofList [index])
                | None, None -> None, namesWithIndices |> Map.add nm Set.empty

            let nm = 
                match index with
                | Some index -> sprintf "%s%d" nm index
                | None -> nm
                
            let nm = if Set.contains nm keywordSet then sprintf "``%s``" nm else nm
            nm, namesWithIndices