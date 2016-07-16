namespace FSharp.Editing.Features

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing

type WordSpan = 
    { SymbolKind: SymbolKind
      Line: int
      StartCol: int
      EndCol: int }
    static member inline Create (kind, line, startCol, endCol) =
        { SymbolKind = kind
          Line = line
          StartCol = startCol
          EndCol = endCol }
    static member inline Create (kind, r: Range.range) = 
        { SymbolKind = kind
          Line = r.StartLine
          StartCol = r.StartColumn 
          EndCol = r.EndColumn }
    member x.Range = lazy (x.Line, x.StartCol, x.Line, x.EndCol)

[<RequireQualifiedAccess>]
type Category =
    | ReferenceType
    | ValueType
    | PatternCase
    | Function
    | MutableVar
    | Quotation
    | Module
    | Unused
    | Printf
    | Escaped
    | Operator
    | Other
    override x.ToString() = sprintf "%A" x

type CategorizedSpan =
    { Category: Category
      WordSpan: WordSpan }

[<AbstractClass>]
type LexerBase() = 
    abstract GetSymbolFromTokensAtLocation: FSharpTokenInfo list * line: int * rightCol: int -> Symbol option
    abstract TokenizeLine: line: int -> FSharpTokenInfo list
    abstract LineCount: int
    member x.GetSymbolAtLocation (line: int, col: int) =
           x.GetSymbolFromTokensAtLocation (x.TokenizeLine line, line, col)
    member x.TokenizeAll() = [|0..x.LineCount-1|] |> Array.map x.TokenizeLine

module private QuotationCategorizer =
    let private categorize (lexer: LexerBase) (ranges: Range.range seq) =
        let trimWhitespaces = 
            List.skipWhile (fun t -> t.CharClass = FSharpTokenCharKind.WhiteSpace) 

        seq {
            for r in ranges do
                if r.EndLine = r.StartLine then
                    yield { Category = Category.Quotation
                            WordSpan = WordSpan.Create(SymbolKind.Other, r) }
                else
                    for line in r.StartLine..r.EndLine do
                        let tokens = lexer.TokenizeLine (line - 1) 
                
                        let tokens =
                            if line = r.StartLine then
                                tokens |> List.skipWhile (fun t -> t.LeftColumn < r.StartColumn)
                            elif line = r.EndLine then
                                tokens |> List.takeWhile (fun t -> t.RightColumn <= r.EndColumn)
                            else tokens
                
                        let tokens = tokens |> trimWhitespaces |> List.rev |> trimWhitespaces |> List.rev
                        
                        match tokens with
                        | [] -> ()
                        | _ ->
                           let minCol = tokens |> List.map (fun t -> t.LeftColumn) |> function [] -> 0 | xs -> xs |> List.min
                     
                           let maxCol = 
                               let tok = tokens |> List.maxBy (fun t -> t.RightColumn) 
                               tok.LeftColumn + tok.FullMatchedLength
                
                           yield { Category = Category.Quotation
                                   WordSpan = WordSpan.Create (SymbolKind.Other, line, minCol, maxCol) }
        }

    let getCategories ast lexer = UntypedAstUtils.getQuotationRanges ast |> categorize lexer

module private StringCategorizers =
    open System.Text.RegularExpressions

    let categorize category (regex: Regex) (getTextLine: int -> string) (r: Range.range) =
        let lines =
            [r.StartLine..r.EndLine]
            |> Seq.map (fun line ->
                let lineStr = getTextLine (line - 1)
                if line = r.StartLine && line = r.EndLine && r.StartColumn + 1 <= r.EndColumn - 1 && lineStr.Length > r.EndColumn - 1 then
                    lineStr.[r.StartColumn + 1 .. r.EndColumn - 1], line, r.StartColumn + 1
                elif line = r.StartLine && lineStr.Length > r.StartColumn + 1 then 
                    lineStr.[r.StartColumn + 1 ..], line, r.StartColumn + 1
                elif line = r.EndLine && lineStr.Length > r.EndColumn - 1 then
                    lineStr.[..r.EndColumn - 1], line, 0
                else lineStr, line, 0)

        lines
        |> Seq.collect (fun (str, line, startColumn) ->
             regex.Matches str 
             |> Seq.cast<Match> 
             |> Seq.fold (fun acc (m: Match) -> 
                if m.Value = "" then acc
                else
                  let category =
                      { Category = category
                        WordSpan = 
                          { SymbolKind = SymbolKind.Other
                            Line = line
                            StartCol = startColumn + m.Index
                            EndCol = startColumn + m.Index + m.Length }}
                  category :: acc
                ) [])
         
    module EscapedChars =
        let private escapingSymbolsRegex = Regex """\\(n|r|t|b|\\|"|'|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})"""

        let private isRegularString (r: Range.range) getTextLine =
            let lineStr: string = getTextLine (r.StartLine - 1)
            if lineStr.Length > r.StartColumn then
                let origLiteral = lineStr.Substring r.StartColumn
                not (origLiteral.StartsWith "\"\"\"" || origLiteral.StartsWith "@")
            else true

        (* VisualStudio return the following strings (presented here as char arrays):
           "a\"b" => [|'"'; 'a'; '\\'; '"'; 'b'; '"'|]
           "a\rb" => [|'"'; 'a'; '\\'; 'r'; 'b'; '"'|] *)

        let getCategories ast getTextLine = 
            UntypedAstUtils.getStringLiterals ast 
            |> List.filter (fun r -> isRegularString r getTextLine)
            |> Seq.collect (categorize Category.Escaped escapingSymbolsRegex getTextLine) 
                                              
module private OperatorCategorizer = 
    let getSpans (symbolUses: (SymbolUse * WordSpan) []) (spansByLine: Map<int, seq<WordSpan>>) tokensByLine =
        let spansBasedOnSymbolUse =
            symbolUses
            |> Array.choose (fun (_, span) -> 
                if span.SymbolKind = SymbolKind.Operator then
                    Some { Category = Category.Operator
                           WordSpan = span }
                else None)

        let spansBasedOnLexer =
            tokensByLine |> Array.foldi (fun (acc: ResizeArray<_>) line tokens -> 
                let operatorTokens = 
                    tokens
                    // pick only =, :> and :?> operators
                    |> Seq.choose (fun t -> 
                        if t.Tag = FSharpTokenTag.EQUALS 
                           || t.Tag = FSharpTokenTag.COLON_GREATER
                           || t.Tag = FSharpTokenTag.COLON_QMARK_GREATER 
                           || t.Tag = FSharpTokenTag.COLON_QMARK 
                        then Some (t.LeftColumn, t.RightColumn + 1)
                        else None)
                    // pick tokens which are not overlapped with any symbols
                    |> Seq.filter (fun (lCol, rCol) ->
                        match spansByLine |> Map.tryFind (line + 1) with
                        | Some spans -> 
                            spans 
                            |> Seq.exists (fun s -> 
                                (s.StartCol <= lCol && s.EndCol >= lCol) || 
                                (s.StartCol <= rCol && s.EndCol >= rCol))
                            |> not
                        | None -> true)
                    |> Seq.map (fun (lCol, rCol) ->
                        { Category = Category.Operator
                          WordSpan = 
                            { SymbolKind = SymbolKind.Operator
                              Line = line + 1
                              StartCol = lCol
                              EndCol = rCol }})
                acc.AddRange(operatorTokens)
                acc) (ResizeArray())

        Array.append spansBasedOnSymbolUse (spansBasedOnLexer.ToArray()) |> Array.distinct 

module SourceCodeClassifier =
    open System.Collections.Generic

    let getIdentifierCategory = function
        | FSharpEntity e ->
            match e with
            | _, ValueType, _ -> Category.ValueType
            | Class -> Category.ReferenceType
            | _, FSharpModule, _ -> Category.Module 
            | _, _, Tuple -> Category.ReferenceType
            | _, (FSharpType | ProvidedType | ByRef | Array), _ -> Category.ReferenceType
            | _ -> Category.Other 
        | _ -> Category.Other 

    let internal getCategory (symbolUse: FSharpSymbolUse) =
        match symbolUse.Symbol with
        | Field (MutableVar, _)
        | Field (_, RefCell) -> Category.MutableVar
        | Pattern -> Category.PatternCase
        | FSharpEntity e ->
            match e with
            | (_, ValueType, _) -> Category.ValueType
            | Class -> Category.ReferenceType
            | (_, FSharpModule, _) -> Category.Module
            | (_, _, Tuple) -> Category.ReferenceType
            | (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType
            | (_, _, Some FunctionType) -> Category.ReferenceType
            | _ -> Category.Other
        | MemberFunctionOrValue f ->
            match f with
            | Constructor ValueType -> Category.ValueType
            | Constructor _ -> Category.ReferenceType
            | Function symbolUse.IsFromComputationExpression -> Category.Function
            | MutableVar -> Category.MutableVar
            | func ->
                match func.FullTypeSafe with
                | Some RefCell -> Category.MutableVar
                | _ -> Category.Other
        | _ -> Category.Other 

    // If "what" span is entirely included in "from" span, then truncate "from" to the end of "what".
    // Example: for ReferenceType symbol "System.Diagnostics.DebuggerDisplay" there are "System", "Diagnostics" and "DebuggerDisplay"
    // plane symbols. After excluding "System", we get "Diagnostics.DebuggerDisplay",
    // after excluding "Diagnostics", we get "DebuggerDisplay" and we are done.
    let private excludeWordSpan from what =
        if what.EndCol < from.EndCol && what.EndCol > from.StartCol then
            { from with StartCol = what.EndCol + 1 } // the dot between parts
        else from

    let private attachWordSpans (tokensByLine: FSharpTokenInfo list []) (lexer: LexerBase) (symbolUses: SymbolUse seq) =
        symbolUses
        |> Seq.groupBy (fun su -> su.SymbolUse.RangeAlternate.EndLine)
        |> Seq.collect (fun (line, sus) ->
            let tokens = tokensByLine.[line - 1]
            sus 
            |> Seq.choose (fun su ->
                let fsSymbolUse = su.SymbolUse
                let r = fsSymbolUse.RangeAlternate
                match fsSymbolUse.Symbol with
                | MemberFunctionOrValue func when func.IsActivePattern && not fsSymbolUse.IsFromDefinition ->
                    // Usage of Active Patterns '(|A|_|)' has the range of '|A|_|',
                    // so we expand the ranges in order to include parentheses into the results
                    Some (su, WordSpan.Create (SymbolKind.Ident, r.End.Line, r.Start.Column - 1, r.End.Column + 1))
                | _ ->
                    lexer.GetSymbolFromTokensAtLocation (tokens, line - 1, r.End.Column - 1) 
                    |> Option.bind (fun sym ->
                        match sym.Kind with
                        | SymbolKind.Ident ->
                            // FCS returns inaccurate ranges for multi-line method chains
                            // Specifically, only the End is right. So we use the lexer to find Start for such symbols.
                            if r.StartLine < r.EndLine then
                                Some (su, WordSpan.Create (sym.Kind, r.End.Line, r.End.Column - sym.Text.Length, r.End.Column))
                            else 
                                Some (su, WordSpan.Create (sym.Kind, r.End.Line, r.Start.Column, r.End.Column))
                        | SymbolKind.Operator when sym.LeftColumn = r.StartColumn -> 
                            Some (su, WordSpan.Create (sym.Kind, r))
                        | _ -> None)))
        |> Seq.toArray

    let private getCategorizedColumnSpansBasedOnSymbolUse 
        (wordSpansByLine: Map<int, WordSpan seq>)
        (getCategory: FSharpSymbolUse -> Category)
        (lexer: LexerBase)
        (symbolUsesWithSpans: (SymbolUse * WordSpan) []) = 
        
        seq { 
            for symbolUse, span in symbolUsesWithSpans do
              let span = 
                  match wordSpansByLine.TryFind span.Line with
                  | Some spans -> spans |> Seq.fold excludeWordSpan span
                  | _ -> span
              
              let span' = 
                  if (span.EndCol - span.StartCol) - symbolUse.SymbolUse.Symbol.DisplayName.Length > 0 then
                      // The span is wider that the symbol's display name.
                     // This means that we have not managed to extract last part of a long ident accurately.
                     // Particularly, it happens for chained method calls like Guid.NewGuid().ToString("N").Substring(1).
                     // So we get ident from the lexer.
                      match lexer.GetSymbolAtLocation (span.Line - 1, span.EndCol - 1) with
                      | Some s when s.Kind = Ident && span.StartCol < s.LeftColumn -> // Lexer says that our span is too wide. Adjust it's left column.
                          { span with StartCol = s.LeftColumn }
                      | _ -> span
                  else span
              
              if span'.EndCol > span'.StartCol then
                  yield 
                    { Category = 
                        if not symbolUse.IsUsed then Category.Unused 
                        else getCategory symbolUse.SymbolUse
                      WordSpan = span' }
        }
        |> Seq.groupBy (fun span -> span.WordSpan)
        |> Seq.map (fun (_, spans) ->
                match List.ofSeq spans with
                | [span] -> span
                | spans -> 
                    spans 
                    |> List.minBy (fun span -> 
                        match span.Category with
                        | Category.Other -> 3
                        | Category.Unused -> 2
                        | Category.Function -> 0 // we prefer Function to hide ReferenceType on some methods in signature files
                        | _ -> 1))
        |> Seq.distinct

    /// Index all symbol usages by LineNumber.
    let private getWordSpansByLine (symbolUsesWithSpans: (SymbolUse * WordSpan) []) =
        symbolUsesWithSpans
        |> Seq.map (fun (_, span) -> span)
        |> Seq.groupBy (fun span -> span.Line)
        |> Map.ofSeq

    /// Returns `CategorizedColumnSpan`s for all symbols, except unused ones.
    let getCategorizedSpans
        (
            allSymbolsUses: SymbolUse[], 
            checkResults: ParseAndCheckResults, 
            lexer: LexerBase, 
            getTextLine: int -> string
        ) =

        let tokensByLine = lexer.TokenizeAll()
        
        let allSymbolsUsesWithSpans = 
            allSymbolsUses 
            |> Seq.filter (fun x -> x.IsUsed)
            |> attachWordSpans tokensByLine lexer

        let wordSpansByLine = getWordSpansByLine allSymbolsUsesWithSpans
        let spansBasedOnSymbolsUses = getCategorizedColumnSpansBasedOnSymbolUse wordSpansByLine getCategory lexer allSymbolsUsesWithSpans
        let ast = checkResults.ParseTree

        let printfSpecifiersRanges =
            checkResults.GetFormatSpecifierLocationsAndArity()
            |> Option.map (fun ranges ->
                 ranges |> Array.map (fun (r, _) -> 
                    { Category = Category.Printf
                      WordSpan = 
                        { SymbolKind = SymbolKind.Other
                          Line = r.StartLine
                          StartCol = r.StartColumn
                          EndCol = r.EndColumn + 1 }}))
            |> Option.getOrElse [||]

        spansBasedOnSymbolsUses
        |> Seq.append (QuotationCategorizer.getCategories ast lexer)
        |> Seq.append printfSpecifiersRanges
        |> Seq.append (StringCategorizers.EscapedChars.getCategories ast getTextLine)
        |> Seq.append (OperatorCategorizer.getSpans allSymbolsUsesWithSpans wordSpansByLine tokensByLine)
        |> Seq.toArray

    /// Returns `CategorizedColumnSpan`s for unused declarations and unused open statements.
    let getCategorizedSpansForUnusedSymbols
        (
            allSymbolsUses: SymbolUse[], 
            checkResults: ParseAndCheckResults, 
            lexer: LexerBase, 
            openDeclarations: OpenDeclaration list, 
            allEntities: Dictionary<string, Idents list> option
        ) =

        let allSymbolsUsesWithSpans =
            allSymbolsUses
            //|> Seq.filter (fun x -> not x.IsUsed)
            |> attachWordSpans (lexer.TokenizeAll()) lexer
       
        let spansBasedOnSymbolsUses = 
            getCategorizedColumnSpansBasedOnSymbolUse 
                (getWordSpansByLine allSymbolsUsesWithSpans) 
                getCategory
                lexer 
                allSymbolsUsesWithSpans
            // we cannot filter unused symbols earlier because we need to get all categories for
            // each span, then pick the most important one, otherwise some symbols would always be market
            // as unused (record, DU, class definitions and others).
            |> Seq.filter (fun x -> x.Category = Category.Unused)

        let unusedOpenDeclarationSpans =
            OpenDeclarationGetter.getUnusedOpenDeclarations checkResults.ParseTree allSymbolsUses openDeclarations allEntities
            |> Seq.map (fun decl -> 
                { Category = Category.Unused
                  WordSpan = 
                    { SymbolKind = SymbolKind.Other
                      Line = decl.DeclarationRange.StartLine 
                      StartCol = decl.DeclarationRange.StartColumn
                      EndCol = decl.DeclarationRange.EndColumn }})
    
        spansBasedOnSymbolsUses 
        |> Seq.append unusedOpenDeclarationSpans
        |> Seq.toArray