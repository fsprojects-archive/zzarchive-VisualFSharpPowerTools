namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

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

type CategorizedColumnSpan<'T> =
    { Category: Category
      WordSpan: WordSpan
      /// Snapshot for which the span was created.
      /// None if the right Snapshot is maintained separatly
      Snapshot: 'T option }

module private QuotationCategorizer =
    let private categorize (lexer: LexerBase) ranges =
        let trimWhitespaces = 
            Seq.skipWhile (fun t -> t.CharClass = FSharpTokenCharKind.WhiteSpace) >> Seq.toList

        ranges
        |> Seq.map (fun (r: Range.range) -> 
            if r.EndLine = r.StartLine then
                seq [ { Category = Category.Quotation
                        WordSpan = { SymbolKind = SymbolKind.Other
                                     Line = r.StartLine
                                     StartCol = r.StartColumn
                                     EndCol = r.EndColumn }
                        Snapshot = None } ]
            else
                [r.StartLine..r.EndLine]
                |> Seq.choose (fun line ->
                     let tokens = lexer.TokenizeLine (line - 1)

                     let tokens =
                        match tokens |> List.tryFind (fun t -> t.TokenName = "RQUOTE") with
                        | Some rquote -> 
                            tokens
                            |> List.rev
                            |> Seq.skipWhile ((<>) rquote)
                            |> Seq.toList
                            |> List.rev
                        | _ -> 
                            match tokens |> List.tryFind (fun t -> t.TokenName = "LQUOTE") with
                            | Some lquote -> tokens |> Seq.skipWhile (fun t -> t <> lquote) |> Seq.toList
                            | _ -> tokens 

                     let tokens = tokens |> trimWhitespaces |> List.rev |> trimWhitespaces |> List.rev
                     
                     match tokens with
                     | [] -> None
                     | _ ->
                        let minCol = tokens |> List.map (fun t -> t.LeftColumn) |> function [] -> 0 | xs -> xs |> List.min
                 
                        let maxCol = 
                            let tok = tokens |> List.maxBy (fun t -> t.RightColumn) 
                            tok.LeftColumn + tok.FullMatchedLength

                        Some { Category = Category.Quotation
                               WordSpan = { SymbolKind = SymbolKind.Other
                                            Line = line
                                            StartCol = minCol
                                            EndCol = maxCol }
                               Snapshot = None }))
        |> Seq.concat

    let getCategories ast lexer = UntypedAstUtils.getQuatationRanges ast |> categorize lexer

module private StringCategorizers =
    open System.Text.RegularExpressions

    let categorize category (regex: Regex) (getTextLine: int -> string) (r: Range.range) =
        let lines =
            [r.StartLine..r.EndLine]
            |> List.map (fun line ->
                let lineStr = getTextLine (line - 1)
                if line = r.StartLine && line = r.EndLine && r.StartColumn + 1 <= r.EndColumn - 1 then
                    lineStr.[r.StartColumn + 1 .. r.EndColumn - 1], line, r.StartColumn + 1
                elif line = r.StartLine then 
                    lineStr.[r.StartColumn + 1 ..], line, r.StartColumn + 1
                elif line = r.EndLine then
                    lineStr.[..r.EndColumn - 1], line, 0
                else lineStr, line, 0)

        lines
        |> List.map (fun (str, line, startColumn) ->
             regex.Matches str 
             |> Seq.cast<Match> 
             |> Seq.filter (fun m -> m.Value <> "")
             |> Seq.fold (fun acc (m: Match) -> 
                  let category =
                      { Category = category
                        WordSpan = 
                          { SymbolKind = SymbolKind.Other
                            Line = line
                            StartCol = startColumn + m.Index
                            EndCol = startColumn + m.Index + m.Length }
                        Snapshot = None }
                  category :: acc  
                ) [])
        |> Seq.concat 
         
    module EscapedChars =
        let private escapingSymbolsRegex = Regex """\\(n|r|t|b|\\|"|'|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})"""

        let private isRegularString (r: Range.range) getTextLine =
            let lineStr: string = getTextLine (r.StartLine - 1) 
            let origLiteral = lineStr.Substring r.StartColumn
            not (origLiteral.StartsWith "\"\"\"" || origLiteral.StartsWith "@")

        (* VisualStudio return the following strings (presented here as char arrays):
           "a\"b" => [|'"'; 'a'; '\\'; '"'; 'b'; '"'|]
           "a\rb" => [|'"'; 'a'; '\\'; 'r'; 'b'; '"'|] *)

        let getCategories ast getTextLine = 
            UntypedAstUtils.getStringLiterals ast 
            |> List.filter (fun r -> isRegularString r getTextLine)
            |> List.map (categorize Category.Escaped escapingSymbolsRegex getTextLine) 
            |> Seq.concat

module private OperatorCategorizer = 
    let getSpans (symbolUses: (SymbolUse * WordSpan) []) (spansByLine: Map<int, seq<WordSpan>>) tokensByLine =
        let spansBasedOnSymbolUse =
            symbolUses
            |> Array.choose (fun (_, span) -> 
                if span.SymbolKind = SymbolKind.Operator then
                    Some { Category = Category.Operator
                           WordSpan = span
                           Snapshot = None }
                else None)

        let operatorSpansByLine = 
            spansBasedOnSymbolUse 
            |> Seq.map (fun span -> span.WordSpan)
            |> Seq.groupBy (fun s -> s.Line) 
            |> Map.ofSeq

        //System.Diagnostics.Debugger.Launch() |> ignore
        let spansBasedOnLexer =
            tokensByLine |> Array.foldi (fun (acc: ResizeArray<_>) line tokens -> 
                let operatorTokens = 
                    tokens
                    |> List.choose (fun t -> 
                        match t.TokenName with 
                        | "EQUALS" -> Some (t.LeftColumn, t.RightColumn + 1)
                        | _ -> None)
                    |> List.filter (fun (lCol, rCol) ->
                        let spans =
                            match operatorSpansByLine |> Map.tryFind (line + 1) with
                            | None -> spansByLine |> Map.tryFind (line + 1)
                            | x -> x
                        match spans with
                        | Some spans -> 
                            spans |> Seq.exists (fun s -> 
                                (lCol < s.StartCol && rCol < s.StartCol) || 
                                (lCol > s.EndCol && rCol > s.EndCol))
                        | None -> true)
                    |> List.map (fun (lCol, rCol) ->
                        { Category = Category.Operator
                          WordSpan = 
                            { SymbolKind = SymbolKind.Operator
                              Line = line + 1
                              StartCol = lCol
                              EndCol = rCol }
                          Snapshot = None })
                acc.AddRange(operatorTokens)
                acc) (ResizeArray())

        Array.append spansBasedOnSymbolUse (spansBasedOnLexer.ToArray()) |> Seq.distinct |> Seq.toArray

module SourceCodeClassifier =
    let getIdentifierCategory = function
        | Entity (_, ValueType, _) -> Category.ValueType
        | Entity Class -> Category.ReferenceType
        | Entity (_, FSharpModule, _) -> Category.Module
        | Entity (_, _, Tuple) -> Category.ReferenceType
        | Entity (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType    
        | _ -> Category.Other 

    let internal getCategory (symbolUse: FSharpSymbolUse) =
        match symbolUse.Symbol with
        | Field (MutableVar, _)
        | Field (_, RefCell) -> Category.MutableVar
        | Pattern -> Category.PatternCase
        | Entity (_, ValueType, _) -> Category.ValueType
        | Entity Class -> Category.ReferenceType
        | Entity (_, FSharpModule, _) -> Category.Module
        | Entity (_, _, Tuple) -> Category.ReferenceType
        | Entity (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType
        | MemberFunctionOrValue (Constructor ValueType) -> Category.ValueType
        | MemberFunctionOrValue (Constructor _) -> Category.ReferenceType
        | MemberFunctionOrValue (Function symbolUse.IsFromComputationExpression) -> Category.Function
        | MemberFunctionOrValue MutableVar -> Category.MutableVar
        | MemberFunctionOrValue func ->
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

    let getCategoriesAndLocations (allSymbolsUses: SymbolUse[], checkResults: ParseAndCheckResults, lexer: LexerBase, 
                                   getTextLine: int -> string, openDeclarations: OpenDeclaration list, allEntities: Map<string, Idents list> option) =

        let tokensByLine = lexer.TokenizeAll()

        let allSymbolsUses2 =
            allSymbolsUses
            |> Seq.groupBy (fun su -> su.SymbolUse.RangeAlternate.EndLine)
            |> Seq.map (fun (line, sus) ->
                let tokens = tokensByLine.[line - 1]
                sus |> Seq.choose (fun su ->
                    let r = su.SymbolUse.RangeAlternate
                    lexer.GetSymbolFromTokensAtLocation (tokens, line - 1, r.End.Column - 1) |> Option.bind (fun sym -> 
                        //printfn "#### su = %A, range = %A, sym.Kind = %A" (su.SymbolUse.Symbol.GetType()) r sym.Kind
                        match sym.Kind with
                        | SymbolKind.Ident ->
                            // FCS returns inaccurate ranges for multiline method chains
                            // Specifically, only the End is right. So we use the lexer to find Start for such symbols.
                            if r.StartLine < r.EndLine then
                                Some (su, { SymbolKind = sym.Kind
                                            Line = r.End.Line
                                            StartCol = r.End.Column - sym.Text.Length
                                            EndCol = r.End.Column })
                            else 
                                Some (su, { SymbolKind = sym.Kind
                                            Line = r.End.Line
                                            StartCol = r.Start.Column
                                            EndCol = r.End.Column })
                        | SymbolKind.Operator when sym.LeftColumn = r.StartColumn -> 
                            Some (su, { SymbolKind = sym.Kind
                                        Line = r.End.Line 
                                        StartCol = r.Start.Column
                                        EndCol = r.End.Column })
                        | _ -> None)))
            |> Seq.concat
            |> Seq.toArray
       
        // index all symbol usages by LineNumber 
        let wordSpansByLine = 
            allSymbolsUses2
            |> Seq.map (fun (_, span) -> span)
            |> Seq.groupBy (fun span -> span.Line)
            |> Seq.map (fun (line, ranges) -> line, ranges)
            |> Map.ofSeq

        let spansBasedOnSymbolsUses = 
            allSymbolsUses2
            |> Seq.choose (fun (symbolUse, span) ->
                let span = 
                    match wordSpansByLine.TryFind span.Line with
                    | Some spans -> spans |> Seq.fold (fun result span -> excludeWordSpan result span) span
                    | _ -> span

                let span' = 
                    if (span.EndCol - span.StartCol) - symbolUse.SymbolUse.Symbol.DisplayName.Length > 0 then
                        // The span is wider that the simbol's display name.
                        // This means that we have not managed to extract last part of a long ident accurately.
                        // Particulary, it happens for chained method calls like Guid.NewGuid().ToString("N").Substring(1).
                        // So we get ident from the lexer.
                        match lexer.GetSymbolAtLocation (span.Line - 1, span.EndCol - 1) with
                        | Some s -> 
                            match s.Kind with
                            | Ident -> 
                                // Lexer says that our span is too wide. Adjust it's left column.
                                if span.StartCol < s.LeftColumn then { span with StartCol = s.LeftColumn }
                                else span
                            | _ -> span
                        | _ -> span
                    else span

                let categorizedSpan =
                    if span'.EndCol <= span'.StartCol then None
                    else Some { Category = 
                                    if not symbolUse.IsUsed then Category.Unused 
                                    else getCategory symbolUse.SymbolUse
                                WordSpan = span' 
                                Snapshot = None }
        
                categorizedSpan)
            |> Seq.groupBy (fun span -> span.WordSpan)
            |> Seq.map (fun (_, spans) -> 
                    match List.ofSeq spans with
                    | [span] -> span
                    | spans -> 
                        spans 
                        |> List.sortBy (fun span -> 
                            match span.Category with
                            | Category.Other -> 3
                            | Category.Unused -> 2
                            | Category.Function -> 0 // we prefer Function to hide ReferenceType on some methods in signature files
                            | _ -> 1)
                        |> List.head)
            |> Seq.distinct

//        debug "LongIdents by line:"  
//        longIdentsByEndPos 
//        |> Seq.map (fun pair -> pair.Key.Line, pair.Key.Column, pair.Value) 
//        |> Seq.iter (debug "%A")

//        allEntities
//        |> Option.iter (fun es ->
//            es
//            |> Map.toSeq
//            |> Seq.map (fun (fullName, idents) -> sprintf "%s %A" fullName idents)
//            |> fun lines -> System.IO.File.WriteAllLines (@"L:\temp\_entities_.txt", lines))

//        let printSymbolUses msg (symbolUses: SymbolUse[]) =
//            debug "[SourceCodeClassifier] %s SymbolUses:\n" msg
//            for sUse in symbolUses do
//                let r = sUse.SymbolUse.RangeAlternate
//                debug "%A (%d, %d) -- (%d, %d)" sUse.FullNames r.StartLine r.StartColumn r.EndLine r.EndColumn
//            symbolUses

        let ast = checkResults.GetUntypedAst()

        let unusedOpenDeclarationSpans =
            OpenDeclarationGetter.getUnusedOpenDeclarations ast allSymbolsUses openDeclarations allEntities
            |> List.map (fun decl -> 
                { Category = Category.Unused
                  WordSpan = { SymbolKind = SymbolKind.Other
                               Line = decl.DeclarationRange.StartLine 
                               StartCol = decl.DeclarationRange.StartColumn
                               EndCol = decl.DeclarationRange.EndColumn }
                  Snapshot = None })
    
        let printfSpecifiersRanges =        
            checkResults.GetFormatSpecifierLocations()
            |> Option.map (fun ranges ->
                 ranges |> Array.map (fun r -> 
                    { Snapshot = None
                      Category = Category.Printf
                      WordSpan = 
                        { SymbolKind = SymbolKind.Other
                          Line = r.StartLine
                          StartCol = r.StartColumn
                          EndCol = r.EndColumn + 1 }}))
            |> Option.getOrElse [||]

        let allSpans = 
            spansBasedOnSymbolsUses 
            |> Seq.append (QuotationCategorizer.getCategories ast lexer)
            |> Seq.append printfSpecifiersRanges
            |> Seq.append (StringCategorizers.EscapedChars.getCategories ast getTextLine)
            |> Seq.append unusedOpenDeclarationSpans
            |> Seq.append (OperatorCategorizer.getSpans allSymbolsUses2 wordSpansByLine tokensByLine)
            |> Seq.toArray

    //    for span in allSpans do
    //       debug "-=O=- %A" span
        allSpans