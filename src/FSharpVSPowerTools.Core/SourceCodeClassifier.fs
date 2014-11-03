namespace FSharpVSPowerTools

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open System.Collections.Generic

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
    | Other
    override x.ToString() = sprintf "%A" x

type CategorizedColumnSpan =
    { Category: Category
      WordSpan: WordSpan }

module private QuotationCategorizer =
    let private categorize (lexer: LexerBase) ranges =
        let trimWhitespaces = 
            Seq.skipWhile (fun t -> t.CharClass = FSharpTokenCharKind.WhiteSpace) >> Seq.toList

        ranges
        |> Seq.map (fun (r: Range.range) -> 
            if r.EndLine = r.StartLine then
                seq [ { Category = Category.Quotation
                        WordSpan = { Line = r.StartLine
                                     StartCol = r.StartColumn
                                     EndCol = r.EndColumn }} ]
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
                               WordSpan = { Line = line
                                            StartCol = minCol
                                            EndCol = maxCol }}))
        |> Seq.concat

    let getCategories ast lexer = UntypedAstUtils.getQuatationRanges ast |> categorize lexer

module private PrintfCategorizer =
    let private printfTerminators = 
        set [ 'b'; 'c'; 's'; 'd'; 'i'; 'u'; 'x'; 'X'; 'o'; 'e'; 'E'; 'f'; 'g'; 'G'; 'M'; 'O'; 'A'; 'a'; 't' ]
    let private printfModifiers = set [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '-'; '+'; ' ' ]
        
    let private categorize (getTextLine: int -> string) (ranges: Range.range) =
        let lines =
            [ranges.StartLine..ranges.EndLine]
            |> List.map (fun line ->
                let lineStr = getTextLine (line - 1)
                if line = ranges.StartLine then 
                    lineStr.Substring(ranges.StartColumn + 1), line, ranges.StartColumn + 1
                elif line = ranges.EndLine then
                    lineStr.Substring(0, ranges.EndColumn - 1), line, 0
                else lineStr, line, 0)
        lines
        |> List.map (fun (str, line, startColumn) ->
            let findLengthAndSkip i = 
                let rec findTerminator i = 
                    if str.Length <= i then 0 else
                    let c = str.[i]
                    if printfTerminators.Contains c then i + 1
                    elif printfModifiers.Contains c then findTerminator (i + 1)
                    else 0
                if str.[i] = '%' then 2, None else
                match findTerminator i with
                | 0  -> 1, None
                | i' -> (i' + 1 - i), Some (i' + 1 - i)

            let rec parseFormatter acc i =
                if i >= (str.Length - 1) then acc else
                match str.[i] with
                | '%' -> 
                    let skip, length = findLengthAndSkip (i + 1) 
                    match length with 
                    | Some length -> 
                        let hit = 
                            { Category = Category.Printf
                              WordSpan = 
                                { Line = line
                                  StartCol = startColumn + i
                                  EndCol = startColumn + i + length }} 
                        parseFormatter (hit :: acc) (i + skip)
                    | _ -> parseFormatter acc (i + skip)
                | _ -> parseFormatter acc (i + 1) 
            parseFormatter [] 0)
        |> Seq.concat 

    let getCategories ast getTextLine =
        let literals = UntypedAstUtils.getPrintfLiterals ast 
        literals
        |> List.map (categorize getTextLine) 
        |> Seq.concat

module private EscapedCharsCategorizer =
    open UntypedAstUtils
    open System.Text.RegularExpressions

    let private isRegularString (r: Range.range) getTextLine =
        let lineStr: string = getTextLine (r.StartLine - 1) 
        let origLiteral = lineStr.Substring r.StartColumn
        not (origLiteral.StartsWith "\"\"\"" || origLiteral.StartsWith "@")

    let escapingSymbolsRegex = Regex """\\(n|r|t|b|\\|"|'|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})"""

    (* "a\"b" => [|'"'; 'a'; '\\'; '"'; 'b'; '"'|]
       "a\rb" => [|'"'; 'a'; '\\'; 'r'; 'b'; '"'|] *)

    let private categorize (getTextLine: int -> string) (lit: StringLiteral) =
        if isRegularString lit.Range getTextLine then
            [lit.Range.StartLine..lit.Range.EndLine]
            |> List.map (fun line ->
                let lineStr = getTextLine (line - 1)
                let lineChars = lineStr.ToCharArray()
                debug "[EscapedCharsCatecorizer] line = %s, chars = %A" lineStr lineChars
                if line = lit.Range.StartLine then 
                    lineStr.Substring(lit.Range.StartColumn), line, lit.Range.StartColumn
                elif line = lit.Range.EndLine then
                    lineStr.Substring(0, lit.Range.EndColumn - 1), line, 0
                else lineStr, line, 0
            )
            |> List.map (fun (str, line, startColumn) ->
                 let matches = 
                    escapingSymbolsRegex.Matches str 
                    |> Seq.cast<Match> 
                    |> Seq.filter (fun m -> m.Value <> "")
                    |> Seq.toArray

                 debug "[Escaped] (line = %d, lineStr = %s) => Matches %A" 
                       line str (matches |> Array.map (fun m -> 
                           sprintf "(idx = %d, len = %d, value = %s, value chars = %A" 
                                   m.Index m.Length m.Value (m.Value.ToCharArray())))

                 matches
                 |> Seq.fold (fun (shift, acc) (m: Match) -> 
                      let category =
                          { Category = Category.Escaped
                            WordSpan = 
                              { Line = line
                                StartCol = shift + startColumn + m.Index
                                EndCol = shift + startColumn + m.Index + m.Length }}
                      shift, category :: acc  
                    ) (0, [])
                  |> snd
               )
            |> Seq.concat 
        else Seq.empty

    let getCategories ast getTextLine = 
        let literals = UntypedAstUtils.getStringLiterals ast 
        literals
        |> List.map (categorize getTextLine) 
        |> Seq.concat
        |> Seq.toArray

module SourceCodeClassifier =
    let getIdentifierCategory = function
        | Entity (_, ValueType, _) -> Category.ValueType
        | Entity Class -> Category.ReferenceType
        | Entity (_, FSharpModule, _) -> Category.Module
        | Entity (_, _, Tuple) -> Category.ReferenceType
        | Entity (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType    
        | _ -> Category.Other 

    // Filter out symbols which ranges are fully included into a bigger symbols. 
    // For example, for this code: Ns.Module.Module1.Type.NestedType.Method() FCS returns the following symbols: 
    // Ns, Module, Module1, Type, NestedType, Method.
    // We want to filter all of them but the longest one (Method).
    let filterNestedSymbolUses (longIdents: IDictionary<_,_>) symbolUses =
        symbolUses
        |> Array.map (fun sUse ->
            match longIdents.TryGetValue sUse.SymbolUse.RangeAlternate.End with
            | true, longIdent -> sUse, Some longIdent
            | _ -> sUse, None)
        |> Seq.groupBy (fun (_, longIdent) -> longIdent)
        |> Seq.map (fun (longIdent, sUses) -> longIdent, sUses |> Seq.map fst)
        |> Seq.map (fun (longIdent, symbolUses) ->
            match longIdent with
            | Some _ -> 
                (* Find all longest SymbolUses which has unique roots. For example:
                           
                    module Top
                    module M =
                        type System.IO.Path with
                            member static ExtensionMethod() = ()

                    open M
                    open System
                    let _ = IO.Path.ExtensionMethod()

                    The last line contains three SymbolUses: "System.IO", "System.IO.Path" and "Top.M.ExtensionMethod". 
                    So, we filter out "System.IO" since it's a prefix of "System.IO.Path".

                *)
                let res =
                    symbolUses
                    |> Seq.sortBy (fun symbolUse -> -symbolUse.SymbolUse.RangeAlternate.EndColumn)
                    |> Seq.fold (fun (prev, acc) next ->
                         match prev with
                         | Some prev -> 
                            if prev.FullNames
                               |> Array.exists (fun prevFullName ->
                                    next.FullNames
                                    |> Array.exists (fun nextFullName ->
                                         nextFullName.Length < prevFullName.Length
                                         && prevFullName |> Array.startsWith nextFullName)) then 
                                Some prev, acc
                            else Some next, next :: acc
                         | None -> Some next, next :: acc)
                       (None, [])
                    |> snd
                    |> List.rev
                    |> List.toSeq
                res
            | None -> symbolUses)
        |> Seq.concat
        |> Seq.toArray

    let getSymbolUsesPotentiallyRequireOpenDecls symbolsUses =
        symbolsUses
        |> Array.filter (fun symbolUse ->
            let res = 
                match symbolUse.SymbolUse.Symbol with
                | Pattern | RecordField _
                | Entity (Class | (AbbreviatedType _ | FSharpType | ValueType | FSharpModule | Array), _, _)
                | Entity (_, _, Tuple)
                | MemberFunctionOrValue (Constructor _ | ExtensionMember) -> true
                | MemberFunctionOrValue func -> not func.IsMember
                | _ -> false
            res)
        //|> Array.map (fun (symbolUse, _) -> symbolUse)

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
    let excludeWordSpan from what =
        if what.EndCol < from.EndCol && what.EndCol > from.StartCol then
            { from with StartCol = what.EndCol + 1 } // the dot between parts
        else from


    let getFullPrefix (longIdents: IDictionary<_,_>) fullName (endPos: Range.pos): Idents option =
        match longIdents.TryGetValue endPos with
        | true, longIdent ->
            let rec loop matchFound longIdents symbolIdents =
                match longIdents, symbolIdents with
                | [], _ -> symbolIdents
                | _, [] -> []
                | lh :: lt, sh :: st ->
                    if lh <> sh then
                        if matchFound then symbolIdents else loop matchFound lt symbolIdents
                    else loop true lt st
                        
            let prefix = 
                loop false (longIdent |> Array.rev |> List.ofArray) (fullName |> Array.rev |> List.ofArray)
                |> List.rev
                |> List.toArray
                            
//            debug "[SourceCodeClassifier] QualifiedSymbol: FullName = %A, Symbol end pos = (%d, %d), Res = %A" 
//                  fullName endPos.Line endPos.Column prefix
            Some prefix
        | _ -> None

    let getCategoriesAndLocations (allSymbolsUses: SymbolUse[], ast: ParsedInput option, lexer: LexerBase, getTextLine: int -> string,
                                   openDeclarations: OpenDeclaration list, allEntities: Map<string, Idents list> option) =
        let allSymbolsUses' =
            allSymbolsUses
            |> Seq.groupBy (fun su -> su.SymbolUse.RangeAlternate.EndLine)
            |> Seq.map (fun (line, sus) ->
                let tokens = lexer.TokenizeLine (line - 1)
                sus
                |> Seq.choose (fun su ->
                    let r = su.SymbolUse.RangeAlternate
                    lexer.GetSymbolFromTokensAtLocation (tokens, line - 1, r.End.Column - 1)
                    |> Option.bind (fun sym -> 
                        match sym.Kind with
                        | SymbolKind.Ident ->
                            // FCS returns inaccurate ranges for multiline method chains
                            // Specifically, only the End is right. So we use the lexer to find Start for such symbols.
                            if r.StartLine < r.EndLine then
                                Some (su, { Line = r.End.Line; StartCol = r.End.Column - sym.Text.Length; EndCol = r.End.Column })
                            else 
                                Some (su, { Line = r.End.Line; StartCol = r.Start.Column; EndCol = r.End.Column })
                        | _ -> None)))
            |> Seq.concat
            |> Seq.toArray
       
        // index all symbol usages by LineNumber 
        let wordSpans = 
            allSymbolsUses'
            |> Seq.map (fun (_, span) -> span)
            |> Seq.groupBy (fun span -> span.Line)
            |> Seq.map (fun (line, ranges) -> line, ranges)
            |> Map.ofSeq

        let spansBasedOnSymbolsUses = 
            allSymbolsUses'
            |> Seq.choose (fun (symbolUse, span) ->
                let span = 
                    match wordSpans.TryFind span.Line with
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
                                WordSpan = span' }
        
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

        let longIdentsByEndPos = UntypedAstUtils.getLongIdents ast
            
//        debug "LongIdents by line:" 
//        longIdentsByEndPos 
//        |> Seq.map (fun pair -> pair.Key.Line, pair.Key.Column, pair.Value) 
//        |> Seq.iter (debug "%A")

        let removeModuleSuffixes (symbolUses: SymbolUse[]) =
            match allEntities with
            | Some entities ->
                symbolUses 
                |> Array.map (fun symbolUse ->
                    let fullNames =
                        symbolUse.FullNames
                        |> Array.map (fun fullName ->
                            match entities |> Map.tryFind (String.Join (".", fullName)) with
                            | Some [cleanIdents] ->
                                //debug "[SourceCodeClassifier] One clean FullName %A -> %A" fullName cleanIdents
                                cleanIdents
                            | Some (firstCleanIdents :: _ as cleanIdentsList) ->
                                if cleanIdentsList |> List.exists ((=) fullName) then
                                    //debug "[SourceCodeClassifier] An exact match found among several clean idents: %A" fullName
                                    fullName
                                else
                                    //debug "[SourceCodeClassifier] No match found among several clean idents, return the first one FullName %A -> %A" 
                                      //    fullName firstCleanIdents
                                    firstCleanIdents
                            | _ -> 
                                //debug "[SourceCodeClassifier] NOT Cleaned FullName %A" fullName
                                fullName)
                    { symbolUse with FullNames = fullNames })
            | None -> symbolUses

//        let printSymbolUses msg (symbolUses: SymbolUse[]) =
//            debug "[SourceCodeClassifier] %s SymbolUses:\n" msg
//            for sUse in symbolUses do
//                let r = sUse.SymbolUse.RangeAlternate
//                debug "%A (%d, %d) -- (%d, %d)" sUse.FullNames r.StartLine r.StartColumn r.EndLine r.EndColumn
//            symbolUses

        let symbolPrefixes: (Range.range * Idents) [] =
            allSymbolsUses
            |> getSymbolUsesPotentiallyRequireOpenDecls
            //|> printSymbolUses "SymbolUsesPotentiallyRequireOpenDecls"
            |> removeModuleSuffixes
            //|> printSymbolUses "SymbolUsesWithModuleSuffixedRemoved"
            |> filterNestedSymbolUses longIdentsByEndPos
            //|> printSymbolUses "SymbolUses without nested"
            |> Array.map (fun symbolUse ->
                let sUseRange = symbolUse.SymbolUse.RangeAlternate
                symbolUse.FullNames
                |> Array.choose (fun fullName ->
                    getFullPrefix longIdentsByEndPos fullName sUseRange.End
                    |> Option.bind (function
                         | [||] -> None
                         | prefix -> Some (sUseRange, prefix)))) 
            |> Array.concat

        //debug "[SourceCodeClassifier] Symbols prefixes:\n%A,\nOpen declarations:\n%A" symbolPrefixes openDeclarations
        
        let openDeclarations = 
            Array.foldBack (fun (symbolRange: Range.range, symbolPrefix: Idents) openDecls ->
                openDecls |> OpenDeclarationGetter.updateOpenDeclsWithSymbolPrefix symbolPrefix symbolRange
            ) symbolPrefixes openDeclarations
            |> OpenDeclarationGetter.spreadIsUsedFlagToParents

        //debug "[SourceCodeClassifier] Fully processed open declarations:"
        //for decl in openDeclarations do debug "%A" decl

        let unusedOpenDeclarations: OpenDeclaration list =
            openDeclarations |> List.filter (fun decl -> not decl.IsUsed)

        //debug "[SourceCodeClassifier] Unused open declarations: %A" unusedOpenDeclarations

        let unusedOpenDeclarationSpans =
            unusedOpenDeclarations
            |> List.map (fun decl -> 
                { Category = Category.Unused
                  WordSpan = { Line = decl.DeclarationRange.StartLine 
                               StartCol = decl.DeclarationRange.StartColumn
                               EndCol = decl.DeclarationRange.EndColumn }})
    
        //debug "[SourceCodeClassifier] AST: %A" ast

        let allSpans = 
            spansBasedOnSymbolsUses 
            |> Seq.append (QuotationCategorizer.getCategories ast lexer)
            |> Seq.append (PrintfCategorizer.getCategories ast getTextLine)
            |> Seq.append (EscapedCharsCategorizer.getCategories ast getTextLine)
            |> Seq.append unusedOpenDeclarationSpans
            |> Seq.toArray

    //    for span in allSpans do
    //       debug "-=O=- %A" span
        allSpans