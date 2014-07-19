namespace FSharpVSPowerTools

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
    | Other
    override x.ToString() = sprintf "%A" x

type CategorizedColumnSpan =
    { Category: Category
      WordSpan: WordSpan }

[<NoComparison>]
type OpenDeclaration =
    { Idents: Idents list
      DeclarationRange: Range.range
      ScopeRange: Range.range }

module OpenDeclarationGetter =
    open UntypedAstUtils
    open System

    let getAutoOpenModules entities =
        entities 
        |> List.filter (fun e -> 
             match e.Kind with
             | EntityKind.Module { IsAutoOpen = true } -> true
             | _ -> false)
        |> List.map (fun e -> e.Idents)

    let getModulesWithModuleSuffix entities =
        entities 
        |> List.choose (fun e -> 
            match e.Kind with
            | EntityKind.Module { HasModuleSuffix = true } ->
                // remove Module suffix
                let lastIdent = e.Idents.[e.Idents.Length - 1]
                let result = Array.copy e.Idents
                result.[result.Length - 1] <- lastIdent.Substring (0, lastIdent.Length - 6)
                Some result
            | _ -> None)

    let parseTooltip (ToolTipText elems): Idents list =
        elems
        |> List.map (fun e -> 
            let rawStrings =
                match e with
                | ToolTipElement.ToolTipElement (s, _) -> [s]
                | ToolTipElement.ToolTipElementGroup elems -> 
                    elems |> List.map (fun (s, _) -> s)
                | _ -> []
            
            let removePrefix prefix (str: string) =
                if str.StartsWith prefix then Some (str.Substring(prefix.Length).Trim()) else None

            rawStrings
            |> List.choose (fun (s: string) ->
                 maybe {
                    let! name, from = 
                        match s.Split ([|'\n'|], StringSplitOptions.RemoveEmptyEntries) with
                        | [|name; from|] -> Some (name, from)
                        | [|name|] -> Some (name, "")
                        | _ -> None

                    let! name = 
                        name 
                        |> removePrefix "namespace"
                        |> Option.orElse (name |> removePrefix "module")
                     
                    let from = from |> removePrefix "from" |> Option.map (fun s -> s + ".") |> Option.getOrElse ""
                    return from + name
                })
            |> List.map (fun s -> s.Split '.'))
        |> List.concat

    type Line = int
    type EndColumn = int

    let getOpenDeclarations (ast: ParsedInput option) (entities: RawEntity list option) 
                            (qualifyOpenDeclarations: Line -> EndColumn -> Idents -> Idents list) =
        match ast, entities with
        | Some (ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _))), Some entities ->
            let autoOpenModules = getAutoOpenModules entities
            debug "All AutoOpen modules: %A" autoOpenModules
            let modulesWithModuleSuffix = getModulesWithModuleSuffix entities

            let rec walkModuleOrNamespace (parent: LongIdent) acc (decls, moduleRange) =
                let openStatements =
                    decls
                    |> List.fold (fun acc -> 
                        function
                        | SynModuleDecl.NestedModule (ComponentInfo(_, _, _, ident, _, _, _, _), nestedModuleDecls, _, nestedModuleRange) -> 
                            walkModuleOrNamespace (parent @ ident) acc (nestedModuleDecls, nestedModuleRange)
                        | SynModuleDecl.Open (LongIdentWithDots(ident, _), openStatementRange) ->
                            let fullIdentsList = 
                                longIdentToArray ident
                                |> qualifyOpenDeclarations openStatementRange.StartLine openStatementRange.EndColumn

                            (* The idea that each open declaration can actually open itself and all direct AutoOpen modules,
                                children AutoOpen modules and so on until a non-AutoOpen module is met.
                                Example:
                                   
                                module M =
                                    [<AutoOpen>]                                  
                                    module A1 =
                                        [<AutoOpen>] 
                                        module A2 =
                                            module A3 = 
                                                [<AutoOpen>] 
                                                module A4 = ...
                                         
                                // this declaration actually open M, M.A1, M.A1.A2, but NOT M.A1.A2.A3 or M.A1.A2.A3.A4
                                open M
                            *)

                            let rec loop acc maxLength =
                                let newModules =
                                    autoOpenModules
                                    |> List.filter (fun autoOpenModule -> 
                                        autoOpenModule.Length = maxLength + 1
                                        && acc |> List.exists (fun collectedAutoOpenModule ->
                                            autoOpenModule |> Array.startsWith collectedAutoOpenModule))
                                match newModules with
                                | [] -> acc
                                | _ -> loop (acc @ newModules) (maxLength + 1)
                                
                            let identsAndAutoOpens = 
                                fullIdentsList
                                |> List.map (fun fullIdents -> fullIdents :: loop [fullIdents] fullIdents.Length)
                                |> List.concat

                            (* For each module that has ModuleSuffix attribute value we add additional Idents "<Name>Module". For example:
                                   
                                module M =
                                    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                                    module M1 =
                                        module M2 =
                                            let func _ = ()
                                open M.M1.M2
                                The last line will produce two Idents: "M.M1.M2" and "M.M1Module.M2".
                                The reason is that FCS return different FullName for entities declared in ModuleSuffix modules
                                depending on thether the module is in the current project or not. 
                            *)
                            let idents = 
                                identsAndAutoOpens
                                |> List.map (fun idents ->
                                    [ yield idents 
                                      match modulesWithModuleSuffix |> List.tryFind (fun m -> idents |> Array.startsWith m) with
                                      | Some m ->
                                          let index = (Array.length m) - 1
                                          let modifiedIdents = Array.copy idents
                                          modifiedIdents.[index] <- idents.[index] + "Module"
                                          yield modifiedIdents
                                      | None -> ()])
                                |> List.concat
                                |> Seq.distinct
                                |> Seq.toList

                            { Idents = idents
                              DeclarationRange = openStatementRange
                              ScopeRange = (Range.mkRange openStatementRange.FileName openStatementRange.Start moduleRange.End) } :: acc 
                        | _ -> acc) [] 
                openStatements @ acc

            modules
            |> List.fold (fun acc (SynModuleOrNamespace(ident, _, decls, _, _, _, moduleRange)) ->
                 walkModuleOrNamespace ident acc (decls, moduleRange) @ acc) []       
        | _ -> [] 

module QuotationCategorizer =
    let private categorize (lexer: LexerBase) ranges =
        let trimWhitespaces = 
            Seq.skipWhile (fun t -> t.CharClass = TokenCharKind.WhiteSpace) >> Seq.toList

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
        |> Seq.toArray

    let getCategories ast lexer = UntypedAstUtils.getQuatationRanges ast |> categorize lexer

module SourceCodeClassifier =
    let getIdentifierCategory = function
        | Entity (_, ValueType, _) -> Category.ValueType
        | Entity Class -> Category.ReferenceType
        | Entity (_, FSharpModule, _) -> Category.Module
        | Entity (_, _, Tuple) -> Category.ReferenceType
        | Entity (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType    
        | _ -> Category.Other 

    // Filter out symbols which ranges are fully included into a bigger symbols. 
    // For example, for this code: Ns.Module.Module1.Type.NestedType.Method() FCS returns the followint symbols: 
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
                    |> Seq.filter (fun nextSymbolUse ->
                        let res = 
                            symbolUses
                            |> Seq.exists (fun sUse -> 
                                nextSymbolUse <> sUse
                                && (sUse.FullNames.Value |> Array.exists (fun fullName ->
                                    nextSymbolUse.FullNames.Value |> Array.exists (fun nextSymbolFullName ->
                                    fullName.Length > nextSymbolFullName.Length
                                    && fullName |> Array.startsWith nextSymbolFullName))))
                            |> not
                        res)
                    |> Seq.toArray
                    |> Array.toSeq
                res
            | None -> symbolUses)
        |> Seq.concat
        |> Seq.toArray

    let getSymbolUsesPotentiallyRequireOpenDecls symbolsUses =
        symbolsUses
        |> Array.filter (fun (symbolUse, _) ->
            match symbolUse.SymbolUse.Symbol with
            | UnionCase _ 
            | Entity (Class | (ValueType | Record | UnionType | Interface | FSharpModule | Delegate), _, _)
            | MemberFunctionOrValue (Constructor _ | ExtensionMember) -> true
            | MemberFunctionOrValue func -> not func.IsMember
            | _ -> false) 
        |> Array.map (fun (symbolUse, _) -> symbolUse)

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

    let getFullPrefix (longIdents: IDictionary<_,_>) fullName endPos: Idents =
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
                            
            debug "[QS] FullName = %A, Symbol end pos = %A, Res = %A" fullName endPos prefix
            prefix
        | _ -> 
            debug "[!QS] Symbol is out of any LongIdent: FullName = %A, Symbol end pos = %A" fullName endPos
            fullName

    let getCategoriesAndLocations (allSymbolsUses: SymbolUse[], ast: ParsedInput option, lexer: LexerBase,
                                   openDeclarations: OpenDeclaration list) =
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
                            | Category.Unused -> 1
                            | Category.Other -> 2
                            | _ -> 0)
                        |> List.head)
            |> Seq.distinct
            |> Seq.toArray

        let longIdentsByEndPos = UntypedAstUtils.getLongIdents ast
        debug "LongIdents by line: %A" (longIdentsByEndPos |> Seq.map (fun pair -> pair.Key.Line, pair.Key.Column, pair.Value) |> Seq.toList)

        let symbolPrefixes: (Range.range * Idents) [] =
            allSymbolsUses'
            |> getSymbolUsesPotentiallyRequireOpenDecls
            |> filterNestedSymbolUses longIdentsByEndPos
            |> Array.map (fun symbolUse ->
                let sUseRange = symbolUse.SymbolUse.RangeAlternate
                symbolUse.FullNames.Value
                |> Array.map (fun fullName ->
                    sUseRange, getFullPrefix longIdentsByEndPos fullName sUseRange.End))
            |> Array.concat

        debug "[SourceCodeClassifier] Symbols prefixes: %A, Open declarations: %A" symbolPrefixes openDeclarations
        
        let unusedOpenDeclarations: OpenDeclaration list =
            Array.foldBack (fun (symbolRange: Range.range, symbolPrefix) openDecls ->
                openDecls |> List.fold (fun (acc, found) (openDecl, used) -> 
                    if found then
                        (openDecl, used) :: acc, found
                    else
                        let usedByCurrentSymbol =
                            Range.rangeContainsRange openDecl.ScopeRange symbolRange
                            && (let isUsed = openDecl.Idents |> List.exists ((=) symbolPrefix)
                                if isUsed then debug "Open decl %A is used by %A (exact prefix)" openDecl symbolPrefix
                                isUsed)
                        (openDecl, used || usedByCurrentSymbol) :: acc, usedByCurrentSymbol) ([], false)
                |> fst
                |> List.rev
            ) symbolPrefixes (openDeclarations |> List.map (fun openDecl -> openDecl, false))
            |> List.filter (fun (_, used) -> not used)
            |> List.map fst

        let unusedOpenDeclarationSpans =
            unusedOpenDeclarations
            |> List.map (fun decl -> 
                { Category = Category.Unused
                  WordSpan = { Line = decl.DeclarationRange.StartLine 
                               StartCol = decl.DeclarationRange.StartColumn
                               EndCol = decl.DeclarationRange.EndColumn }})
            |> List.toArray
    
        //printfn "[SourceCodeClassifier] AST: %A" untypedAst

        let allSpans = 
            spansBasedOnSymbolsUses 
            |> Array.append (QuotationCategorizer.getCategories ast lexer)
            |> Array.append unusedOpenDeclarationSpans
    //    for span in allSpans do
    //       debug "-=O=- %A" span
        allSpans