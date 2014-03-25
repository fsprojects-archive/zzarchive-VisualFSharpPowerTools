module FSharpVSPowerTools.Core.SourceCodeClassifier

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

type Category =
    | ReferenceType
    | ValueType
    | PatternCase
    | TypeParameter
    | Function
    | PublicField
    | MutableVar
    | Quotation
    | Other
    override x.ToString() = sprintf "%A" x

let internal getCategory (symbolUse: FSharpSymbolUse) =
    let symbol = symbolUse.Symbol
    
    let isOperator (name: string) =
        if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4 then
            name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
        else false

    let rec getEntityAbbreviatedType (entity: FSharpEntity) =
        if entity.IsFSharpAbbreviation then
            let typ = entity.AbbreviatedType
            if typ.HasTypeDefinition then getEntityAbbreviatedType typ.TypeDefinition
            else entity
        else entity

    let rec getAbbreviatedType (fsharpType: FSharpType) =
        if fsharpType.IsAbbreviation then
            let typ = fsharpType.AbbreviatedType
            if typ.HasTypeDefinition then getAbbreviatedType typ
            else fsharpType
        else fsharpType

    let isReferenceCell (fsharpType: FSharpType) = 
        let ty = getAbbreviatedType fsharpType
        ty.HasTypeDefinition 
        && ty.TypeDefinition.IsFSharpRecord
        && ty.TypeDefinition.FullName = "Microsoft.FSharp.Core.FSharpRef`1"

    match symbol with
    | :? FSharpGenericParameter
    | :? FSharpStaticParameter -> 
        TypeParameter
    | :? FSharpUnionCase
    | :? FSharpActivePatternCase -> 
        PatternCase

    | :? FSharpField as f ->
        if f.IsMutable || isReferenceCell f.FieldType then MutableVar
        elif f.Accessibility.IsPublic then PublicField 
        else Other

    | :? FSharpEntity as e ->
        //debug "%A (type: %s)" e (e.GetType().Name)
        let e = getEntityAbbreviatedType e
        if e.IsEnum || e.IsValueType then ValueType
        elif e.IsClass || e.IsDelegate || e.IsFSharpExceptionDeclaration
           || e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsMeasure || e.IsProvided
           || e.IsProvidedAndErased || e.IsProvidedAndGenerated 
           || (e.IsFSharp && e.IsOpaque && not e.IsFSharpModule && not e.IsNamespace) then
            ReferenceType
        else Other
    
    | :? FSharpMemberFunctionOrValue as func ->
        //debug "%A (type: %s)" mfov (mfov.GetType().Name)
        if func.CompiledName = ".ctor" then 
            if func.EnclosingEntity.IsValueType || func.EnclosingEntity.IsEnum then ValueType
            else ReferenceType
        elif func.FullType.IsFunctionType && not func.IsGetterMethod && not func.IsSetterMethod
             && not symbolUse.IsFromComputationExpression then 
            if isOperator func.DisplayName then Other
            else Function
        elif func.IsMutable || isReferenceCell func.FullType then MutableVar
        else Other
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Other

type Point = 
    { Line: int
      Col: int }

type Range = 
    { Start: Point
      End: Point }
    static member FromRange (r: Range.range) = 
        { Start = { Line = r.StartLine; Col = r.StartColumn }
          End = { Line = r.EndLine; Col = r.EndColumn }}

type CategorizedColumnSpan =
    { Category: Category
      Range: Range }

// If "what" span is entirely included in "from" span, then truncate "from" to the end of "what".
// Example: for ReferenceType symbol "System.Diagnostics.DebuggerDisplay" there are "System", "Diagnostics" and "DebuggerDisplay"
// plane symbols. After excluding "System", we get "Diagnostics.DebuggerDisplay",
// after excluding "Diagnostics", we get "DebuggerDisplay" and we are done.
let excludeRange from what =
    if what.End.Col < from.End.Col && what.End.Col > from.Start.Col then
        { from with Start = { from.Start with Col = what.End.Col + 1 }} // the dot between parts
    else from
 
let getCategoriesAndLocations (allSymbolsUses: FSharpSymbolUse[], untypedAst: ParsedInput option, 
                               getLexerSymbol: int -> int -> Symbol option) =
    let allSymbolsUses =
        allSymbolsUses
        // FCS can return multi-line ranges, let's ignore them
        |> Array.filter (fun symbolUse -> symbolUse.RangeAlternate.StartLine = symbolUse.RangeAlternate.EndLine)
      
    // index all symbol usages by LineNumber 
    let wordSpans = 
        allSymbolsUses
        |> Seq.map (fun su -> Range.FromRange su.RangeAlternate)
        |> Seq.groupBy (fun r -> r.Start.Line)
        |> Seq.map (fun (line, ranges) -> line, ranges)
        |> Map.ofSeq

    let spansBasedOnSymbolsUses = 
        allSymbolsUses
        |> Seq.choose (fun x ->
            let range = Range.FromRange x.RangeAlternate
        
            let range = 
                match wordSpans.TryFind x.RangeAlternate.StartLine with
                | Some spans -> spans |> Seq.fold (fun result span -> excludeRange result span) range
                | _ -> range

            let range' = 
                if (range.End.Col - range.Start.Col) - x.Symbol.DisplayName.Length > 0 then
                    // The span is wider that the simbol's display name.
                    // This means that we have not managed to extract last part of a long ident accurately.
                    // Particulary, it happens for chained method calls like Guid.NewGuid().ToString("N").Substring(1).
                    // So we get ident from the lexer.
                    match getLexerSymbol (x.RangeAlternate.Start.Line - 1) (range.End.Col - 1) with
                    | Some s -> 
                        match s.Kind with
                        | Ident -> 
                            // Lexer says that our span is too wide. Adjust it's left column.
                            if range.Start.Col < s.LeftColumn then 
                                { range with Start = { range.Start with Col = s.LeftColumn }}
                            else range
                        | _ -> range
                    | _ -> range
                else range

            let categorizedSpan =
                if range'.End.Col <= range'.Start.Col then None
                else Some { Category = getCategory x; Range = range' }
        
            categorizedSpan)
        |> Seq.distinct
        |> Seq.toArray

    let quotationRanges = ref (ResizeArray<_>())

    let rec visitPattern = function
        | SynPat.Wild(_) -> ()
        | SynPat.Named(pat, name, _, _, _) ->
            visitPattern pat
        | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
            //let names = String.concat "." [ for i in ident -> i.idText ]
            //printfn "  .. identifier: %s" names
            ()
        | pat -> () // printfn "  .. other pattern: %A" pat

    let rec visitExpression = function
        | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            // Visit all sub-expressions
            //printfn "Conditional:"
            visitExpression cond
            visitExpression trueBranch
            falseBranchOpt |> Option.iter visitExpression 
    
        | SynExpr.LetOrUse(_, _, bindings, body, _) ->
            // Visit bindings (there may be multiple 
            // for 'let .. = .. and .. = .. in ...'
            //printfn "LetOrUse with the following bindings:"
            for binding in bindings do
              let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, 
                           data, pat, retInfo, init, m, sp)) = binding
              visitPattern pat 
              visitExpression init
            // Visit the body expression
            //printfn "And the following body:"
            visitExpression body
        | SynExpr.Quote (op, isRaw, quotedExpr, isFromQueryExpression, range) ->
            (!quotationRanges).Add range
        | x -> () // printfn " - not supported expression: %A" x

    let visitDeclarations decls = 
        for declaration in decls do
            match declaration with
            | SynModuleDecl.Let(isRec, bindings, range) ->
                // Let binding as a declaration is similar to let binding
                // as an expression (in visitExpression), but has no body
                for binding in bindings do
                    let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, 
                                data, pat, retInfo, body, m, sp)) = binding
                    visitPattern pat 
                    visitExpression body         
            | _ -> () // printfn " - not supported declaration: %A" declaration

    let visitModulesAndNamespaces modulesOrNss =
        for moduleOrNs in modulesOrNss do
            let (SynModuleOrNamespace(_, _, decls, _, _, _, _)) = moduleOrNs
            visitDeclarations decls

    untypedAst |> Option.iter (fun ast ->
        match ast with
        | ParsedInput.ImplFile(implFile) ->
            // Extract declarations and walk over them
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
            visitModulesAndNamespaces modules
        | _ -> ()
    )

    //printfn "AST: %A" untypedAst
    let qoutations = 
        !quotationRanges |> Seq.map (fun r -> 
            { Category = Quotation
              Range = Range.FromRange r })
        |> Seq.toArray

    let allSpans = spansBasedOnSymbolsUses |> Array.append qoutations

    //for span in allSpans do
        //debug "-=O=- %A" span

    allSpans
