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
    | Other
    override x.ToString() = sprintf "%A" x

let internal getCategory (symbolUse: FSharpSymbolUse) =
    let symbol = symbolUse.Symbol
    
    let isOperator (name: string) =
        if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4 then
            name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
        else false

    match symbol with
    | :? FSharpGenericParameter
    | :? FSharpStaticParameter -> 
        TypeParameter
    | :? FSharpUnionCase
    | :? FSharpActivePatternCase -> 
        PatternCase

    | :? FSharpField as f ->
        if f.Accessibility.IsPublic then PublicField else Other

    | :? FSharpEntity as e ->
        //debug "%A (type: %s)" e (e.GetType().Name)
        if e.IsEnum || e.IsValueType then
            ValueType
        elif e.IsFSharpAbbreviation then
            let typ = e.AbbreviatedType
            if typ.HasTypeDefinition && (typ.TypeDefinition.IsEnum || typ.TypeDefinition.IsValueType) then
                ValueType
            else ReferenceType
        elif e.IsClass || e.IsDelegate || e.IsFSharpExceptionDeclaration
           || e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsMeasure || e.IsProvided
           || e.IsProvidedAndErased || e.IsProvidedAndGenerated 
           || (e.IsFSharp && e.IsOpaque && not e.IsFSharpModule && not e.IsNamespace) then
            ReferenceType
        else Other
    
    | :? FSharpMemberFunctionOrValue as func ->
        //debug "%A (type: %s)" mfov (mfov.GetType().Name)
        if func.CompiledName = ".ctor" then 
            if func.EnclosingEntity.IsValueType || func.EnclosingEntity.IsEnum then
                ValueType
            else
                ReferenceType
        elif func.FullType.IsFunctionType && not func.IsGetterMethod && not func.IsSetterMethod
             && not symbolUse.IsFromComputationExpression then 
            if isOperator func.DisplayName then
                Other
            else
                Function
        else Other
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Other

type ColumnSpan = 
    { Start: int 
      End: int }

type CategorizedColumnSpan =
    { Category: Category
      Line: int
      ColumnSpan: ColumnSpan }

// If "what" span is entirely included in "from" span, then truncate "from" to the end of "what".
// Example: for ReferenceType symbol "System.Diagnostics.DebuggerDisplay" there are "System", "Diagnostics" and "DebuggerDisplay"
// plane symbols. After excluding "System", we get "Diagnostics.DebuggerDisplay",
// after excluding "Diagnostics", we get "DebuggerDisplay" and we are done.
let excludeColumnSpan from what =
    if what.End < from.End && what.End >= from.Start then
        { from with Start = what.End + 1 } // the dot between parts
    else from
 
let getCategoriesAndLocations (allSymbolsUses: FSharpSymbolUse[], getLexerSymbol: int -> int -> Symbol option) =
    let allSymbolsUses =
        allSymbolsUses
        // FCS can return multi-line ranges, let's ignore them
        |> Array.filter (fun symbolUse -> symbolUse.RangeAlternate.StartLine = symbolUse.RangeAlternate.EndLine)
      
    // index all symbol usages by LineNumber 
    let wordSpans = 
        allSymbolsUses
        |> Array.map (fun su -> 
            su.RangeAlternate.StartLine, 
            { Start = su.RangeAlternate.StartColumn; End = su.RangeAlternate.EndColumn })
        |> Seq.groupBy fst
        |> Seq.map (fun (line, words) -> line, words |> Seq.map snd)
        |> Map.ofSeq

    allSymbolsUses
    |> Array.map (fun x ->
        let r = x.RangeAlternate
        let span = { Start = r.StartColumn; End = r.EndColumn }
        
        let span = 
            match wordSpans.TryFind x.RangeAlternate.StartLine with
            | Some spans -> spans |> Seq.fold (fun result span -> excludeColumnSpan result span) span
            | _ -> span

        let span' = 
            if (span.End - span.Start) - x.Symbol.DisplayName.Length > 0 then
                // The span is wider that the simbol's display name.
                // This means that we have not managed to extract last part of a long ident accurately.
                // Particulary, it happens for chained method calls like Guid.NewGuid().ToString("N").Substring(1).
                // So we get ident from the lexer.
                match getLexerSymbol (r.StartLine - 1) (span.End - 1) with
                | Some s ->
                    match s.Kind with
                    | Ident -> 
                        // Lexer says that our span is too wide. Adjust it's left column.
                        if span.Start < s.LeftColumn then { span with Start = s.LeftColumn }
                        else span
                    | _ -> span
                | _ -> span
            else span

        let categorizedSpan = { Category = getCategory x; Line = r.StartLine; ColumnSpan = span' } 
        //debug "-=O=- %A: %s, FullName = %s, Range = %s, Span = %A" 
        //  x.Symbol (x.Symbol.GetType().Name) x.Symbol.FullName (x.RangeAlternate.ToShortString()) categorizedSpan
        categorizedSpan
        )
    |> Seq.distinct
    |> Seq.toArray