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
           || e.IsProvidedAndErased || e.IsProvidedAndGenerated then
            ReferenceType
        else Other
    
    | :? FSharpMemberFunctionOrValue as func ->
        //debug "%A (type: %s)" mfov (mfov.GetType().Name)
        if func.CompiledName = ".ctor" then ReferenceType
        elif func.FullType.IsFunctionType && not func.IsGetterMethod && not func.IsSetterMethod then 
            if func.DisplayName.StartsWith "( " && func.DisplayName.EndsWith " )" then
                Other
            else
                Function
        else Other
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Other

type Range = 
    { StartCol: int 
      EndCol: int }

type CategorizedRange =
    { Category: Category
      Line: int
      Range: Range }

// If "what" range is completely included by "from" one, then truncate "from" to the end of "what".
// Example: for ReferenceType symbol "System.Diagnostics.DebuggerDisplay" there are "System", "Diagnostics" and "DebuggerDisplay"
// plane symbols. After excluding "System", we get "Diagnostics.DebuggerDisplay",
// after excluding "Diagnostics", we get "DebuggerDisplay" and we are done.
let excludeRange from what =
    if what.EndCol < from.EndCol && what.StartCol >= from.StartCol then
        { from with StartCol = what.EndCol + 1 } // the dot between parts
    else from

let getCategoriesAndLocations (allSymbolsUses: FSharpSymbolUse[]) =
    let allSymbolsUses =
        allSymbolsUses
        // FCS can return multi-line ranges, let's ignore them
        |> Array.filter (fun x -> x.RangeAlternate.StartLine = x.RangeAlternate.EndLine)

    // index all symbol usages by LineNumber 
    let wordRanges = 
        allSymbolsUses
        |> Array.map (fun su -> 
            su.RangeAlternate.StartLine, 
            { StartCol = su.RangeAlternate.StartColumn; EndCol = su.RangeAlternate.EndColumn })
        |> Seq.groupBy fst
        |> Seq.map (fun (line, words) -> line, words |> Seq.map snd)
        |> Map.ofSeq

    allSymbolsUses
    |> Array.map (fun x ->
        let r = x.RangeAlternate
        let range = { StartCol = r.StartColumn; EndCol = r.EndColumn }
        
        let range = 
            match wordRanges.TryFind x.RangeAlternate.StartLine with
            | Some ranges -> ranges |> Seq.fold (fun res r -> excludeRange res r) range
            | _ -> range

        let category = getCategory x
        //debug "-=O=- %A: %s, FullName = %s, Name = %s, Range = %s, Location = %A" 
        //      x.Symbol (x.Symbol.GetType().Name) x.Symbol.FullName name (x.RangeAlternate.ToShortString()) location
        { Category = category; Line = r.StartLine; Range = range })
    |> Seq.distinct
    |> Seq.toArray