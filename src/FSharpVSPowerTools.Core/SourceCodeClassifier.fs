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
    | Other

let internal getCategory (symbolUse: FSharpSymbolUse) =
    let symbol = symbolUse.Symbol

    match symbol with
    | :? FSharpGenericParameter
    | :? FSharpStaticParameter -> 
        TypeParameter
    | :? FSharpUnionCase
    //| :? FSharpField 
    | :? FSharpActivePatternCase -> 
        PatternCase

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
    
    | :? FSharpMemberFunctionOrValue as mfov ->
        //debug "%A (type: %s)" mfov (mfov.GetType().Name)
        if mfov.CompiledName = ".ctor" then ReferenceType
        elif mfov.FullType.IsFunctionType then 
            if mfov.DisplayName.StartsWith "( " && mfov.DisplayName.EndsWith " )" then
                Other
            else
                Function
        else Other
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Other

let getTypeLocations (allSymbolsUses: FSharpSymbolUse[]) =
    allSymbolsUses
    |> Array.map (fun x ->
        let r = x.RangeAlternate
        let symbolLength = r.EndColumn - r.StartColumn
        let visibleName = x.Symbol.FullName.Substring (max 0 (x.Symbol.FullName.Length - symbolLength))
        let name = x.Symbol.DisplayName
        let namespaceLength = visibleName.Length - name.Length
        
        let location = 
            if namespaceLength > 0 && symbolLength > name.Length && visibleName.EndsWith name then
                let startCol = r.StartColumn + namespaceLength
                let startCol = 
                    if startCol < r.EndColumn then startCol
                    else r.StartColumn
                r.StartLine, startCol, r.EndLine, r.EndColumn
            else 
                r.StartLine, r.StartColumn, r.EndLine, r.EndColumn
        let category = getCategory x
        debug "-=O=- %A: FullName = %s, Name = %s, range = %A, symbol = %A" category visibleName name location x.Symbol
        category, location)
