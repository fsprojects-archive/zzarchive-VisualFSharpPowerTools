module FSharpVSPowerTools.Core.SourceCodeClassifier

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

let isTypeSymbol (symbolUse: FSharpSymbolUse) =
    let symbol = symbolUse.Symbol

    match symbol with
    | :? FSharpGenericParameter
    | :? FSharpStaticParameter
    | :? FSharpUnionCase
    | :? FSharpField 
    | :? FSharpActivePatternCase -> false

    | :? FSharpEntity as e ->
        debug "%A (type: %s)" e (e.GetType().Name)
        e.IsClass || e.IsDelegate || e.IsEnum || e.IsFSharpAbbreviation || e.IsFSharpExceptionDeclaration
        || e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsMeasure || e.IsProvided
        || e.IsProvidedAndErased || e.IsProvidedAndGenerated || e.IsValueType
    
    | :? FSharpMemberFunctionOrValue as mfov ->
        debug "%A (type: %s)" mfov (mfov.GetType().Name)
        symbolUse.IsDefinition = false && mfov.IsImplicitConstructor
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        if (symbol.DeclarationLocation.IsNone && symbol.ImplementationLocation.IsNone
            && symbol.DisplayName <> ".ctor") then
            false
        else
            true

let getTypeLocations (allSymbolsUses: FSharpSymbolUse[]) =
    allSymbolsUses
    |> Array.filter isTypeSymbol
    |> Array.map (fun x ->
        let r = x.RangeAlternate
        let fullName = x.Symbol.FullName
        let name = x.Symbol.DisplayName
        let namespaceLength = fullName.Length - name.Length
        let symbolLength = r.EndColumn - r.StartColumn
        let res = 
            if namespaceLength > 0 && symbolLength > name.Length && fullName.EndsWith name then
                let startCol = r.StartColumn + namespaceLength
                let startCol = 
                    if startCol < r.EndColumn then startCol
                    else r.StartColumn
                r.StartLine, startCol, r.EndLine, r.EndColumn
            else 
                r.StartLine, r.StartColumn, r.EndLine, r.EndColumn
        //debug "-=O=- FullName = %s, Name = %s, range = %A" fullName name res
        res)
