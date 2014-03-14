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
    | :? FSharpEntity as e ->
        debug "%A (type: %s)" e (e.GetType().Name)
        e.IsFSharpModule = false
    | :? FSharpMemberFunctionOrValue as mfov ->
        debug "%A (type: %s)" mfov (mfov.GetType().Name)
        symbolUse.IsDefinition = false && mfov.IsImplicitConstructor
    | :? FSharpUnionCase
    | :? FSharpField -> false
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
    |> Array.map (fun x -> x.RangeAlternate)
