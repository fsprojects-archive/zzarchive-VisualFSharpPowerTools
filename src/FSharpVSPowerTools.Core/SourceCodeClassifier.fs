module FSharpVSPowerTools.Core.SourceCodeClassifier

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

[<NoComparison>]
type TypeLocation = 
    { TypeName: string
      Range: Range.range }

    static member FromLongIdentWithDots ((LongIdentWithDots(ident, _) as i)) =
        TypeLocation.FromIdentAndRange ident i.Range
        
    static member FromIdentAndRange (ident: LongIdent) _range =
        let name, range =
            ident
            |> List.rev
            |> (function
                | h :: _ -> h.idText, h.idRange
                | [] -> "", Range.range())

        { TypeName = name; Range = range }

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
    let comparer =
        { new IEqualityComparer<FSharpSymbolUse> with
            member this.Equals(x, y): bool =
                x.IsDefinition = y.IsDefinition &&
                x.RangeAlternate = y.RangeAlternate &&
                x.Symbol.DisplayName = y.Symbol.DisplayName
            member this.GetHashCode(x): int =
                hash (x.IsDefinition, x.RangeAlternate, x.Symbol.DisplayName) }

    HashSet<_>(allSymbolsUses, comparer) 
    |> Seq.filter isTypeSymbol
    |> Seq.map (fun x -> { TypeName = x.Symbol.DisplayName; Range = x.RangeAlternate })
    |> Seq.toArray
