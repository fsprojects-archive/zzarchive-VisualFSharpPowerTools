module FSharpVSPowerTools.PrintfSpecifiersUsageGetter

open Microsoft.FSharp.Compiler
open FSharpVSPowerTools.UntypedAstUtils

[<NoComparison>]
type PrintfSpecifierUse =
    { SpecifierRange: Range.range
      ArgumentRange: Range.range }

let getAll (input: ParseAndCheckResults): PrintfSpecifierUse[] option Async =
    asyncMaybe {
        let! specifierRanges = input.GetFormatSpecifierLocations()
        let _printfFunctions = Printf.getAll input.ParseTree
        return specifierRanges |> Array.map (fun x -> { SpecifierRange = x; ArgumentRange = x })
    }