module FSharpVSPowerTools.PrintfSpecifiersUsageGetter

open Microsoft.FSharp.Compiler
open FSharpVSPowerTools.UntypedAstUtils

[<NoComparison>]
type PrintfSpecifierUse =
    { SpecifierRange: Range.range
      ArgumentRange: Range.range }

let private sortRange (r: Range.range) = r.StartLine, r.StartColumn

let getAll (input: ParseAndCheckResults): PrintfSpecifierUse[] option Async =
    asyncMaybe {
        let! specifierRanges = input.GetFormatSpecifierLocations()
        let specifierRanges = 
            specifierRanges 
            |> Array.map (fun x -> 
                Range.mkRange x.FileName x.Start (Range.mkPos x.EndLine (x.EndColumn + 1)))

        let printfFunctions = Printf.getAll input.ParseTree

        return 
            printfFunctions
            |> Array.fold (fun (specifierRanges, acc) func ->
                let ownSpecifiers, restSpecifiers = 
                    specifierRanges 
                    |> Array.partition (Range.rangeContainsRange func.Full)
            
                if ownSpecifiers.Length > func.Args.Length then
                    failwithf "Too many Printf specifiers for %+A (%d > %d)" func ownSpecifiers.Length func.Args.Length

                let uses = 
                    ownSpecifiers
                    |> Array.sortBy sortRange
                    |> Array.zip (func.Args.[0..ownSpecifiers.Length - 1] |> Array.sortBy sortRange)
                    |> Array.map (fun (arg, specifier) -> { SpecifierRange = specifier; ArgumentRange = arg })
                restSpecifiers, uses :: acc
               ) (specifierRanges, [])
            |> snd
            |> List.toArray
            |> Array.concat
    }