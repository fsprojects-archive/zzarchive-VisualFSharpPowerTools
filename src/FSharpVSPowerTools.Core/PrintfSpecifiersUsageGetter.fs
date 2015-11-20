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
                    |> Array.partition (Range.rangeContainsRange func.String)
            
                if func.Args.Length > ownSpecifiers.Length then
                    failwithf "Too many Printf arguments for %+A (%d > %d)" func func.Args.Length ownSpecifiers.Length

                let uses = 
                    func.Args
                    |> Array.sortBy sortRange
                    |> Array.zip (ownSpecifiers.[0..func.Args.Length - 1] |> Array.sortBy sortRange)
                    |> Array.map (fun (specifier, arg) -> { SpecifierRange = specifier; ArgumentRange = arg })
                restSpecifiers, uses :: acc
               ) (specifierRanges, [])
            |> snd
            |> List.toArray
            |> Array.concat
    }