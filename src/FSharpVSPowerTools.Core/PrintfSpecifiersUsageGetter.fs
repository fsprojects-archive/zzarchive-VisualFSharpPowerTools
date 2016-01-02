module FSharpVSPowerTools.PrintfSpecifiersUsageGetter

open Microsoft.FSharp.Compiler
open FSharpVSPowerTools.UntypedAstUtils

[<NoComparison>]
type PrintfSpecifierUse =
    { SpecifierRange: Range.range
      ArgumentRange: Range.range }

let private startPos (r: Range.range) = r.StartLine, r.StartColumn

let getAll (input: ParseAndCheckResults) (onError: string -> unit): PrintfSpecifierUse[] option Async =
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
                    |> Array.partition (Range.rangeContainsRange func.FormatString)
                
                match ownSpecifiers with
                | [||] -> restSpecifiers, acc
                | _ ->
                    if func.Args.Length > ownSpecifiers.Length then
                        onError (sprintf "Too many Printf arguments for %+A (%d > %d)" 
                                         func func.Args.Length ownSpecifiers.Length)
                    
                    let prioritizeArgPos pos = 
                        Array.partition (fun a -> Range.rangeBeforePos a pos)
                        >> function (l, r) -> [| r |> Array.sortBy startPos
                                                 l |> Array.sortBy startPos |]
                                              |> Array.concat

                    let uses = 
                        func.Args
                        |> prioritizeArgPos ownSpecifiers.[0].Start
                        |> Array.zip (ownSpecifiers.[0..func.Args.Length - 1] |> Array.sortBy startPos)
                        |> Array.map (fun (specifier, arg) -> { SpecifierRange = specifier; ArgumentRange = arg })
                    restSpecifiers, uses :: acc
               ) (specifierRanges, [])
            |> snd
            |> List.toArray
            |> Array.concat
    }