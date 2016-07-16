module FSharp.Editing.Features.PrintfSpecifiersUsageGetter

open Microsoft.FSharp.Compiler
open FSharp.Editing.UntypedAstUtils
open FSharp.Editing

[<NoComparison>]
type PrintfSpecifierUse =
    { SpecifierRange: Range.range
      ArgumentRange: Range.range }

let private startPos (r: Range.range) = r.StartLine, r.StartColumn
let private endPos (r: Range.range) = r.EndLine, r.EndColumn
let private mergeRanges (ranges : Range.range[]) =
    let startRange = ranges |> Array.minBy startPos
    let endRange = ranges |> Array.maxBy endPos
    Range.mkRange startRange.FileName startRange.Start endRange.End

let getAll (input: ParseAndCheckResults) (onError: string -> unit): PrintfSpecifierUse[] option Async =
    asyncMaybe {
        let! specRangesAndArities = input.GetFormatSpecifierLocationsAndArity()
        let specRangesAndArities = 
            specRangesAndArities 
            |> Array.map (fun (x, ar) -> 
                (Range.mkRange x.FileName x.Start (Range.mkPos x.EndLine (x.EndColumn + 1))), ar)

        let printfFunctions = Printf.getAll input.ParseTree

        return 
            printfFunctions
            |> Array.fold (fun (specRangesAndArities, acc) func ->
                let ownSpecifiers, restSpecifiers = 
                    specRangesAndArities
                    |> Array.partition (fst >> (Range.rangeContainsRange func.FormatString))

                match ownSpecifiers with
                | [||] -> restSpecifiers, acc
                | _ ->
                    let numSpecifierArgs = ownSpecifiers |> Array.sumBy snd
                    if func.Args.Length > numSpecifierArgs then
                        onError (sprintf "Too many Printf arguments for %+A (%d > %d)" 
                                         func func.Args.Length numSpecifierArgs)

                    let prioritizeArgPos pos = 
                        Array.partition (fun a -> Range.rangeBeforePos a pos)
                        >> function (l, r) -> [| r |> Array.sortBy startPos
                                                 l |> Array.sortBy startPos |]
                                              |> Array.concat

                    let uses =
                        let numUsedArgs = min func.Args.Length numSpecifierArgs
                        let sortedOwnSpecifiers =
                            ownSpecifiers |> Array.sortBy (fst>>startPos)

                        let usedArgs = 
                            func.Args
                            |> prioritizeArgPos (fst ownSpecifiers.[0]).Start
                            |> function args -> args.[0..(numUsedArgs - 1)]

                        let argChunks =
                            usedArgs
                            |> Array.splitByChunks (sortedOwnSpecifiers |> Array.map snd)

                        let argChunkRanges =
                            argChunks
                            |> Array.filter (fun chunk -> chunk.Length > 0)
                            |> Array.map mergeRanges

                        let argAcceptingSpecifiers =
                            sortedOwnSpecifiers
                            |> Array.filter (fun (_, ar) -> ar > 0)
                            |> function arr -> arr.[0..(argChunkRanges.Length - 1)]

                        argChunkRanges
                        |> Array.zip argAcceptingSpecifiers
                        |> Array.map (fun ((spec, _), arg) -> { SpecifierRange = spec; ArgumentRange = arg })
                    restSpecifiers, uses :: acc
               ) (specRangesAndArities, [])
            |> snd
            |> List.toArray
            |> Array.concat
    }