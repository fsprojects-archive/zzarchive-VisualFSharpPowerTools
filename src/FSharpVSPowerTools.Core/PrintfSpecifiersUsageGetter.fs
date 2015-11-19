module FSharpVSPowerTools.PrintfSpecifiersUsageGetter

open Microsoft.FSharp.Compiler

[<NoComparison>]
type PrintfSpecifierUse =
    { SpecifierRange: Range.range
      ArgumentRange: Range.range }

let getAll (_input: ParseAndCheckResults): PrintfSpecifierUse[] option =
    Some [||]