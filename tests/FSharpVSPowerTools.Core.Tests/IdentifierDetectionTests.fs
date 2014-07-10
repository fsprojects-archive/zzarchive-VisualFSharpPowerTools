module FSharpVSPowerTools.Core.Tests.IdentifierDetectionTest

open FSharpVSPowerTools
open FSharpVSPowerTools.IdentifierUtils
open NUnit.Framework

[<Test>]
let ``should be able to detect backticked identifiers``() = 
    isDoubleBacktickIdent "``this is already encapsulated``" |> assertTrue
    isDoubleBacktickIdent "``this``" |> assertTrue
    isDoubleBacktickIdent "``X.Y``" |> assertTrue
    isDoubleBacktickIdent "this" |> assertFalse
    isDoubleBacktickIdent "myVariable" |> assertFalse

[<Test>]
let ``should be able to detect unioncase identifiers``() = 
    isUnionCaseIdent "Case1" |> assertTrue
    isUnionCaseIdent "``Case 1``" |> assertTrue
    isUnionCaseIdent "case1" |> assertFalse
    isUnionCaseIdent "``X.Y``" |> assertFalse
    isUnionCaseIdent "``Case2[x]``" |> assertFalse
