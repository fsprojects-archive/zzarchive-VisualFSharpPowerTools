module FSharpVSPowerTools.Core.Tests.RenameTests

open FSharpVSPowerTools.Rename.Checks
open NUnit.Framework

let shouldStaysAsIs name = encapsulateIdentifier name |> assertEqual name
let shouldBeEncapsulated name = encapsulateIdentifier name |> assertEqual ("``" + name + "``")

[<Test>]
let ``should not encapsulate normal identifiers``() = 
    shouldStaysAsIs "abc"
    shouldStaysAsIs "abc1234"
    shouldStaysAsIs "a_4"

[<Test>]
let ``should encapsulate keywords``() = 
    shouldBeEncapsulated "namespace"
    shouldBeEncapsulated "module"
    shouldBeEncapsulated "let"

[<Test>]
let ``should encapsulate special chars``() = 
    shouldBeEncapsulated "this is a valid identifierer"
    shouldBeEncapsulated "look!" // reserved for future F#

[<Test>]
let ``should be able to detect backticked identifiers``() = 
    isDoubleBacktickIdent "``this is already encapsulated``" |> assertTrue
    isDoubleBacktickIdent "``this``" |> assertTrue
    isDoubleBacktickIdent "this" |> assertFalse
    isDoubleBacktickIdent "myVariable" |> assertFalse


[<Test>]
let ``should not encapsulate already encapsulated identifiers``() = 
    shouldStaysAsIs "``this is already encapsulated``"
    shouldStaysAsIs "``this``"