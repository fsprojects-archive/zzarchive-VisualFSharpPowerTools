module FSharpVSPowerTools.Core.Tests.RenameTests

open FSharpVSPowerTools
open NUnit.Framework

/// Encapsulates identifiers for rename operations if needed
let encapsulate name = 
    let delimiter = "``"
    let specialChars = [" "; "!"]
    let isKeyWord = List.exists ((=) name) Microsoft.FSharp.Compiler.Lexhelp.Keywords.keywordNames
    let containsSpecialChar = List.exists (fun char -> name.Contains char) specialChars

    if name.StartsWith delimiter && name.EndsWith delimiter then name // already encapsulated
    elif isKeyWord || containsSpecialChar then delimiter + name + delimiter
    else name

let staysAsIs name = encapsulate name |> assertEqual name
let wasEncapsulated name = encapsulate name |> assertEqual ("``" + name + "``")

[<Test>]
let ``should not encapsulate normal identifiers``() = 
    staysAsIs "abc"
    staysAsIs "abc1234"
    staysAsIs "a_4"

[<Test>]
let ``should encapsulate keywords``() = 
    wasEncapsulated "namespace"
    wasEncapsulated "module"
    wasEncapsulated "let"

[<Test>]
let ``should encapsulate special chars``() = 
    wasEncapsulated "this is a valid identifierer"
    wasEncapsulated "look!" // reserved for future F#

[<Test>]
let ``should not encapsulate already encapsulated identifiers``() = 
    staysAsIs "``this is already encapsulated``"
    staysAsIs "``this``"
