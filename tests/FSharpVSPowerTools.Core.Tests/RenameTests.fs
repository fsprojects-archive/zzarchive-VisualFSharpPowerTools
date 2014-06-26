module FSharpVSPowerTools.Core.Tests.RenameTests

open FSharpVSPowerTools
open NUnit.Framework

let encapsulate oldName = 
    if List.exists ((=) oldName) Microsoft.FSharp.Compiler.Lexhelp.Keywords.keywordNames then "``" + oldName + "``"
    else oldName

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