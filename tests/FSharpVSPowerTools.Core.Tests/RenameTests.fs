﻿module FSharpVSPowerTools.Core.Tests.RenameTests

open FSharpVSPowerTools
open FSharpVSPowerTools.IdentifierUtils
open NUnit.Framework

let shouldStaysAsIs symbolKind name = encapsulateIdentifier symbolKind name |> assertEqual name
let shouldBeEncapsulated symbolKind name = encapsulateIdentifier symbolKind name |> assertEqual ("``" + name + "``")

[<Test>]
let ``should not encapsulate normal identifiers``() = 
    shouldStaysAsIs SymbolKind.Ident "abc"
    shouldStaysAsIs SymbolKind.Ident "abc1234"
    shouldStaysAsIs SymbolKind.Ident "a_4"

[<Test>]
let ``should encapsulate keywords``() = 
    shouldBeEncapsulated SymbolKind.Ident "namespace"
    shouldBeEncapsulated SymbolKind.Ident "module"
    shouldBeEncapsulated SymbolKind.Ident "let"

[<Test>]
let ``should encapsulate special chars``() = 
    shouldBeEncapsulated SymbolKind.Ident "this is a valid identifierer"
    shouldBeEncapsulated SymbolKind.Ident "look!" // reserved for future F#

[<Test>]
let ``should not encapsulate already encapsulated identifiers``() = 
    shouldStaysAsIs SymbolKind.Ident "``this is already encapsulated``"
    shouldStaysAsIs SymbolKind.Ident "``this``"

[<Test>]
let ``should not encapsulate operators``() = 
    shouldStaysAsIs SymbolKind.Operator "</>"
    shouldStaysAsIs SymbolKind.Operator "*"

[<Test>]
let ``should not encapsulate generic type parameters``() = 
    shouldStaysAsIs SymbolKind.GenericTypeParameter "'a"

[<Test>]
let ``should not encapsulate statically resolved type parameters``() = 
    shouldStaysAsIs SymbolKind.StaticallyResolvedTypeParameter "'a"