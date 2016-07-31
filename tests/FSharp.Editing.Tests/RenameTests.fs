module FSharp.Editing.Tests.RenameTests

open NUnit.Framework
open FSharp.Editing.IdentifierUtils
open FSharp.Editing

let shouldStaysAsIs symbolKind name = encapsulateIdentifier symbolKind name |> assertEqual name
let shouldBeEncapsulated symbolKind name = encapsulateIdentifier symbolKind name |> assertEqual ("``" + name + "``")

[<Test; Parallelizable>]
let ``should not encapsulate normal identifiers``() = 
    shouldStaysAsIs SymbolKind.Ident "abc"
    shouldStaysAsIs SymbolKind.Ident "abc1234"
    shouldStaysAsIs SymbolKind.Ident "a_4"

[<Test; Parallelizable>]
let ``should encapsulate keywords``() = 
    shouldBeEncapsulated SymbolKind.Ident "namespace"
    shouldBeEncapsulated SymbolKind.Ident "module"
    shouldBeEncapsulated SymbolKind.Ident "let"

[<Test; Parallelizable>]
let ``should encapsulate special chars``() = 
    shouldBeEncapsulated SymbolKind.Ident "this is a valid identifierer"
    shouldBeEncapsulated SymbolKind.Ident "look!" // reserved for future F#

[<Test; Parallelizable>]
let ``should not encapsulate already encapsulated identifiers``() = 
    shouldStaysAsIs SymbolKind.Ident "``this is already encapsulated``"
    shouldStaysAsIs SymbolKind.Ident "``this``"

[<Test; Parallelizable>]
let ``should not encapsulate operators``() = 
    shouldStaysAsIs SymbolKind.Operator "</>"
    shouldStaysAsIs SymbolKind.Operator "*"

[<Test; Parallelizable>]
let ``should not encapsulate generic type parameters``() = 
    shouldStaysAsIs SymbolKind.GenericTypeParameter "'a"

[<Test; Parallelizable>]
let ``should not encapsulate statically resolved type parameters``() = 
    shouldStaysAsIs SymbolKind.StaticallyResolvedTypeParameter "'a"