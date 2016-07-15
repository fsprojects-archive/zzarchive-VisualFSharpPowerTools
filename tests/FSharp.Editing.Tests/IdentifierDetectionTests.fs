module FSharp.Editing.Tests.IdentifierDetectionTest

open NUnit.Framework
open FSharp.Editing.IdentifierUtils

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

[<Test>]
let ``should be able to detect type names``() = 
    isTypeNameIdent "Type1" |> assertTrue
    isTypeNameIdent "``Type 1``" |> assertTrue
    isTypeNameIdent "type1" |> assertTrue
    isTypeNameIdent "``type 1``" |> assertTrue
    isTypeNameIdent "``X.Y``" |> assertFalse
    isTypeNameIdent "My.Foo" |> assertFalse
    isTypeNameIdent "``My.Foo``" |> assertFalse
    isTypeNameIdent "``Case2[x]``" |> assertFalse

[<Test>]
let ``should be able to detect fixable identifiers``() = 
    isFixableIdentifier "Type1" |> assertTrue
    isFixableIdentifier "``Type 1``" |> assertTrue
    isFixableIdentifier "->" |> assertTrue
    isFixableIdentifier "x y" |> assertTrue
    isFixableIdentifier "``X.Y``" |> assertTrue
    isFixableIdentifier "My.Foo" |> assertTrue
    isFixableIdentifier null |> assertFalse
    isFixableIdentifier "" |> assertFalse
