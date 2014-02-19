#if INTERACTIVE
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.ParserTests
#endif

open NUnit.Framework
open FSharp.CompilerBinding

let (==>) (col, line) result = Parsing.findLongIdents (col, line) |> assertEqual (Some result)

[<Test>]
let ``can find simple idents``() =
    (4, "let f x = x * x") ==> (5, ["f"])

[<Test>]
let ``can find simple double-tick quoted idents``() =
    (7, "let ``f`` x = x * x") ==> (7, ["f"])

[<Test>]
let ``can find double-tick quoted idents containing whitespaces``() =
    (7, "let ``f g`` x = x * x") ==> (7, ["f g"])

