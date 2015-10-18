module FSharpVSPowerTools.Core.Tests.UtilsTests

open FsCheck
open NUnit.Framework
open FSharpVSPowerTools

[<Test>]
let ``Array areEqual``() =
    Check.QuickThrowOnFailure <| fun (x: int[]) (y: int[]) ->
        (x = y) = (Array.areEqual x y)

    Assert.AreEqual((null = [||]), (Array.areEqual null [||]))
    Assert.AreEqual(([||] = null), (Array.areEqual [||] null))
