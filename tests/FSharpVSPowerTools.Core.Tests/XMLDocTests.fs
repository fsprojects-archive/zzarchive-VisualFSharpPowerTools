#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpXmlDoc.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.XmlDocTests
#endif

open System.IO
open FSharpVSPowerTools.Core
open NUnit.Framework

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "SampleFile.fs")
let input = File.ReadAllText(fileName)

let output = XmlDocParser.GetXmlDocables(input, fileName) |> Set.ofList

[<Test>]
let ``should create XML Doc for module-level let bounds``() =
    Set.contains (XmlDocable(3, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(5, 0, ["x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(7, 0, ["x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(77, 0, ["x"])) output |> assertEqual true

[<Test>]
let ``should not create XML Doc for module-level let bounds which already have non-empty XML Docs``() =
    Set.contains (XmlDocable(79, 0, ["x"])) output |> assertEqual false
    Set.contains (XmlDocable(82, 0, ["x"])) output |> assertEqual false

[<Test>]
let ``should create XML Doc for type names``() =
    Set.contains (XmlDocable(18, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(42, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(45, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(86, 0, [])) output |> assertEqual true

[<Test>]
let ``should not create XML Doc for types which alredy have non-empty XML Docs``() =
    Set.contains (XmlDocable(89, 0, [])) output |> assertEqual false

[<Test>]
let ``should create XML Doc for members``() =
    Set.contains (XmlDocable(24, 4, ["z"; "x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(27, 4, [])) output |> assertEqual true
    Set.contains (XmlDocable(33, 4, ["x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(91, 4, [])) output |> assertEqual true

[<Test>]
let ``should not create XML Doc for members which already have non-empty XML Docs``() =
    Set.contains (XmlDocable(93, 4, [])) output |> assertEqual false

[<TestCase ("///<", 3)>]
[<TestCase ("/// <", 4)>]
[<TestCase ("///    <", 7)>]
[<TestCase (" ///<", 4)>]
let ``detects blank XML doc comment``(sample, pos) =
    XmlDocComment.isBlank sample |> assertEqual (Some pos)

[<TestCase "">]
[<TestCase "/">]
[<TestCase "//">]
[<TestCase "///">]
[<TestCase "////">]
[<TestCase "word">]
[<TestCase "///</">]
[<TestCase "///< ">]
let ``detects not blank XML doc comment``(sample) =
    XmlDocComment.isBlank sample |> assertEqual None

#if INTERACTIVE
Seq.iter (printfn "%A") output;;
``should create XML Doc for module-level let bounds``();;
``should create XML Doc for type names``();;
``should create XML Doc for members``();;
#endif