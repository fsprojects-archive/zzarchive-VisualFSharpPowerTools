#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpXmlDoc.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.XMLDocTests
#endif

open System.IO
open XmlDocHelpers
open NUnit.Framework

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "SampleFile.fs")
let input = File.ReadAllText(fileName)

let output = XmlDocHelpers.GetXmlDocables(input, fileName) |> Set.ofList

[<Test>]
let ``should create XML Doc for module-level let bounds``() =
    Set.contains (XmlDocable(3, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(5, 0, ["x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(7, 0, ["x"; "y"])) output |> assertEqual true

[<Test>]
let ``should create XML Doc for type names``() =
    Set.contains (XmlDocable(18, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(42, 0, [])) output |> assertEqual true
    Set.contains (XmlDocable(45, 0, [])) output |> assertEqual true

[<Test>]
let ``should create XML Doc for members``() =
    Set.contains (XmlDocable(24, 4, ["z"; "x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(27, 4, [])) output |> assertEqual true
    Set.contains (XmlDocable(33, 4, ["x"; "y"])) output |> assertEqual true

#if INTERACTIVE
Seq.iter (printfn "%A") output;;
``should create XML Doc for module-level let bounds``();;
``should create XML Doc for type names``();;
``should create XML Doc for members``();;
#endif