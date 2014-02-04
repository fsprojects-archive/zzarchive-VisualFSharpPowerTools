#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpXmlDoc.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.UnitTests.XMLDocTests
#endif

open System.IO
open XmlDocHelpers
open NUnit.Framework

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "SampleFile.fs")
let input = File.ReadAllText(fileName)

let mutable output = Set.empty

[<TestFixtureSetUp>]
let fixtureSetup() =
    output <- XmlDocHelpers.GetXmlDocables(input, fileName) |> Set.ofList

[<Test>]
let ``should create XML Doc for module-level let bounds``() =
    Set.contains (XmlDocable(line=3, indent=0, paramNames=[])) output |> assertEqual true
    Set.contains (XmlDocable(line=5, indent=0, paramNames=["x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(line=7, indent=0, paramNames=["x"; "y"])) output |> assertEqual true

[<Test>]
let ``should create XML Doc for type names``() =
    Set.contains (XmlDocable(line=18, indent=0, paramNames=[])) output |> assertEqual true
    Set.contains (XmlDocable(line=42, indent=0, paramNames=[])) output |> assertEqual true
    Set.contains (XmlDocable(line=45, indent=0, paramNames=[])) output |> assertEqual true

[<Test>]
let ``should create XML Doc for members``() =
    Set.contains (XmlDocable(line=24, indent=4, paramNames=["z"; "x"; "y"])) output |> assertEqual true
    Set.contains (XmlDocable(line=27, indent=4, paramNames=[])) output |> assertEqual true
    Set.contains (XmlDocable(line=33, indent=4, paramNames=["x"; "y"])) output |> assertEqual true

#if INTERACTIVE
fixtureSetup();;
Seq.iter (printfn "%A") output;;
``should create XML Doc for module-level let bounds``();;
``should create XML Doc for type names``();;
``should create XML Doc for members``();;
#endif