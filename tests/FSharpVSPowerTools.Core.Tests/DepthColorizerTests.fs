#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.DepthColorizerTests
#endif

open System.IO
open FSharpVSPowerTools.Core
open NUnit.Framework

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "DepthColorizerSampleFile.fs")
let input = File.ReadAllText(fileName)

let output = DepthParser.GetNonoverlappingDepthRanges(input, fileName) |> Async.RunSynchronously

[<Test>]
let ``should create single level at depth 0 for module definition``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 1 && i = 0 && c = 17 && d = 0) |> assertEqual true
    output |> Seq.filter (fun (l, _, _, _) -> l = 1) |> Seq.length |> assertEqual 1

[<Test>]
let ``should create single level for blank line``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 2 && i = 0 && c = 0 && d = 0) |> assertEqual true
    output |> Seq.filter (fun (l, _, _, _) -> l = 2 ) |> Seq.length |> assertEqual 1

[<Test>]
let ``should create depth 1 for module-level let``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 3 && i = 0 && c = 10 && d = 1) |> assertEqual true
    
[<Test>]
let ``should create depth 1 for local module``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 5 && i = 0 && c = 20 && d = 1) |> assertEqual true
    output |> Seq.exists (fun (l, i, c, d) -> l = 5 && i = 0 && c = 0 && d = 0) |> assertEqual true
    
[<Test>]
let ``should increase depth for inner module``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 6 && i = 0 && c = 4 && d = 1) |> assertEqual true
    output |> Seq.exists (fun (l, i, c, d) -> l = 6 && i = 4 && c = 18 && d = 2) |> assertEqual true

[<Test>]
let ``should show all lines of func at same depth``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 8 && i = 8 && c = 255 && d = -3) |> assertEqual true
    output |> Seq.exists (fun (l, i, c, d) -> l = 9 && i = 12 && c = 13 && d = 3) |> assertEqual true
    
[<Test>]
let ``should increase depth for type definition``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 12 && i = 8 && c = 255 && d = -3) |> assertEqual true
    output |> Seq.exists (fun (l, i, c, d) -> l = 13 && i = 12 && c = 17 && d = 3) |> assertEqual true
    
[<Test>]
let ``should decrease depth for end of type definition``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 13 && i = 17 && c = 21 && d = 2) |> assertEqual true
    
[<Test>]
let ``should increase depth for body of if statement``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 27 && i = 16 && c = 52 && d = 3) |> assertEqual true
    output |> Seq.exists (fun (l, i, c, d) -> l = 29 && i = 16 && c = 26 && d = 3) |> assertEqual true

[<Test>]
let ``should increase depth for type member``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 73 && i = 8 && c = 18 && d = 3) |> assertEqual true

[<Test>]
let ``should increase depth for interface member``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 81 && i = 8 && c = 255 && d = -3) |> assertEqual true

[<Test>]
let ``should increase depth for elif``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 150 && i = 12 && c = 17 && d = 5) |> assertEqual true

[<Test>]
let ``should increase depth for nested elif``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 152 && i = 12 && c = 17 && d = 6) |> assertEqual true

[<Test>]
let ``should increase depth inside try block``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 131 && i = 12 && c = 17 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside with block``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 133 && i = 12 && c = 17 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside finally block``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 138 && i = 12 && c = 17 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside else``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 128 && i = 12 && c = 17 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside matched pattern``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 143 && i = 8 && c = 20 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside while loop``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 157 && i = 12 && c = 17 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside for loop``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 160 && i = 12 && c = 17 && d = 4) |> assertEqual true

[<Test>]
let ``should increase depth inside async expression``() =
    output |> Seq.exists (fun (l, i, c, d) -> l = 175 && i = 12 && c = 17 && d = 4) |> assertEqual true

#if INTERACTIVE
Seq.iter (printfn "%A") output;;
``should create single level at depth 0 for module definition``();;
``should create single level for blank line``();;
``should create depth 1 for module-level let``();;
``should create depth 1 for local module``();;
``should increase depth for inner module``();;
``should show all lines of func at same depth``();;
``should increase depth for type definition``();;
``should decrease depth for end of type definition``();;
``should increase depth for body of if statement``();;
``should increase depth for type member``();;
``should increase depth for interface member``();;
``should increase depth for elif``();;
``should increase depth for nested elif``();;
``should increase depth inside try block``();;
``should increase depth inside with block``();;
``should increase depth inside finally block``();;
``should increase depth inside else``();;
``should increase depth inside matched pattern``();;
``should increase depth inside while loop``();;
``should increase depth inside for loop``();;
``should increase depth inside async expression``();;
#endif