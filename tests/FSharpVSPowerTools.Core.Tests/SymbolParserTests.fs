#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpXmlDoc.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../bin/FSharpVSPowerTools.Logic.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.SymbolParserTests
#endif

open NUnit.Framework
open System.IO
open FSharpVSPowerTools

let source = File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "Tutorial.fs"))

let args = 
  [|"--noframework"; "--debug-"; "--optimize-"; "--tailcalls-";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"|]

let checkGetSymbol line col lineStr expected =
    SymbolParser.getSymbol source line col lineStr args None
    |> Option.map (fun { Line = line; LeftColumn = leftCol; RightColumn = rightCol; Text = text; Kind = kind } ->
        text, (line, leftCol), (line, rightCol), kind)
    |> assertEqual expected

[<Test>]
let ``should find operators``() =
    checkGetSymbol 693 10 "    let (>>=) x y = ()" (Some (">>=", (693, 9), (693, 12), Operator))
    checkGetSymbol 695 12 "    let (>~>>) x y = ()" (Some (">~>>", (695, 9), (695, 13), Operator))
    checkGetSymbol 701 12 "    let ( >>. ) x y = x" (Some (">>.", (701, 10), (701, 13), Operator))
    checkGetSymbol 704 15 "    let x = 1 >>. ws >>. 2 >>. ws" (Some (">>.", (704, 14), (704, 17), Operator))
    checkGetSymbol 728 9 "    M.N.(+.) 1 2" (Some ("+.", (728, 9), (728, 11), Operator))

[<Test>]
let ``should find identifiers``() =
    checkGetSymbol 703 8 "    let ws x = x" (Some ("ws", (703, 8), (703, 10), Ident))
    checkGetSymbol 702 24 "    1 >>. 2 >>. 3 |> ignore" (Some ("ignore", (702, 21), (702, 27), Ident))
    
    checkGetSymbol 722 14 "    Nested.``long name``()" (Some ("``long name``", (722, 11), (722, 24), Ident))

    checkGetSymbol 582 59 "    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))"
        (Some ("map", (582, 57), (582, 60), Ident))

    checkGetSymbol 582 48 "    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))"
        (Some ("Parallel", (582, 48), (582, 56), Ident))

    checkGetSymbol 582 56 "    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))"
        (Some ("Parallel", (582, 48), (582, 56), Ident))

[<Test>]
let ``should find generic parameters``() =
    checkGetSymbol 707 12 "    type C<'a> = C of 'a" (Some ("'a", (707, 11), (707, 13), GenericTypeParameter))
    checkGetSymbol 707 22 "    type C<'a> = C of 'a" (Some ("'a", (707, 22), (707, 24), GenericTypeParameter))

[<Test>]
let ``should find statically resolved type parameters``() =
    checkGetSymbol 730 22 "    let inline check< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =" 
        (Some ("^T", (730, 22), (730, 24), StaticallyResolvedTypeParameter))

[<Test>]
let ``should find active patterns``() =
    checkGetSymbol 744 10 "    let (|A|Bb|Ccc|) (x: int) =" (Some ("A", (744, 10), (744, 11), Ident))
    checkGetSymbol 744 12 "    let (|A|Bb|Ccc|) (x: int) =" (Some ("Bb", (744, 12), (744, 14), Ident))
