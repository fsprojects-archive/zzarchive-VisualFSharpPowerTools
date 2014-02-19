﻿#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpXmlDoc.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../bin/FSharpVSPowerTools.Logic.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.CompilerServiceTests
#endif

open NUnit.Framework
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.CompilerBinding
open FSharpVSPowerTools.ProjectSystem

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Tutorial.fs")
let source = File.ReadAllText(fileName)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")

let sourceFiles = [| fileName |]
let args = 
  [|"--noframework"; "--debug-"; "--optimize-"; "--tailcalls-";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"|]

let framework = FSharpTargetFramework.NET_4_5

#if INTERACTIVE
let checker = InteractiveChecker.Create()

let projectOptions = 
    checker.GetProjectOptionsFromCommandLineArgs
       (projectFileName,
        [| 
            yield! args
            yield fileName
        |])

let rec allSymbolsInEntities compGen (entities: IList<FSharpEntity>) = 
    [ for e in entities do 
          yield (e :> FSharpSymbol) 
          for x in e.MembersFunctionsAndValues do
             if compGen || not x.IsCompilerGenerated then 
               yield (x :> FSharpSymbol)
          for x in e.UnionCases do
             yield (x :> FSharpSymbol)
          for x in e.FSharpFields do
             if compGen || not x.IsCompilerGenerated then 
                 yield (x :> FSharpSymbol)
          yield! allSymbolsInEntities compGen e.NestedEntities ]

let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
printfn "There are %i error(s)." wholeProjectResults.Errors.Length
Array.iter (printfn "Errors:\n %A") wholeProjectResults.Errors;;
let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities;;
let allUsesOfAllSymbols = 
    [ for s in allSymbols do 
            let loc = s.DeclarationLocation |> Option.map (fun r -> Range.Range.toZ r)
            yield s.ToString(), loc, wholeProjectResults.GetUsesOfSymbol(s) ]
//allUsesOfAllSymbols |> List.iter (printfn "%A")
#endif

let getUsesOfSymbol line col lineStr =
    VSLanguageService.Instance.GetUsesOfSymbolAtLocation(projectFileName, fileName, source, sourceFiles, 
                                                                   line, col, lineStr, args, framework)
    |> Async.RunSynchronously
    |> Option.map (fun (_, _, _, symbolUses) -> symbolUses |> Array.map (fun x -> x.Range))
    |> Option.map set

let checkSymbolUsage line col lineStr expected =
    getUsesOfSymbol line col lineStr |> assertEqual (Some (set expected))

let hasNoSymbolUsage line col lineStr =
    getUsesOfSymbol line col lineStr |> assertEqual None

[<Test>]
let ``should find usages of arrays``() =
    checkSymbolUsage 
        126 29 "    let substring = helloWorld.[0..6]"
        [ (126, 20), (126, 30); (123, 8), (123, 18); (132, 17), (132, 27) ]

[<Test>]
let ``should find usages of members``() =
    checkSymbolUsage
        217 26 "        member this.Length = length"
        [ (217, 20), (217, 26); (227, 63), (227, 77); (227, 78), (227, 92) ]

    checkSymbolUsage
        610 35 "    eventForDelegateType.Publish.AddHandler("
        [ (610, 4), (610, 43) ]

    checkSymbolUsage
        714 16 "    Nested.``long name``()"
        [ (712, 12), (712, 25); (714, 4), (714, 24) ]

[<Test>]
let ``should find usages of DU constructors named with single upper-case letter``() =
    checkSymbolUsage
        470 14 "    type A = B of int"
        [ (470, 13), (470, 14); (471, 9), (471, 10); (471, 16), (471, 17) ]

[<Test>]
let ``should find usages of DU types named with single upper-case letter``() =
    checkSymbolUsage
        470 10 "    type A = B of int"
        [ (470, 9), (470, 10); (472, 13), (472, 14) ]

[<Test>]
let ``should find usages of operators``() =
    checkSymbolUsage
        690 22 "    let func1 x = x *. x + 3"
        [ (689, 10), (689, 12); (690, 20), (690, 22); (691, 23), (691, 25) ]

[<Test>]
let ``should find usages of operators starting with '>' symbol``() =
    checkSymbolUsage
        693 11 "    let (>>=) x y = ()"
        [ (693, 9), (693, 12); (694, 6), (694, 9) ]

    checkSymbolUsage
        696 8 "    1 >~>> 2"
        [ (695, 9), (695, 13); (696, 6), (696, 10) ]

[<Test>]
let ``should find usages of operators containing dots``() =
    checkSymbolUsage
        697 11 "    let (.>>) x y = ()"
        [ (697, 9), (697, 12); (698, 6), (698, 9) ]

    checkSymbolUsage
        699 11 "    let (>.>) x y = ()"
        [ (699, 9), (699, 12); (700, 6), (700, 9) ]

    checkSymbolUsage
        701 11 "    let ( >>. ) x y = ()"
        [ (701, 10), (701, 13); (702, 6), (702, 9); (702, 12), (702, 15); (704, 14), (704, 17); (704, 21), (704, 24); 
          (704, 27), (704, 30) ]

[<Test>]
let ``should find usages of symbols if where are operators containing dots on the same line``() =
    let line = "    let x = 1 >>. ws >>. 2 >>. ws"
    let usages = [ (703, 8), (703, 10); (704, 18), (704, 20); (704, 31), (704, 33) ]
    checkSymbolUsage 704 18 line usages
    checkSymbolUsage 704 19 line usages
    checkSymbolUsage 704 20 line usages

[<Test>]
let ``should find usages of symbols contacting with a special symbol on the right``() =
    checkSymbolUsage
        706 12 "    let f (a, b) = a + b"
        [ (706, 11), (706, 12); (706, 19), (706, 20) ]

    checkSymbolUsage
        707 9 "    type C<'a> = C of 'a"
        [ (707, 9), (707, 10) ]

    checkSymbolUsage
        709 5 "    g(2)"
        [ (708, 8), (708, 9); (709, 4), (709, 5) ]

[<Test>]
let ``should not find usages inside comments``() =
    hasNoSymbolUsage 478 11 "    // List.length ref"

[<Test>]
let ``should not find usages inside strings``() =
    hasNoSymbolUsage 476 22 "    let y = \"a message and more\""

[<Test>]
let ``should not find usages inside compiler directives``() =
    hasNoSymbolUsage 682 12 "#if COMPILED"