#if INTERACTIVE
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
open FSharpVSPowerTools
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
let vsLanguageService = new FSharp.CompilerBinding.LanguageService(fun _ -> ())
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
    vsLanguageService.GetUsesOfSymbolAtLocationInFile(projectFileName, fileName, source, sourceFiles, 
                                                      line, col, lineStr, args, framework, AllowStaleResults.No)
    |> Async.RunSynchronously
    |> Option.map (fun (_, _, symbolUses) -> 
        symbolUses |> Array.map (fun x -> 
                        let r = x.RangeAlternate
                        ((r.StartLine-1, r.StartColumn), (r.EndLine-1, r.EndColumn))))
    |> Option.map set

let checkSymbolUsage line col lineStr expected =
    getUsesOfSymbol line col lineStr |> assertEqual (Some (set expected))

let hasNoSymbolUsage line col lineStr =
    getUsesOfSymbol line col lineStr |> assertEqual None

let checkGetSymbol line col lineStr expected =
    SymbolParser.getSymbol source line col lineStr args None
    |> Option.map (fun { Line = line; LeftColumn = leftCol; RightColumn = rightCol; Text = text; Kind = kind } ->
        text, (line, leftCol), (line, rightCol), kind)
    |> assertEqual expected

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
        722 16 "    Nested.``long name``()"
        [ (720, 12), (720, 25); (722, 4), (722, 24) ]

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
let ``should find fully qualified operator``() =
    checkSymbolUsage 
        728 9 "    M.N.(+.) 1 2" 
        [ (726, 17), (726, 19); (728, 4), (728, 11) ]

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
        707 10 "    type C<'a> = C of 'a"
        [ (707, 9), (707, 10) ]

    checkSymbolUsage
        709 5 "    g(2)"
        [ (708, 8), (708, 9); (709, 4), (709, 5) ]

[<Test; Ignore "To activate when fixed">]
let ``should find all symbols in combined match patterns``() =
    checkSymbolUsage
        763 27 "    let _ = match [] with [h] | [_; h] | [_; _; h] -> h | _ -> 0"
        [(763, 27), (763, 28); (763, 36), (763, 37); (763, 42), (763, 43); (763, 51), (763, 52)]

[<Test>]
let ``should not find usages inside comments``() =
    hasNoSymbolUsage 478 11 "    // List.length ref"

[<Test>]
let ``should not find usages inside multiline comments``() =
    hasNoSymbolUsage 713 13 "        let p = 1"

[<Test>]
let ``should not find usages inside strings``() =
    hasNoSymbolUsage 476 22 "    let y = \"a message and more\""

[<Test>]
let ``should not find usages inside multiline strings``() =
    hasNoSymbolUsage 716 4 "let p = 1"

[<Test>]
let ``should not find usages inside compiler directives``() =
    hasNoSymbolUsage 682 12 "#if COMPILED"

[<Test>]
let ``should find usages of generic parameters``() =
    checkSymbolUsage 707 12 "    type C<'a> = C of 'a" 
        [ (707, 11), (707, 13)
          (707, 22), (707, 24) ]

[<Test>]
let ``should find usages of statically resolved type parameters``() =
    checkSymbolUsage 730 22 "    let inline check< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =" 
        [ (730, 22), (730, 24)
          (730, 30), (730, 32) 
          (730, 63), (730, 65) 
          (730, 82), (730, 84) 
          (730, 88), (730, 90)
          (731, 12), (731, 14)
          (731, 44), (731, 46) ] 

[<Test>]
let ``should find usages of named discriminated union fields``() =
    checkSymbolUsage 735 15 "        | B of field1: int * field2: string" 
        [ (735, 15), (735, 21)
          (737, 15), (737, 21)
          (740, 13), (740, 19) ]

[<Test>]
let ``should find usages of active patterns``() =
    checkSymbolUsage 744 10 "    let (|A|Bb|Ccc|) (x: int) =" 
        [ (744, 10), (744, 11)
          (745, 22), (745, 23)
          (749, 10), (749, 11) ]

[<Test; Ignore "FCS does not support this yet">]
let ``should find usages of statically resolved method names``() =
    checkSymbolUsage 754 54 "    let inline checkIt< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =" 
        [ (754, 54), (754, 62)
          (755, 32), (755, 44) ]

[<Test>]
let ``should find usages of property initializers``() =
    checkSymbolUsage 759 19 """        member val Prop = "" with get, set"""
        [ (759, 19), (759, 23)
          (761, 14), (761, 18) ]

type ITempSource = 
    inherit System.IDisposable
    abstract FilePath: string

let private tempSource content = 
    let path = Path.GetTempPath();
    let path = Path.ChangeExtension(path, "fs")
    File.WriteAllText(path, content)
    {
        new ITempSource with
            member this.FilePath = path
            member this.Dispose() = File.Delete(path)
    }

[<Test>]
let ``ProcessParseTree should be called for all files in project``() =
    use f1 = tempSource "module M1"
    use f2 = tempSource "module M2"
    let seen = ResizeArray()
    vsLanguageService.ProcessParseTrees(
        projectFileName, 
        Map.empty, 
        [| f1.FilePath; f2.FilePath |], 
        args, 
        framework, 
        seen.Add,
        System.Threading.CancellationToken.None)

    assertTrue (seen.Count = 2)
    assertEqual seen.[0].Range.FileName f1.FilePath
    assertEqual seen.[1].Range.FileName f2.FilePath

[<Test>]
let ``ProcessParseTree should prefer open documents``() =
    use f1 = tempSource "module Foo"
    let seen = ResizeArray()
    vsLanguageService.ProcessParseTrees(
        projectFileName,
        [f1.FilePath, "module Bar"] |> Map.ofList, 
        [| f1.FilePath|], 
        args, 
        framework, 
        seen.Add,
        System.Threading.CancellationToken.None)

    assertTrue (seen.Count = 1)
    match seen.[0] with
    | Ast.ParsedInput.ImplFile(Ast.ParsedImplFileInput(name, _isScript, _fileName, _scopedPragmas, _hashDirectives, [m], _)) -> 
        match m with
        | Ast.SynModuleOrNamespace([name], isModule, decls, _xmldoc, _attributes, _access, _range) ->
            assertEqual name.idText "Bar"
        | x -> 
            Assert.Fail (sprintf "Expected empty module named Bar got %+A" x)
    | _ -> 
        Assert.Fail("Impl file expected")

[<Test>]
let ``ProcessParseTree should react on cancellation``() =
    use f1 = tempSource "module Foo"
    let seen = ResizeArray()
    let cts = new System.Threading.CancellationTokenSource();
    cts.Cancel()
    vsLanguageService.ProcessParseTrees(
        projectFileName,
        Map.empty, 
        [| f1.FilePath|], 
        args, 
        framework, 
        seen.Add,
        cts.Token)

    assertTrue (seen.Count = 0)