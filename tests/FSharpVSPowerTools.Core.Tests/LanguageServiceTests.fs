﻿#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.LanguageServiceTests
#endif

open NUnit.Framework
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

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
let languageService = new LanguageService(fun _ -> ())
let opts = languageService.GetProjectCheckerOptions(projectFileName, sourceFiles, args, [||])
#if INTERACTIVE
let checker = FSharpChecker.Create()

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
    languageService.GetUsesOfSymbolAtLocationInFile(opts, fileName, source, line, col, lineStr, args, AllowStaleResults.No, Lexer.queryLexState)
    |> Async.RunSynchronously
    |> Option.map (fun (_, _, symbolUses) -> 
        symbolUses |> Array.map (fun x -> 
                        let r = x.RangeAlternate
                        if r.StartLine <> r.EndLine then failwithf "StartLine should be equal to EndLine"
                        r.StartLine - 1, r.StartColumn, r.EndColumn))
    |> Option.map set

let checkSymbolUsage line col lineStr expected =
    getUsesOfSymbol line col lineStr |> assertEqual (Some (set expected))

let hasNoSymbolUsage line col lineStr =
    getUsesOfSymbol line col lineStr |> assertEqual None

let checkGetSymbol line col lineStr expected =
    Lexer.getSymbol source line col lineStr args Lexer.queryLexState
    |> Option.map (fun { Line = line; LeftColumn = leftCol; RightColumn = rightCol; Text = text; Kind = kind } ->
        text, (line, leftCol), (line, rightCol), kind)
    |> assertEqual expected

[<Test>]
let ``should find usages of arrays``() =
    checkSymbolUsage 
        126 29 "    let substring = helloWorld.[0..6]"
        [ (126, 20, 30); (123, 8, 18); (132, 17, 27) ]

[<Test>]
let ``should find usages of members``() =
    checkSymbolUsage
        217 26 "        member this.Length = length"
        [ (217, 20, 26); (227, 63, 77); (227, 78, 92) ]

    checkSymbolUsage
        610 35 "    eventForDelegateType.Publish.AddHandler("
        [ (610, 4, 43) ]

    checkSymbolUsage
        722 16 "    Nested.``long name``()"
        [ (720, 12, 25); (722, 4, 24) ]

[<Test>]
let ``should find usages of DU constructors named with single upper-case letter``() =
    checkSymbolUsage
        470 14 "    type A = B of int"
        [ (470, 13, 14); (471, 9, 10); (471, 16, 17) ]

[<Test>]
let ``should find usages of DU types named with single upper-case letter``() =
    checkSymbolUsage
        470 10 "    type A = B of int"
        [ (470, 9, 10); (472, 13, 14) ]

[<Test>]
let ``should find usages of operators``() =
    checkSymbolUsage
        690 22 "    let func1 x = x *. x + 3"
        [ (689, 10, 12); (690, 20, 22); (691, 23, 25) ]

[<Test>]
let ``should find usages of operators starting with '>' symbol``() =
    checkSymbolUsage
        693 11 "    let (>>=) x y = ()"
        [ (693, 9, 12); (694, 6, 9) ]

    checkSymbolUsage
        696 8 "    1 >~>> 2"
        [ (695, 9, 13); (696, 6, 10) ]

[<Test>]
let ``should find usages of operators containing dots``() =
    checkSymbolUsage
        697 11 "    let (.>>) x y = ()"
        [ (697, 9, 12); (698, 6, 9) ]

    checkSymbolUsage
        699 11 "    let (>.>) x y = ()"
        [ (699, 9, 12); (700, 6, 9) ]

    checkSymbolUsage
        701 11 "    let ( >>. ) x y = ()"
        [ (701, 10, 13); (702, 6, 9); (702, 12, 15); (704, 14, 17); (704, 21, 24); (704, 27, 30) ]

[<Test>]
let ``should find usages of operators containing 'at' symbol``() =
    checkSymbolUsage
        887 10 "    let (@) x y = ()"
        [ (887, 9, 10); (888, 14, 15) ]

    checkSymbolUsage
        889 10 "    let (@@) x y = ()"
        [ (889, 9, 11); (890, 14, 16) ]

    checkSymbolUsage
        891 10 "    let (@.@) x y = ()"
        [ (891, 9, 12); (892, 14, 17) ]

[<Test>]
let ``should find fully qualified operator``() =
    checkSymbolUsage 
        728 9 "    M.N.(+.) 1 2" 
        [ (726, 17, 19); (728, 4, 11) ]

[<Test>]
let ``should find usages of symbols if there are operators containing dots on the same line``() =
    let line = "    let x = 1 >>. ws >>. 2 >>. ws"
    let usages = [ (703, 8, 10); (704, 18, 20); (704, 31, 33) ]
    checkSymbolUsage 704 18 line usages
    checkSymbolUsage 704 19 line usages
    checkSymbolUsage 704 20 line usages

[<Test>]
let ``should find usages of symbols contacting with a special symbol on the right``() =
    checkSymbolUsage
        706 12 "    let f (a, b) = a + b"
        [ (706, 11, 12); (706, 19, 20) ]

    checkSymbolUsage
        707 9 "    type C<'a> = C of 'a"
        [ (707, 9, 10) ]

    checkSymbolUsage
        707 10 "    type C<'a> = C of 'a"
        [ (707, 9, 10) ]

    checkSymbolUsage
        709 5 "    g(2)"
        [ (708, 8, 9); (709, 4, 5) ]

[<Test>]
let ``should find all symbols in combined match patterns``() =
    checkSymbolUsage
        763 27 "    let _ = match [] with [h] | [_; h] | [_; _; h] -> h | _ -> 0"
        [(763, 27, 28); (763, 36, 37); (763, 42, 43); (763, 51, 52)]

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
        [ (707, 11, 13); (707, 22, 24) ]

[<Test>]
let ``should find usages of statically resolved type parameters``() =
    checkSymbolUsage 730 22 "    let inline check< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =" 
        [ (730, 22, 24)
          (730, 30, 32) 
          (730, 63, 65) 
          (730, 82, 84) 
          (730, 88, 90)
          (731, 12, 14)
          (731, 44, 46) ] 

[<Test>]
let ``should find usages of named discriminated union fields``() =
    checkSymbolUsage 735 15 "        | B of field1: int * field2: string" 
        [ (735, 15, 21)
          (737, 15, 21)
          (740, 13, 19) ]

[<Test>]
let ``should find usages of active patterns``() =
    checkSymbolUsage 744 10 "    let (|A|Bb|Ccc|) (x: int) =" 
        [ (744, 10, 11)
          (745, 22, 23)
          (749, 10, 11) ]

[<Test>]
let ``should find usages of statically resolved method names``() =
    checkSymbolUsage 754 54 "    let inline checkIt< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =" 
        [ (754, 52, 62) ]
    
    checkSymbolUsage 755 32 "        if (^T : (static member IsInfinity: ^T -> bool) (num)) then None" 
        [ (755, 32, 42) ]

[<Test>]
let ``should find usages of property initializers``() =
    checkSymbolUsage 759 19 """        member val Prop = "" with get, set"""
        [ (759, 19, 23)
          (761, 14, 18) ]

[<Test>]
let ``should find usages of properties with explicit getters and setters``() =
    checkSymbolUsage 765 17 "        member x.Name with get() = 0 and set (v: int) = ()"
        [ (765, 17, 21)
          (767, 12, 35)
          (768, 4, 27) ]

[<Test>] 
let ``should find usages of fully qualified record fields``() =
    checkSymbolUsage 770 9 "    type Record = { Field: int }"
        [ (770, 9, 15)
          (771, 14, 20) ]

    checkSymbolUsage 771 14 "    let r = { Record.Field = 1 }"
        [ (770, 9, 15)
          (771, 14, 20) ]

[<Test; Ignore "Bug in FCS 0.0.54">] 
let ``should find usages of generic types``() =
    checkSymbolUsage 895 9 "    type Type1<'a, 'b>() ="
        [ (895, 9, 14)
          (897, 12, 17) ]

let getFirstSymbol line col lineStr symbolText =
    async {
        let! results = languageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.No)
        return! results.GetSymbolUseAtLocation (line+1, col, lineStr, [symbolText]) }
    |> Async.RunSynchronously

[<Test; Ignore>]
let ``should instantiate types correctly``() =
    let symbolUse = getFirstSymbol 810 26 "              member x.Add(item: KeyValuePair<'K, 'V>): unit = " "Add"
    let symbol = symbolUse.Value.Symbol :?> FSharpMemberOrFunctionOrValue
    let genericType = symbol.FullType.GenericArguments.[0]
    let genericSymbolUse = getFirstSymbol 779 15 "        { new IDictionary<'K, 'V> with" "IDictionary"
    let genericParams = (genericSymbolUse.Value.Symbol :?> FSharpEntity).GenericParameters
    let context = genericSymbolUse.Value.DisplayContext
    let specificSymbolUse = getFirstSymbol 777 9 "    let x: KeyValuePair<string, int> = failwith \"\"" "x"
    let specificType = (specificSymbolUse.Value.Symbol :?> FSharpMemberOrFunctionOrValue).FullType
    let specificParams = specificType.GenericArguments
    let instantiatedType = genericType.Instantiate(Seq.zip genericParams specificParams |> Seq.toList)
//    printfn "Generic type: %A" genericType
//    printfn "Specific type: %A" specificType
//    printfn "Generic params: %A" genericParams
//    printfn "Specific params: %A" specificParams
//    printfn "Instantiated type: %A" instantiatedType
    instantiatedType.Format context |> assertEqual (specificType.Format context)

[<Test>]
let ``should instantiate types on a single entity``() =
    let symbolUse = getFirstSymbol 833 39 "        { new IDictionary<string, int> with" "IDictionary"
    let context = symbolUse.Value.DisplayContext
    let symbol = symbolUse.Value.Symbol :?> FSharpEntity
    let entity = symbol.DeclaredInterfaces.[0].TypeDefinition
    let genericParams = entity.GenericParameters
    let specificParams = symbol.DeclaredInterfaces.[0].GenericArguments
    let currentMember = entity.MembersFunctionsAndValues.[5]
    let genericType = currentMember.FullType
    let instantiatedType = genericType.Instantiate(Seq.zip genericParams specificParams |> Seq.toList)
//    printfn "Checking member: %O" currentMember.DisplayName
//    printfn "Generic type: %A" genericType
//    printfn "Generic params: %A" genericParams
//    printfn "Specific params: %A" specificParams
//    printfn "Instantiated type: %A" instantiatedType
    instantiatedType.Format context |> assertEqual "KeyValuePair<'TKey,'TValue> [] * int -> unit"

#if INTERACTIVE
``should instantiate types on a single entity``();;
``should instantiate types correctly``();;
#endif

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
    languageService.ProcessParseTrees(
        projectFileName, 
        Map.empty, 
        [| f1.FilePath; f2.FilePath |], 
        args, 
        framework, 
        seen.Add,
        System.Threading.CancellationToken.None) |> Async.RunSynchronously

    assertTrue (seen.Count = 2)
    assertEqual seen.[0].Range.FileName f1.FilePath
    assertEqual seen.[1].Range.FileName f2.FilePath

[<Test>]
let ``ProcessParseTree should prefer open documents``() =
    use f1 = tempSource "module Foo"
    let seen = ResizeArray()
    languageService.ProcessParseTrees(
        projectFileName,
        [f1.FilePath, "module Bar"] |> Map.ofList, 
        [| f1.FilePath|], 
        args, 
        framework, 
        seen.Add,
        System.Threading.CancellationToken.None)
    |> Async.RunSynchronously

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
    let cts = new System.Threading.CancellationTokenSource()
    cts.Cancel()
    languageService.ProcessParseTrees(
        projectFileName,
        Map.empty, 
        [| f1.FilePath|], 
        args, 
        framework, 
        seen.Add,
        cts.Token)
    |> Async.RunSynchronously

    assertTrue (seen.Count = 0)