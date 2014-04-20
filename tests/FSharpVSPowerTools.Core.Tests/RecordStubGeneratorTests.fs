#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
//#load "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.RecordStubGeneratorTests
#endif

open NUnit.Framework
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.CompilerBinding
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "RecordSampleFile.fs")
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
allUsesOfAllSymbols |> List.iter (printfn "%A")
#endif

let tryFindRecord line col lineStr idents =
    let results = 
        vsLanguageService.ParseAndCheckFileInProject(projectFileName, fileName, source, sourceFiles, args, framework, AllowStaleResults.MatchingSource)
        |> Async.RunSynchronously

    let symbolUse = results.GetSymbolUseAtLocation(line, col, lineStr, idents)
                    |> Async.RunSynchronously

    match symbolUse with
    | Some s when (s.Symbol :? FSharpEntity) ->
        let e = s.Symbol :?> FSharpEntity
        if e.IsFSharpRecord then
            e.FSharpFields
            |> Seq.map (fun f -> f.Name)
            |> Seq.reduce (fun a b -> a + " " + b)
            |> Some
        else 
            None
    | _ -> None

[<Test>]
let ``should generate stubs for single-field record type``() =
    tryFindRecord 12 7 "let x: Record =" ["Record"]
    |> assertEqual (Some "Field")

[<Test>]
let ``should generate stubs for multiple-field record type``() =
    tryFindRecord 13 7 "let y: Record2 =" ["Record2"]
    |> assertEqual (Some "Field1 Field2")


#if INTERACTIVE
``should generate stubs for single-field record type``()
``should generate stubs for multiple-field record type``()
#endif