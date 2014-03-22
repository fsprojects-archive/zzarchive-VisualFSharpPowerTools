#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.InterfaceStubGeneratorTests
#endif

open NUnit.Framework
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.CompilerBinding
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "InterfaceSampleFile.fs")
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

let getInterfaceStub line col lineStr idents =
    let results = 
        vsLanguageService.ParseAndCheckFileInProject(projectFileName, fileName, source, sourceFiles, args, framework, AllowStaleResults.MatchingSource)
        |> Async.RunSynchronously
    let symbol = results.GetSymbolAtLocation(line, col, lineStr, idents)
    match symbol with
    | Some s when (s :? FSharpEntity) ->
        let e = s :?> FSharpEntity
        if e.IsInterface then
            Some (InterfaceStubGenerator.formatInterface 0 4 "x" "raise (System.NotImplementedException())" e)
        else 
            None
    | _ -> None

let checkInterfaceStub line col lineStr idents (expected: string) =
    getInterfaceStub line col lineStr idents 
    |> Option.map (fun s -> s.Replace("\r\n", "\n"))
    |> assertEqual (Some <| expected.Replace("\r\n", "\n"))

[<Test>]
let ``should generate stubs for simple interface``() =
    checkInterfaceStub 5 24 "   interface IPrintable with " ["IPrintable"] """
member x.Print(): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should generate stubs for simple interface in object expressions``() =
    checkInterfaceStub 9 21 "    { new IPrintable with" ["IPrintable"] """
member x.Print(): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should generate stubs for non-F# interface in object expressions``() =
    checkInterfaceStub 16 29 "    { new System.IDisposable with" ["System"; "IDisposable"] """
member x.Dispose(): unit = 
    raise (System.NotImplementedException())
"""

[<Ignore("Probably a bug in interface inheritance in FCS. Need to check.")>]
[<Test>]
let ``should generate stubs for composite interface``() =
    checkInterfaceStub 31 25 "    interface Interface3 with " ["Interface3"] """
member x.Method1(arg1: int): int = 
    raise (System.NotImplementedException())
member x.Method2(arg1: int): int = 
    raise (System.NotImplementedException())
member x.Method3(arg1: int): int = 
    raise (System.NotImplementedException())
"""

#if INTERACTIVE
``should generate stubs for simple interface``();;
``should generate stubs for simple interface in object expressions``();;
``should generate stubs for non-F# interface in object expressions``();;
``should generate stubs for composite interface``();;
#endif
