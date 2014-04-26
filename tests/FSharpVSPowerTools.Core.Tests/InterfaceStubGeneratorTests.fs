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

let getInterfaceStub nonGeneric line col lineStr idents =
    let results = 
        vsLanguageService.ParseAndCheckFileInProject(projectFileName, fileName, source, sourceFiles, args, framework, AllowStaleResults.MatchingSource)
        |> Async.RunSynchronously
    let symbolUse = results.GetSymbolUseAtLocation(line, col, lineStr, idents) |> Async.RunSynchronously
    match symbolUse with
    | Some s when (s.Symbol :? FSharpEntity) ->
        let e = s.Symbol :?> FSharpEntity
        if InterfaceStubGenerator.isInterface e then
            let typeParams = if nonGeneric then [||] else [|"'a"|]
            Some (InterfaceStubGenerator.formatInterface 0 4 typeParams "x" "raise (System.NotImplementedException())" s.DisplayContext e)
        else 
            None
    | _ -> None

let checkInterfaceStub line col lineStr idents (expected: string) =
    getInterfaceStub false line col lineStr idents 
    |> Option.map (fun s -> s.Replace("\r\n", "\n"))
    |> Option.get
    |> assertEqual (expected.Replace("\r\n", "\n"))

let checkInterfaceStubNonGeneric line col lineStr idents (expected: string) =
    getInterfaceStub true line col lineStr idents 
    |> Option.map (fun s -> s.Replace("\r\n", "\n"))
    |> Option.get
    |> assertEqual (expected.Replace("\r\n", "\n"))

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

[<Test>]
let ``should generate stubs for composite interface``() =
    checkInterfaceStub 31 25 "    interface Interface3 with " ["Interface3"] """
member x.Method3(arg1: int): int = 
    raise (System.NotImplementedException())

member x.Method2(arg1: int): int = 
    raise (System.NotImplementedException())

member x.Method1(arg1: int): int = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should generate stubs for interfaces with multiple properties``() =
    checkInterfaceStub 98 11 "    { new Indexer3 with " ["Indexer3"] """
member x.Item
    with get (): string = 
        raise (System.NotImplementedException())

member x.Item
    with set (v: string): unit = 
        raise (System.NotImplementedException())

member x.Item
    with set (v: float): unit = 
        raise (System.NotImplementedException())

member x.Item
    with get (): int = 
        raise (System.NotImplementedException())
"""

[<Test>]
let ``should generate stubs for interfaces with non-F# properties``() =
    checkInterfaceStub 119 38 "    { new System.Collections.Generic.IList<'a> with" ["IList"] """
member x.get_Item(index: int): 'a = 
    raise (System.NotImplementedException())

member x.set_Item(index: int, value: 'a): unit = 
    raise (System.NotImplementedException())

member x.IndexOf(item: 'a): int = 
    raise (System.NotImplementedException())

member x.Insert(index: int, item: 'a): unit = 
    raise (System.NotImplementedException())

member x.RemoveAt(index: int): unit = 
    raise (System.NotImplementedException())

member x.get_Count(): int = 
    raise (System.NotImplementedException())

member x.get_IsReadOnly(): bool = 
    raise (System.NotImplementedException())

member x.Add(item: 'a): unit = 
    raise (System.NotImplementedException())

member x.Clear(): unit = 
    raise (System.NotImplementedException())

member x.Contains(item: 'a): bool = 
    raise (System.NotImplementedException())

member x.CopyTo(array: 'a [], arrayIndex: int): unit = 
    raise (System.NotImplementedException())

member x.Remove(item: 'a): bool = 
    raise (System.NotImplementedException())

member x.GetEnumerator(): IEnumerator<'a> = 
    raise (System.NotImplementedException())

member x.GetEnumerator(): System.Collections.IEnumerator = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should avoid name capturing in arguments``() =
    checkInterfaceStub 164 11 "    { new IComparer<'a> with" ["IComparer"] """
member x.Compare(x1: 'a, y: 'a): int = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should escape keywords in arguments``() =
    checkInterfaceStub 173 15 "    interface IKeyword with" ["IKeyword"] """
member x.Method(``member``: int) (member1: int) (member2: int): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should handle type alias``() =
    checkInterfaceStub 254 19 "    let _ = { new D with" ["D"] """
member x.Dispose(): unit = 
    raise (System.NotImplementedException())
"""

[<Test; Ignore("This test picks up generic version for some strange reason.")>]
let ``should use qualified names when appropriate``() =
    checkInterfaceStubNonGeneric 178 35 "    { new System.Collections.ICollection with" ["ICollection"] """
member x.CopyTo(array: System.Array, index: int): unit = 
    raise (System.NotImplementedException())

member x.get_Count(): int = 
    raise (System.NotImplementedException())

member x.get_SyncRoot(): obj = 
    raise (System.NotImplementedException())

member x.get_IsSynchronized(): bool = 
    raise (System.NotImplementedException())

member x.GetEnumerator(): System.Collections.IEnumerator = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should fix leading upper case letter in argument names``() =
    checkInterfaceStub 261 15 "let _ = { new IWithUpperCaseArgs with" ["IWithUpperCaseArgs"] """
member x.Method(arg1: int) (aRg2: int) (aRg3: int) (arG4: int) (arg5: int) (arg2: int): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should not collide argument names in setters``() =
    checkInterfaceStub 280 15 "let _ = { new IWithProperties with" ["IWithProperties"] """
member x.Item
    with set (v: int) (v1: int): unit = 
        raise (System.NotImplementedException())
"""

#if INTERACTIVE
``should generate stubs for simple interface``();;
``should generate stubs for simple interface in object expressions``();;
``should generate stubs for non-F# interface in object expressions``();;
``should generate stubs for composite interface``();;
#endif
