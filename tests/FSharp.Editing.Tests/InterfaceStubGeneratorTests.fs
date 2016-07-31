#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#r "../../packages/FsCheck/lib/net40-Client/FsCheck.dll"
#load "../../src/FSharpVSPowerTools.Core/InterfaceStubGenerator.fs"
#load "TestHelpers.fs"
#else
module FSharp.Editing.Tests.InterfaceStubGeneratorTests
#endif

open NUnit.Framework
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.Features

[<Literal>]
let dataFolderName = __SOURCE_DIRECTORY__ + "/../data/"
type dataFolder = FSharp.Management.FileSystem<dataFolderName>
 
let fileName = dataFolder.``InterfaceSampleFile.fs``
let source = File.ReadAllText(fileName)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let vsLanguageService = LanguageService()
let opts = vsLanguageService.GetProjectCheckerOptions(projectFileName, [| fileName |], TestHelpers.LanguageServiceTestHelper.args, [||]) 

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
allUsesOfAllSymbols |> List.iter (printfn "%A")
#endif

let private isInterfaceDeclarationAt line col =
    let results = 
        vsLanguageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.MatchingSource)
        |> Async.RunSynchronously
    
    let ast = results.ParseTree
    let pos = Range.Pos.fromZ (line-1) col
    (ast |> Option.bind (InterfaceStubGenerator.tryFindInterfaceDeclaration pos)).IsSome

[<Test>]
let ``should find interface declaration in class interface implementation``() = 
    isInterfaceDeclarationAt 5 14 |> assertTrue

[<Test>]
let ``should find interface declaration in object expression``() = 
    isInterfaceDeclarationAt 9 11 |> assertTrue
    isInterfaceDeclarationAt 82 11 |> assertTrue

[<Test>]
let ``should find second interface declaration in object expression``() = 
    isInterfaceDeclarationAt 66 17 |> assertTrue

[<Test>]
let ``should find interface declaration in base class constructor call``() = 
    isInterfaceDeclarationAt 406 31 |> assertTrue

[<Test>]
let ``should find abbreviated interface declaration``() = 
    isInterfaceDeclarationAt 257 19 |> assertTrue

[<Test>]
let ``should not find interface declaration in object expression if the base type is class``() = 
    isInterfaceDeclarationAt 51 18 |> assertFalse

let getInterfaceStub typeParams line col lineStr idents verboseMode =
    let results = 
        vsLanguageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.MatchingSource)
        |> Async.RunSynchronously
    let symbolUse = results.GetSymbolUseAtLocation(line, col, lineStr, idents) |> Async.RunSynchronously
    let typeParams = 
        let ast = results.ParseTree
        let pos = Range.Pos.fromZ (line-1) col
        ast
        |> Option.bind (InterfaceStubGenerator.tryFindInterfaceDeclaration pos)
        |> Option.map (fun x -> x.TypeParameters)
        |> fun opt -> defaultArg opt typeParams
    match symbolUse with
    | Some s when (s.Symbol :? FSharpEntity) ->
        let entity = s.Symbol :?> FSharpEntity
        if InterfaceStubGenerator.isInterface entity then
            Some (InterfaceStubGenerator.formatInterface 0 4 typeParams
                    "x" "raise (System.NotImplementedException())" s.DisplayContext Set.empty entity verboseMode)
        else 
            None
    | _ -> None

let checkInterfaceStubFull verbose line col lineStr idents (expected: string) =
    getInterfaceStub [|"'a"|] line col lineStr idents verbose
    |> Option.map (fun s -> s.Replace("\r\n", "\n"))
    |> Option.get
    |> assertEqual (expected.Replace("\r\n", "\n"))

let checkInterfaceStub = checkInterfaceStubFull true

let checkInterfaceStubWith typeParams line col lineStr idents (expected: string) =
    getInterfaceStub typeParams line col lineStr idents true
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
member x.Method1(arg1: int): int = 
    raise (System.NotImplementedException())
member x.Method2(arg1: int): int = 
    raise (System.NotImplementedException())
member x.Method3(arg1: int): int = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should generate stubs for interfaces with multiple properties``() =
    checkInterfaceStub 98 11 "    { new Indexer3 with " ["Indexer3"] """
member x.Item
    with set (v: float): unit = 
        raise (System.NotImplementedException())
member x.Item: int = 
    raise (System.NotImplementedException())
member x.Item
    with get (): string = 
        raise (System.NotImplementedException())
    and set (v: string): unit = 
        raise (System.NotImplementedException())
"""

[<Test>]
let ``should generate stubs for interfaces with non-F# properties``() =
    checkInterfaceStub 119 38 "    { new System.Collections.Generic.IList<'a> with" ["IList"] """
member x.Add(item: 'a): unit = 
    raise (System.NotImplementedException())
member x.Clear(): unit = 
    raise (System.NotImplementedException())
member x.Contains(item: 'a): bool = 
    raise (System.NotImplementedException())
member x.CopyTo(array: 'a [], arrayIndex: int): unit = 
    raise (System.NotImplementedException())
member x.Count: int = 
    raise (System.NotImplementedException())
member x.GetEnumerator(): IEnumerator<'a> = 
    raise (System.NotImplementedException())
member x.GetEnumerator(): System.Collections.IEnumerator = 
    raise (System.NotImplementedException())
member x.IndexOf(item: 'a): int = 
    raise (System.NotImplementedException())
member x.Insert(index: int, item: 'a): unit = 
    raise (System.NotImplementedException())
member x.IsReadOnly: bool = 
    raise (System.NotImplementedException())
member x.Item
    with get (index: int): 'a = 
        raise (System.NotImplementedException())
    and set (index: int) (v: 'a): unit = 
        raise (System.NotImplementedException())
member x.Remove(item: 'a): bool = 
    raise (System.NotImplementedException())
member x.RemoveAt(index: int): unit = 
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
    checkInterfaceStub 257 19 "    let _ = { new D with" ["D"] """
member x.Dispose(): unit = 
    raise (System.NotImplementedException())
"""

[<Test; Ignore("This test picks up generic version for some strange reason.")>]
let ``should use qualified names when appropriate``() =
    checkInterfaceStubWith [||] 178 35 "    { new System.Collections.ICollection with" ["ICollection"] """
member x.CopyTo(array: System.Array, index: int): unit = 
    raise (System.NotImplementedException())

member x.GetEnumerator(): System.Collections.IEnumerator = 
    raise (System.NotImplementedException())

member x.Count: int =
    raise (System.NotImplementedException())

member x.IsSynchronized: bool =
    raise (System.NotImplementedException())

member x.SyncRoot: obj =
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should fix leading upper case letter in argument names``() =
    checkInterfaceStub 265 15 "let _ = { new IWithUpperCaseArgs with" ["IWithUpperCaseArgs"] """
member x.Method(arg1: int) (aRg2: int) (aRg3: int) (arG4: int) (arg5: int) (arg2: int): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should not collide argument names in setters``() =
    checkInterfaceStub 284 15 "let _ = { new IWithProperties with" ["IWithProperties"] """
member x.Item
    with set (v: int) (v1: int): unit = 
        raise (System.NotImplementedException())
"""

[<Test>]
let ``should replace generic parameters on interfaces``() =
    checkInterfaceStubWith [|"string"; "int"|] 290 15 "let _ = { new IDictionary<string, int> with" ["IDictionary"] """
member x.Add(key: string, value: int): unit = 
    raise (System.NotImplementedException())
member x.Add(item: KeyValuePair<string,int>): unit = 
    raise (System.NotImplementedException())
member x.Clear(): unit = 
    raise (System.NotImplementedException())
member x.Contains(item: KeyValuePair<string,int>): bool = 
    raise (System.NotImplementedException())
member x.ContainsKey(key: string): bool = 
    raise (System.NotImplementedException())
member x.CopyTo(array: KeyValuePair<string,int> [], arrayIndex: int): unit = 
    raise (System.NotImplementedException())
member x.Count: int = 
    raise (System.NotImplementedException())
member x.GetEnumerator(): IEnumerator<KeyValuePair<string,int>> = 
    raise (System.NotImplementedException())
member x.GetEnumerator(): System.Collections.IEnumerator = 
    raise (System.NotImplementedException())
member x.IsReadOnly: bool = 
    raise (System.NotImplementedException())
member x.Item
    with get (key: string): int = 
        raise (System.NotImplementedException())
    and set (key: string) (v: int): unit = 
        raise (System.NotImplementedException())
member x.Keys: ICollection<string> = 
    raise (System.NotImplementedException())
member x.Remove(key: string): bool = 
    raise (System.NotImplementedException())
member x.Remove(item: KeyValuePair<string,int>): bool = 
    raise (System.NotImplementedException())
member x.TryGetValue(key: string, value: byref<int>): bool = 
    raise (System.NotImplementedException())
member x.Values: ICollection<int> = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should replace generic parameters for postfix type application``() =
    checkInterfaceStub 346 15 "let _ = { new IMy<int option> with" ["IMy"] """
member x.Method(arg1: int option): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should replace generic parameters for prefix type application``() =
    checkInterfaceStub 352 15 "let _ = { new IMy<Choice<int, string>> with" ["IMy"] """
member x.Method(arg1: Choice<int, string>): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should replace generic parameters for tuple types``() =
    checkInterfaceStub 356 15 "let _ = { new IMy<int * int> with" ["IMy"] """
member x.Method(arg1: int * int): unit = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should print the condensed form of events``() =
    checkInterfaceStub 365 15 "    interface IMyEvent<int> with" ["IMyEvent"] """
[<CLIEvent>]
member x.M: IEvent<int> = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should ensure properties are grouped correctly``() =
    checkInterfaceStub 374 15 "let _ = { new NewInfrastructure<string> with" ["NewInfrastructure"] """
member x.ReadWriteProp
    with get (): int = 
        raise (System.NotImplementedException())
    and set (v: int): unit = 
        raise (System.NotImplementedException())
member x.ReadonlyProp: int = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should ensure .NET event handlers are generated correctly``() =
    checkInterfaceStub 397 37 "let _ = { new System.ComponentModel.INotifyPropertyChanged with" ["System"; "ComponentModel"; "INotifyPropertyChanged"] """
[<CLIEvent>]
member x.PropertyChanged: IEvent<System.ComponentModel.PropertyChangedEventHandler,System.ComponentModel.PropertyChangedEventArgs> = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``should always use verbose mode on overloaded methods``() =
    checkInterfaceStubFull false 416 31 "type Overloaded =
    interface IOverloaded with" ["IOverloaded"] """
member x.Bar(thing) = raise (System.NotImplementedException())
member x.Foo(num: bool): bool = 
    raise (System.NotImplementedException())
member x.Foo(num: int): int = 
    raise (System.NotImplementedException())
"""

[<Test>]
let ``when verbose syntax is not requested use lightweight syntax``() =
    checkInterfaceStubFull false 423 15 "type LightweightInfrastructure() =
    interface Infrastructure with" ["Infrastructure"] """
member x.Serialize(arg1) = raise (System.NotImplementedException())
member x.ToXml() = raise (System.NotImplementedException())
"""

[<Test>]
let ``curried functions are handled correctly with lightweight syntax``() =
    checkInterfaceStubFull false 173 15 "type OKeyword =
    interface IKeyword with" ["IKeyword"] """
member x.Method ``member`` member1 member2 = raise (System.NotImplementedException())
"""

open System
open FsCheck
open Microsoft.FSharp.Compiler.SourceCodeServices.PrettyNaming


let allUnderscores (arg: string) = arg.ToCharArray() |> Array.forall (fun c -> c = '_')

let keywords = KeywordNames |> List.filter (not << allUnderscores)

let private rnd = Random()

let shuffle ls =
    let swap (a: _ []) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let arr = Array.ofList ls
    Array.iteri (fun i _ -> swap arr i (rnd.Next(i, Array.length arr))) arr
    arr |> Array.toList


let randomList = 
    Arb.generate
    |> Gen.filter (fun (x: string) -> 
        x <> null && x.Length > 0 && not (Char.IsDigit x.[0])
        && x.ToCharArray() |> Array.forall (fun c -> Char.IsLetterOrDigit c || c = '_')
        && not (allUnderscores x))
    |> Gen.nonEmptyListOf



let arbitraryArg =
    let keywords = Gen.elements keywords
    gen {
        let! randoms = randomList
        let! keywords = keywords |> Gen.listOf |> Gen.resize 50
        return (shuffle (randoms @ keywords))
    }
    |> Arb.fromGen


let normalizeArgs =
    List.fold (fun (acc, namesWithIndices) arg ->
            let arg, namesWithIndices = CodeGenerationUtils.normalizeArgName namesWithIndices arg
            arg :: acc, namesWithIndices)
        ([], Map.empty)
    >> fst


let checkArgs fn =
    Check.QuickThrowOnFailure (Prop.forAll arbitraryArg fn)

[<Test; Parallelizable>]
let ``does not change number of args``() =
    checkArgs (fun args -> normalizeArgs args |> List.length = List.length args)

    
[<Test; Parallelizable>]
let ``no duplicated args``() =
    checkArgs (fun args ->
        let normalized = normalizeArgs args
        normalized |> Seq.distinct |> Seq.toList = normalized)


[<Test; Parallelizable>]
let ``no keywords in args``() =
    checkArgs (fun args ->
        let normalized = normalizeArgs args
        set normalized |> Set.intersect (set keywords) = Set.empty)


[<Test; Parallelizable>]
let ``no empty args``() =
    checkArgs (fun args ->
        normalizeArgs args |> List.forall (not << String.IsNullOrWhiteSpace))


[<Test; Parallelizable>]
let ``all args start with lower case letter``() =
    checkArgs (fun args ->
        normalizeArgs args |> List.forall (fun arg -> 
            let arg = if arg.StartsWith "``" then arg.Substring 2 else arg
            let firstChar = arg.ToCharArray().[0]
            Char.IsLower firstChar || firstChar = '_')
    )


#if INTERACTIVE
``should generate stubs for simple interface``();;
``should generate stubs for simple interface in object expressions``();;
``should generate stubs for non-F# interface in object expressions``();;
``should generate stubs for composite interface``();;
#endif
