module FSharpVSPowerTools.Core.Tests.NavigateToItemTests

open NUnit.Framework
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open System.Threading
 
//type ITempSource = 
//    inherit System.IDisposable
//    abstract FilePath: string
//
//let private tempSource content = 
//    let path = Path.ChangeExtension(Path.GetTempPath(), ".fs")
//    File.WriteAllText(path, content)
//    { new ITempSource with
//        member __.FilePath = path
//        member __.Dispose() = File.Delete path }
//
//let private processParseTree sources openDocs ct = 
//    let seen = ResizeArray()
//    languageService.ProcessParseTrees(opts, openDocs, sources, (fun file ast -> seen.Add (file, ast)), ct) 
//    |> Async.RunSynchronously
//    seen |> Seq.map (fun (file, ast) -> file, ast) |> Seq.toList
//
//[<Test>]
//let ``ProcessParseTree should be called for all files in project``() =
//    use f1 = tempSource "module M1"
//    use f2 = tempSource "module M2"
//    let actual = processParseTree [| f1.FilePath; f2.FilePath |] Map.empty CancellationToken.None |> List.map fst
//    CollectionAssert.AreEquivalent ([ f1.FilePath; f2.FilePath ], actual)
//
//[<Test>]
//let ``ProcessParseTree should prefer open documents``() =
//    use f1 = tempSource "module Foo"
//    let actual = processParseTree [| f1.FilePath |] (Map.ofList [f1.FilePath, "module Bar"]) CancellationToken.None
//
//    match actual |> List.map snd with
//    | [ Ast.ParsedInput.ImplFile(Ast.ParsedImplFileInput(_name, _isScript, _fileName, _scopedPragmas, _hashDirectives, [m], _)) ] -> 
//        match m with
//        | Ast.SynModuleOrNamespace([name], _isModule, _decls, _xmldoc, _attributes, _access, _range) ->
//            assertEqual name.idText "Bar"
//        | x -> 
//            Assert.Fail (sprintf "Expected empty module named Bar got %+A" x)
//    | asts -> 
//        Assert.Fail(sprintf "Single ImplFile node expected, but was %A" asts)
//
//[<Test>]
//let ``ProcessParseTree should react on cancellation``() =
//    use f1 = tempSource "module Foo"
//    let cts = new CancellationTokenSource()
//    cts.Cancel()
//    let actual = processParseTree [| f1.FilePath|] Map.empty cts.Token
//    assertTrue (actual.Length = 0) 