module FSharp.Editing.Tests.NavigableItemsCollectorTests

open NUnit.Framework
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
 
let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpCompilerVersion.FSharp_3_1
let languageService = LanguageService()
                                                    
let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, LanguageServiceTestHelper.args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let parseSource source = 
    async {
        let! res = languageService.ParseFileInProject(opts fileName, fileName, source)
        return res.ParseTree
    }
    |> Async.RunSynchronously
     
let (=>) source expected =
    match parseSource source with
    | None -> failwith "Language service returned no parse tree"
    | Some ast ->
        let actual =
            NavigableItemsCollector.collect fileName ast 
            |> Seq.map (fun x -> x.Name, x.Kind, x.Range.Start.Row, x.Range.Start.Col, x.Range.End.Row, x.Range.End.Col)
            |> Seq.toList
        
        try
            actual |> Collection.assertEquiv expected
        with _ -> 
            debug "AST: %A" ast
            debug "Actual: %A" actual
            reraise()

type Kind = NavigableItemKind

[<Test>]
let ``can collect navigable items of different kinds in a file``() =
    """
module Module1

module Module2 =
    let foo = 1
    let bar x = x
    type Type1 = Type1 of int
"""
    => [ "Module1", Kind.Module, 1, 7, 1, 14
         "Module2", Kind.Module, 3, 7, 3, 14
         "foo", Kind.ModuleValue, 4, 8, 4, 11
         "bar", Kind.ModuleValue, 5, 8, 5, 11
         "Type1", Kind.Type, 6, 9, 6, 14
         "Type1", Kind.UnionCase, 6, 17, 6, 22 ]