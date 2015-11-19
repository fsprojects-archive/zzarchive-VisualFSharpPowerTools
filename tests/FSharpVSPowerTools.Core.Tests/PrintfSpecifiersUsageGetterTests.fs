#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.PrintfSpecifiersUsageGetterTests
#endif

open NUnit.Framework
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

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

let (=>) source (expected: (int * ((int * int) list)) list) = 
    let opts = opts source
    let results = 
        languageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.No)
        |> Async.RunSynchronously

    let actual = 
        PrintfSpecifiersUsageGetter.getAll results
        |> Async.RunSynchronously
        |> Option.getOrElse [||]
        |> Array.toList
        |> List.collect (fun x -> [x.SpecifierRange; x.ArgumentRange])
        |> List.groupBy (fun r -> r.StartLine)
        |> List.map (fun (line, rs) ->
            line, 
            rs 
            |> List.map (fun x -> x.StartColumn, x.EndColumn)
            |> List.sort)
        |> List.sortBy (fun (line, _) -> line)

    let expected = 
        expected 
        |> List.map (fun (line, sus) -> line, List.sort sus)
        |> List.sortBy (fun (line, _) -> line)
    
    try actual |> Collection.assertEquiv expected
    with _ -> 
        debug "AST: %A" results.ParseTree
        for x in actual do
            debug "Actual: %A" x
        reraise()

[<Test>]
let ``should find usages in printf``() =
    """
let _ = printf "%+A foo %d" 1 2
"""
    => [2, [16, 18; 20, 21]]