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

let (=>) source (expected: (int * (((int * int) * (int * int)) list)) list) = 
    let opts = opts source
    let results = 
        languageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.No)
        |> Async.RunSynchronously

    let actual = ref []
    try
        actual :=
            PrintfSpecifiersUsageGetter.getAll results
            |> Async.RunSynchronously
            |> Option.getOrElse [||]
            |> Array.toList
            |> List.groupBy (fun r -> r.SpecifierRange.StartLine)
            |> List.map (fun (line, rs) ->
                line, 
                rs 
                |> List.map (fun x -> 
                    (x.SpecifierRange.StartColumn, x.SpecifierRange.EndColumn),
                    (x.ArgumentRange.StartColumn, x.ArgumentRange.EndColumn))
                |> List.sort)
            |> List.sortBy (fun (line, _) -> line)

        let expected = 
            expected 
            |> List.map (fun (line, sus) -> line, List.sort sus)
            |> List.sortBy (fun (line, _) -> line)
    
        !actual |> Collection.assertEquiv expected
    with _ -> 
        debug "AST: %A" results.ParseTree
        for x in !actual do
            debug "Actual: %A" x
        reraise()

[<Test>]
let ``simplest case``() =
    """
printf "%+A foo %d" 1 2
"""
    => [2, [(8, 11), (20, 21)
            (16, 18), (22, 23)]]

[<Test>]
let ``printf as an argument of another printf``() =
    """
printf "%+A foo %s" 1 (sprintf "%d" 2)
"""
    => [2, [(8, 11), (20, 21)
            (16, 18), (22, 38)
            (32, 34), (36, 37)]]

[<Test>]
let ``partially applied printf``() =
    """
printf "%+A foo %s" 1
"""
    => [2, [(8, 11), (20, 21)]]

[<Test>]
let ``another function above printf``() =
    """
let f x = x
printf "%+A foo" 1
"""
    => [3, [(8, 11), (17, 18)]]

[<Test>]
let ``another curried function above printf``() =
    """
let f x y = x y
printf "%+A foo" 1
"""
    => [3, [(8, 11), (17, 18)]]

[<Test>]
let ``printf as an argument to another function``() =
    """
System.IO.File.OpenRead (sprintf "file %d" 1) |> ignore
"""
    => [2, [(39, 41), (43, 44)]]

[<Test>]
let ``argument backward piped``() =
    """
failwithf "Not an object: %s" <| "foo"
"""
    => [2, [(26, 28), (33, 38)]]

[<Test>]
let ``argument forward piped``() =
    """
"foo" |> failwithf "Not an object: %s"
"""
    => [2, [(35, 37), (0, 5)]]