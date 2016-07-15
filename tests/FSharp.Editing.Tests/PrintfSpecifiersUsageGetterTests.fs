#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharp.Editing.Tests.PrintfSpecifiersUsageGetterTests
#endif

open NUnit.Framework
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.Features

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
            PrintfSpecifiersUsageGetter.getAll results ignore
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

[<Test>]
let ``class property initialiazer``() =
    """
System.Exception(message = sprintf "%d" 1) |> ignore
"""
    => [2, [(36, 38), (40, 41)]]

[<Test>]
let ``class property initialiazer with infix to forward pipe``() =
    """
System.Exception(message = (1 * 1 |> sprintf "%d")) |> ignore
"""
    => [2, [(46, 48), (28, 33)]]

[<Test>]
let ``equality to forward piped``() =
    """
1 = 1 |> printf "%b"
"""
    => [2, [(17, 19), (0, 5)]]

[<Test>]
let ``constructor parameter with piped equality``() =
    """
System.Exception(1 = 1 |> sprintf "%b") |> ignore
"""
    => [2, [(35, 37), (17, 22)]]

[<Test>]
let ``constructor argument with greater than``() =
    """
System.Exception(("" > sprintf "%d" 1).ToString())
"""
    => [2, [(32, 34), (36, 37)]]

[<Test>]
let ``constructor argument with addition than``() =
    """
System.Exception(("" + sprintf "%d" 1).ToString())
"""
    => [2, [(32, 34), (36, 37)]]

[<Test>]
let ``division to forward piped``() =
    """
1 / 1 |> printf "%d"
"""
    => [2, [(17, 19), (0, 5)]]

[<Test>]
let ``multiply and division to forward piped``() =
    """
1 * 1 / 1 |> printf "%d"
"""
    => [2, [(21, 23), (0, 9)]]

[<Test>]
let ``forward pipe 2 deconstructs tuple``() =
    """
(1, 2) ||> sprintf "%d %d"
"""
    => [2, [(20, 22), (1, 2)
            (23, 25), (4, 5)]]

[<Test>]
let ``forward pipe 3 deconstructs tuple``() =
    """
(1, 2, 3) |||> sprintf "%d %d %d"
"""
    => [2, [(24, 26), (1, 2)
            (27, 29), (4, 5)
            (30, 32), (7, 8)]]

[<Test>]
let ``forward pipe with right hand argument``() =
    """
2 |> sprintf "%d %d" 1
"""
    => [2, [(14, 16), (21, 22)
            (17, 19), (0, 1)]]

[<Test>]
let ``forward pipe with multiple right hand arguments``() =
    """
3 |> sprintf "%d %d %d" 1 2
"""
    => [2, [(14, 16), (24, 25)
            (17, 19), (26, 27)
            (20, 22), (0, 1)]]

[<Test>]
let ``forward pipe 2 deconstructs tuple with right hand argument``() =
    """
(2, 3) ||> sprintf "%d %d %d" 1
"""
    => [2, [(20, 22), (30, 31)
            (23, 25), (1, 2)
            (26, 28), (4, 5)]]

[<Test>]
let ``forward pipe 3 deconstructs tuple with right hand argument``() =
    """
(2, 3, 4) |||> sprintf "%d %d %d %d" 1
"""
    => [2, [(24, 26), (37, 38)
            (27, 29), (1, 2)
            (30, 32), (4, 5)
            (33, 35), (7, 8)]]

[<Test>]
let ``too many args should ignore extra args``() =
    """
printf "%d" 1 2
"""
    => [2, [(8, 10), (12, 13)]]

[<Test>]
let ``too many args with forward-pipe should ignore extra args``() =
    """
3 |> printf "%d" 1 2
"""
    => [2, [(13, 15), (17, 18)]]

[<Test>]
let ``too many args with forward-pipe 2 should ignore extra args``() =
    """
(2, 3) ||> printf "%d %d" 1
"""
    => [2, [(19, 21), (26, 27)
            (22, 24), (1, 2)]]

[<Test>]
let ``backwards pipe with equal specifiers and args``() =
    """
ignore <| sprintf "%d" 1
"""
    => [2, [(19, 21), (23, 24)]]

[<Test>]
let ``backwards pipe with too many args``() =
    """
ignore <| sprintf "%d" 1 2
"""
    => [2, [(19, 21), (23, 24)]]

[<Test>]
let ``zero-arity specifiers mixed with normal``() =
    """
printf "%% %d %% %% %f %% %s %% %A" 1 2.5 "x"
"""
    => [2, [(11, 13), (36, 37)
            (20, 22), (38, 41)
            (26, 28), (42, 45)]]

[<Test>]
let ``two-arity specifiers mixed with normal``() =
    """
printf "%*% %d %*f %.*f %f %*s %s %*d" 1 2 3 3.3 4 4.4 5.5 6 "6"
"""
    => [2, [(8, 11), (39, 40)
            (12, 14), (41, 42)
            (15, 18), (43, 48)
            (19, 23), (49, 54)
            (24, 26), (55, 58)
            (27, 30), (59, 64)]]

[<Test>]
let ``three-arity specifiers mixed with normal``() =
    """
printf "%d %*.*f %*.*f %f %*.*f %s" 1 2 2 2.2 3 3 3.3 4.4 5 5 5.5 "6"
"""
    => [2, [(8, 10), (36, 37)
            (11, 16), (38, 45)
            (17, 22), (46, 53)
            (23, 25), (54, 57)
            (26, 31), (58, 65)
            (32, 34), (66, 69)]]

[<Test>]
let ``zero- one- two- and three- arity specifiers together increasing``() =
    """
printf "%% %d %a %*.*f" 1 (fun _ _ -> ()) "2" 3 3 3.3
"""
    => [2, [(11, 13), (24, 25)
            (14, 16), (26, 45)
            (17, 22), (46, 53)]]

[<Test>]
let ``zero- one- two- and three- arity specifiers together decreasing``() =
    """
printf "%*.*f %a %d %%" 1 1 1.1 (fun _ _ -> ()) "2" 3
"""
    => [2, [(8, 13), (24, 31)
            (14, 16), (32, 51)
            (17, 19), (52, 53)]]

[<Test>]
let ``two-arity specifier missing one arg``() =
    """
printf "%*d" 1
"""
    => [2, [(8, 11), (13, 14)]]

[<Test>]
let ``three-arity specifier missing one arg``() =
    """
printf "%*.*f" 1 1
"""
    => [2, [(8, 13), (15, 18)]]

[<Test>]
let ``three-arity specifier missing two args``() =
    """
printf "%*.*f" 1
"""
    => [2, [(8, 13), (15, 16)]]
