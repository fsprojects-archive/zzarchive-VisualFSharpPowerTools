#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/UntypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "TestHelpers.fs"
#else
module FSharpVSPowerTools.Core.Tests.UntypedAstUtils.Outlining
#endif

open System.IO
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.UntypedAstUtils.Outlining

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
    languageService.ParseFileInProject(opts fileName, fileName, source)
    |> Async.RunSynchronously

type Line = int
type Col = int

let (=>) (source: string) (expectedRanges: (Line * Col * Line * Col) list) =
    let ast = (parseSource source).ParseTree
    try
        match ast with
        | Some tree ->
            let actualRanges =
                getOutliningRanges tree
                |> Seq.filter (fun r -> r.StartLine <> r.EndLine)
                |> Seq.map (fun r -> r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
                |> List.ofSeq
            CollectionAssert.AreEquivalent (List.sort expectedRanges, List.sort actualRanges)
        | None -> failwithf "Expected there to be a parse tree for source:\n%s" source
    with _ ->
        debug "AST:\n%+A" ast
        reraise()

[<Test>]
let ``empty file``() = "" => []

[<Test>]
let ``nested module``() =
    """
module MyModule =
    ()
"""
    => [ 2, 15, 3, 6 ]

[<Test>]
let ``module with multiline function``() =
    """
module MyModule =
    let foo() =
        foo()
"""
    => [ 2, 15, 4, 13
         3, 13, 4, 13 ]

[<Test>]
let ``DU``() =
    """
type Color =
    | Red
    | Green
    | Blue
"""
    => [ 2, 10, 5, 10 ]

[<Test>]
let ``DU with interface``() =
    """
type Color =
    | Red
    | Green
    | Blue

    interface IDisposable with
        member __.Dispose() =
            (docEventListener :> IDisposable).Dispose()
"""
    => [ 2, 10, 9, 55
         7, 25, 9, 55
         8, 27, 9, 55 ]

[<Test>]
let ``record with interface``() =
    """
type Color =
    { Red: int
        Green: int
        Blue: int 
    }

    interface IDisposable with
        member __.Dispose() =
            (docEventListener :> IDisposable).Dispose()
"""
    => [ 2, 10, 10, 55
         8, 25, 10, 55
         9, 27, 10, 55 ]

[<Test>]
let ``type with a do block``() =
    """
type Color() =   // 2
    let foo() =
        ()

    do
        foo()
        ()       // 8
"""
    => [ 2, 10, 8, 10
         3, 13, 4, 10
         6, 6, 8, 10 ]

[<Test>]
let ``complex outlining test``() =
    """
module MyModule =       // 2
    let foo() = ()
    let bar() =
        ()

    type Color =        // 7
        { Red: int
          Green: int
          Blue: int 
        }

        interface IDisposable with      // 13
            member __.Dispose() =
                (docEventListener :> IDisposable).Dispose()

    module MyInnerModule =              // 17

        type RecordColor =              // 19
            { Red: int
              Green: int
              Blue: int 
            }

            interface IDisposable with  // 25
                member __.Dispose() =
                    (docEventListener :> IDisposable).Dispose()
"""
    => [ 2, 15, 27, 63     // MyModule
         4, 13, 5, 10
         7, 14, 15, 59     // Color
         13, 29, 15, 59
         14, 31, 15, 59
         17, 24, 27, 63    // MyInnerModule
         19, 24, 27, 63    // RecordColor
         25, 33, 27, 63
         26, 35, 27, 63 ]
    
[<Test>]
let ``open statements``() =
    """
open M             // 2
open N             // 3
                   // 4
module M =         // 5
    let x = 1      // 6
                   // 7
    open M         // 8
    open N         // 9
                   // 10
    module M =     // 11
        open M     // 12
                   // 13
        let x = 1  // 14
                   // 15
    module M =     // 16
        open M     // 17
        open N     // 18
        let x = 1  // 19
                   // 20
open M             // 21
open N             // 22
open H             // 23
                   // 24
open G             // 25
open H             // 26
"""
    => [ 2, 5, 3, 6
         5, 8, 19, 17
         8, 9, 9, 10
         11, 12, 14, 17
         16, 12, 19, 17
         17, 13, 18, 14
         21, 5, 23, 6
         25, 5, 26, 6 ]

[<Test>]
let ``hash directives``() =
    """
#r @"a"   // 2
#r "b"    // 3
          // 4
#r "c"    // 5
          // 6
#r "d"    // 7
#r "e"    // 8
let x = 1 // 9
          // 10
#r "f"    // 11
#r "g"    // 12
#load "x" // 13
#r "y"    // 14
          // 15 
#load "a" // 16
      "b" // 17
      "c" // 18
          // 19
#load "a" // 20
      "b" // 21
      "c" // 22
#r "d"    // 23
"""
    => [ 2, 3, 3, 6
         7, 3, 8, 6
         11, 3, 14, 6
         16, 6, 18, 9
         20, 6, 23, 6 ]

[<Test>]
let ``nested let bindings``() =
    """
let f x =       // 2
    let g x =   // 3
        let h = // 4
            ()  // 5
        ()      // 6
    x           // 7
"""
    => [ 2, 7, 7, 5
         3, 11, 6, 10
         4, 13, 5, 14 ]

[<Test>]
let ``match``() =
    """
match None with     // 2
| Some _ ->         // 3
    ()              // 4
| None ->           // 5
    match None with // 6
    | Some _ -> ()  // 7
    | None ->       // 8
        let x = ()  // 9
        ()          // 10
"""
    => [ 2, 15, 10, 10
         6, 19, 10, 10 ]
         
[<Test>]
let ``computation expressions``() =
    """
seq {              // 2
    yield ()       // 3
    let f x =      // 4
        ()         // 5
    yield! seq {   // 6
        yield () } // 7
}                  // 8
"""
    => [ 2, 4, 8, 1
         4, 11, 5, 10
         6, 15, 7, 18 ]

[<Test>]
let ``list``() =
    """
let _ = 
    [ 1; 2
      3 ]
"""
  => [ 2, 5, 4, 9
       3, 6, 4, 7 ]

[<Test>]
let ``object expressions``() =
    """
let _ =
    { new System.IDisposable with
        member __.Dispose() = () }
"""
    => [ 2, 5, 4, 34
         3, 28, 4, 33 ]
         
[<Test>]
let ``try - with``() =
    """
try           // 2
    let f x = // 3
        ()    // 4
with _ ->     // 5
    let f x = // 6
        ()    // 7
    ()        // 8
"""
    => [ 2, 3, 8, 6
         3, 11, 4, 10
         5, 4, 8, 6
         6, 11, 7, 10 ]

[<Test>]
let ``try - finally``() =
    """
try           // 2
    let f x = // 3
        ()    // 4
finally       // 5
    let f x = // 6
        ()    // 7
    ()        // 8
"""
    => [ 2, 3, 8, 6
         3, 11, 4, 10
         5, 7, 8, 6
         6, 11, 7, 10 ]

[<Test>]
let ``if - then - else``() =
    """
if true then
    let f x = 
        ()
    ()
else
    let f x =
        ()
    ()
"""
    => [ 3, 11, 4, 10
         7, 11, 8, 10 ]