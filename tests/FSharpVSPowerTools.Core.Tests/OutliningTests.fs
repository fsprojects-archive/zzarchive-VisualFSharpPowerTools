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

let (=>) (source: string) (lineRanges: (int * int) list) =
    let ast = (parseSource source).ParseTree
    match ast with
    | Some tree ->
        let multilineRanges =
            getOutliningRanges tree
            |> Seq.filter (fun r -> r.StartLine <> r.EndLine)
            |> Seq.map (fun r -> r.StartLine, r.EndLine)
            |> List.ofSeq
        assertEqual lineRanges multilineRanges
    | None -> failwithf "Expected there to be a parse tree for source:\n%s" ""

[<Test>]
let ``parsing an empty file doesn't emit any ranges``() =
    "" => []

[<Test>]
let ``parsing a module correctly``() =
    """
    module MyModule =
        ()
    """
    => [ (2,3) ]

[<Test>]
let ``parsing a module with multiline function correctly``() =
    """
    module MyModule =
        let foo() =
            foo()
    """
    => [ (2,4); (3,4) ]

[<Test>]
let ``parsing a DU correctly``() =
    """
    type Color =
        | Red
        | Green
        | Blue
    """
    => [ (2,5) ]

[<Test>]
let ``parsing a DU with interface correctly``() =
    """
    type Color =
        | Red
        | Green
        | Blue

        interface IDisposable with
            member __.Dispose() =
                (docEventListener :> IDisposable).Dispose()
    """
    => [ (2,9); (7,9); (8,9) ]

[<Test>]
let ``parsing a record with interface correctly``() =
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
    => [ (2,10); (8,10); (9,10) ]

[<Test>]
let ``parsing a type with a do block correctly``() =
    """
    type Color() =        // 2
        let foo() =
            ()

        do
            foo()
            ()          // 8
    """
    => [ (2,8); (3,4); (6,8) ]

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
    => [ (2,27) // MyModule
         (4,5)  // bar
         (7,15) // Color
         (13,15)
         (14,15)
         (17,27) // MyInnerModule
         (19,27) // RecordColor
         (25,27)
         (26,27)
       ]
