module FSharpVSPowerTools.Core.Tests.SourceCodeClassifierTests

open System.IO
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharp.CompilerBinding
open FSharpVSPowerTools.Core.SourceCodeClassifier

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Coloring.fs")
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
let vsLanguageService = FSharp.CompilerBinding.LanguageService(fun _ -> ())

let checkCategories line expected = 
    let results = 
        vsLanguageService.ParseAndCheckFileInProject
            (projectFileName, fileName, source, sourceFiles, args, framework, AllowStaleResults.MatchingSource) 
        |> Async.RunSynchronously
    SourceCodeClassifier.getCategoriesAndLocations (results.GetAllUsesOfAllSymbolsInFile())
    |> Array.choose (fun loc -> 
        match loc.Category with 
        | Other -> None
        | _ when loc.Line <> line -> None
        | _ -> Some (loc.Category, loc.ColumnSpan.Start, loc.ColumnSpan.End))
    |> Array.toList
    |> Collection.assertEquiv expected

[<Test>]
let ``module value``() = 
    checkCategories 3 []
    checkCategories 6 []

[<Test>]
let ``module function``() = checkCategories 4 [ Function, 4, 18 ]

[<Test>]
let ``module higher order function``() = 
    checkCategories 5 [ Function, 24, 28; Function, 34, 38; Function, 4, 23 ]

[<Test>]
let ``class let value``() = checkCategories 11 []

[<Test>]
let ``class let function``() = checkCategories 12 [ Function, 8, 24 ]

[<Test>]
let ``class method``() = checkCategories 13 [ Function, 13, 19 ]

[<Test>]
let ``class property``() = checkCategories 14 []

[<Test>]
let ``static method``() = checkCategories 15 [ Function, 18, 30 ]

[<Test>]
let ``static property``() = checkCategories 16 []

[<Test>]
let ``event``() = 
    checkCategories 9 [ ReferenceType, 16, 21 ]
    checkCategories 17 []

[<Test>]
let ``static event``() = 
    checkCategories 10 [ ReferenceType, 29, 34 ]
    checkCategories 18 []

[<Test>]
let ``constructors``() = 
    checkCategories 8 [ ReferenceType, 5, 10 ]
    checkCategories 19 [ ReferenceType, 4, 7; ReferenceType, 12, 15; ReferenceType, 23, 28 ]

[<Test>]
let ``interface implemented in a class``() = 
    checkCategories 20 [ ReferenceType, 21, 32 ]
    checkCategories 21 [ Function, 17, 24 ]

[<Test>]
let ``property with explicit accessors``() = 
    checkCategories 22 [] 
    checkCategories 23 []
    checkCategories 24 [ ReferenceType, 31, 34 ]

[<Test; Ignore "FCS 0.0.36 does not return FSharpSymbolUse for this case">]
let ``fully qualified CLI type constructor``() = 
    checkCategories 26 [ ReferenceType, 30, 39 ]

[<Test>]
let ``fully qualified F# type constructor``() = 
    checkCategories 32 [ ReferenceType, 21, 25 ]

[<Test>]
let ``generic class declaration``() = 
    checkCategories 34 [ ReferenceType, 5, 17; TypeParameter, 18, 20 ]

[<Test>]
let ``generic class instantiation``() = 
    checkCategories 35 [ ReferenceType, 24, 36; ReferenceType, 37, 40 ]
    checkCategories 36 [ ReferenceType, 35, 47; ReferenceType, 54, 58 ]
    checkCategories 37 [ ReferenceType, 28, 40; ValueType, 48, 56 ]

[<Test>]
let ``record``() = 
    checkCategories 39 
        [ 
            ReferenceType, 5, 11
            PublicField, 16, 24; ReferenceType, 26, 29
            PublicField, 31, 44; ReferenceType, 52, 56 
        ]

[<Test>]
let ``value type``() = 
    checkCategories 41 [ ReferenceType, 27, 30 ] // FCS 0.0.39 bug, should be ValueType instead
    checkCategories 42 [ ValueType, 22, 27 ]
    checkCategories 43 [ ReferenceType, 34, 42 ] // FCS 0.0.39 bug, should be ValueType, 34, 42
    checkCategories 44 [ ValueType, 5, 18 ]
    checkCategories 45 [ ValueType, 5, 30; ValueType, 33, 46 ]
    checkCategories 46 [ ReferenceType, 20, 33 ] // FCS 0.0.39 bug, should be ValueType, 20, 33
    checkCategories 47 [ ValueType, 31, 56; ReferenceType, 59, 84 ] // FCS 0.0.39 bug, should be ValueType, 59, 84 

[<Test>]
let ``DU case of function``() =
    checkCategories 49 [ ReferenceType, 5, 19; PatternCase, 22, 30; ReferenceType, 35, 39; ReferenceType, 43, 47 ]
    checkCategories 50 [ PatternCase, 5, 13; Function, 14, 22; PatternCase, 26, 34 ]
    checkCategories 51 [ PatternCase, 6, 14; PatternCase, 34, 42; Function, 43, 47; Function, 51, 55 ]

[<Test>]
let ``double quoted function without spaces``() = checkCategories 52 [ Function, 4, 45 ]

[<Test>]
let ``double quoted function with spaces``() = checkCategories 53 [ Function, 4, 42 ]

[<Test>]
let ``fully qualified attribute``() = checkCategories 54 [ ReferenceType, 21, 36 ]

[<Test; Ignore "FCS 0.0.39 limitation">]
let ``async type``() = checkCategories 56 [ Function, 4, 16; ReferenceType, 19, 24; Function, 25, 41 ]

[<Test>]
let ``computation expression``() = 
    checkCategories 57 []
    checkCategories 58 [ Function, 8, 12 ]
    checkCategories 59 [ Function, 10, 14 ]
