module FSharpVSPowerTools.Core.Tests.SourceCodeClassifierTests

open System.IO
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharp.CompilerBinding
open FSharpVSPowerTools.Core.SourceCodeClassifier

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Coloring.fs")
let source = File.ReadAllText(fileName)
let sourceLines = source.Split([|"\n"|], System.StringSplitOptions.None)
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
        vsLanguageService.GetAllUsesOfAllSymbolsInFile
            (projectFileName, fileName, source, sourceFiles, args, framework, AllowStaleResults.MatchingSource) 
        |> Async.RunSynchronously

    let getLexerSymbol line col =
        let lineStr = sourceLines.[line]
        SymbolParser.getSymbol source line col lineStr args SymbolParser.queryLexState

    SourceCodeClassifier.getCategoriesAndLocations (results, getLexerSymbol)
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
    checkCategories 19 [ ReferenceType, 4, 7; ValueType, 12, 15; ReferenceType, 23, 28 ]

[<Test>]
let ``interface implemented in a class``() = 
    checkCategories 20 [ ReferenceType, 21, 32 ]
    checkCategories 21 [ Function, 17, 24 ]

[<Test>]
let ``property with explicit accessors``() = 
    checkCategories 22 [] 
    checkCategories 23 []
    checkCategories 24 [ ValueType, 31, 34 ]

[<Test>]
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
    checkCategories 35 [ ReferenceType, 24, 36; ValueType, 37, 40 ]
    checkCategories 36 [ ReferenceType, 35, 47; ReferenceType, 54, 58 ]
    checkCategories 37 [ ReferenceType, 28, 40; ValueType, 48, 56 ]

[<Test>]
let ``record``() = 
    checkCategories 39 
        [ 
            ReferenceType, 5, 11
            PublicField, 16, 24; ValueType, 26, 29
            PublicField, 31, 44; ReferenceType, 52, 56 
        ]

[<Test>]
let ``value type``() = 
    checkCategories 41 [ ValueType, 27, 30 ]
    checkCategories 42 [ ValueType, 22, 27 ]
    checkCategories 43 [ ValueType, 34, 42 ]
    checkCategories 44 [ ValueType, 5, 18 ]
    checkCategories 45 [ ValueType, 5, 30; ValueType, 33, 46 ]
    checkCategories 46 [ ValueType, 20, 33 ] 
    checkCategories 47 [ ValueType, 31, 56; ValueType, 59, 84 ]

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

[<Test>]
let ``async type``() = checkCategories 56 [ Function, 4, 16; ReferenceType, 19, 24; Function, 25, 41 ]

[<Test>]
let ``standard computation expression``() = 
    checkCategories 57 []
    checkCategories 58 [ Function, 8, 12 ]
    checkCategories 59 [ Function, 10, 14 ]

[<Test>]
let ``user defined computation expression``() = checkCategories 68 []

[<Test>]
let ``method chain``() =
    checkCategories 69 [ ReferenceType, 15, 26; Function, 39, 46 ]
    
[<Test>]
let ``complex method chain``() =
    checkCategories 70 
        [ ValueType, 15, 19; Function, 20, 27; Function, 30, 38; Function, 44, 53 ]

[<Test; Ignore "Bug">]
let ``generic type with ignored type parameter``() = checkCategories 71 [ ReferenceType, 8, 12 ]

[<Test>]
let ``F# namespace``() = checkCategories 72 [ ReferenceType, 37, 41; ValueType, 42, 45 ]
       
[<Test>]
let ``double quoted member``() = checkCategories 75 [ Function, 12, 25; Function, 28, 37 ]

[<Test>]
let ``indexer``() = checkCategories 77 []

[<Test>]
let ``mutable value``() = checkCategories 78 [ MutableVar, 12, 24 ]

[<Test>]
let ``mutable field``() = 
    checkCategories 80 [ MutableVar, 14, 26; ValueType, 28, 31 ]
    checkCategories 82 [ MutableVar, 16, 28 ]
    checkCategories 84 [ MutableVar, 16, 31 ]

