module FSharpVSPowerTools.Core.Tests.SourceCodeClassifierTests

open System.IO
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier

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
let languageService = LanguageService(fun _ -> ())
let opts = 
    languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, args, [||], framework)
    |> Async.RunSynchronously

let checkCategories line (expected: (Category * int * int) list)  = 
    let symbolsUses =
        languageService.GetAllUsesOfAllSymbolsInFile (opts, fileName, source, AllowStaleResults.MatchingSource) 
        |> Async.RunSynchronously

    let lexer = 
        { new ILexer with
            member x.GetSymbolAtLocation line col =
                let lineStr = sourceLines.[line]
                Lexer.getSymbol source line col lineStr args Lexer.queryLexState 
            member x.TokenizeLine line =
                let lineStr = sourceLines.[line]
                Lexer.tokenizeLine source args line lineStr Lexer.queryLexState }

    let parseResults = 
        languageService.ParseFileInProject(opts, fileName, source) |> Async.RunSynchronously

    SourceCodeClassifier.getCategoriesAndLocations (symbolsUses, parseResults.ParseTree, lexer)
    |> Array.choose (fun loc -> 
        match loc.Category with 
        | Category.Other -> None
        | _ when loc.WordSpan.Line = line ->
            let span = loc.WordSpan
            Some (loc.Category, span.StartCol, span.EndCol)
        | _ -> None)
    |> Array.toList
    |> List.sortBy (fun (_, line, col) -> line, col)
    |> Collection.assertEquiv expected

[<Test>]
let ``module value``() = 
    checkCategories 3 []
    checkCategories 6 []

[<Test>]
let ``module function``() = checkCategories 4 [ Category.Function, 4, 18 ]

[<Test>]
let ``module higher order function``() = 
    checkCategories 5 [ Category.Function, 24, 28; Category.Function, 34, 38; Category.Function, 4, 23 ]

[<Test>]
let ``class let value``() = checkCategories 11 []

[<Test>]
let ``class let function``() = checkCategories 12 [ Category.Function, 8, 24 ]

[<Test>]
let ``class method``() = checkCategories 13 [ Category.Function, 13, 19 ]

[<Test>]
let ``class property``() = checkCategories 14 []

[<Test>]
let ``static method``() = checkCategories 15 [ Category.Function, 18, 30 ]

[<Test>]
let ``static property``() = checkCategories 16 []

[<Test>]
let ``event``() = 
    checkCategories 9 [ Category.ReferenceType, 16, 21 ]
    checkCategories 17 []

[<Test>]
let ``static event``() = 
    checkCategories 10 [ Category.ReferenceType, 29, 34 ]
    checkCategories 18 []

[<Test>]
let ``constructors``() = 
    checkCategories 8 [ Category.ReferenceType, 5, 10 ]
    checkCategories 19 [ Category.ValueType, 12, 15; Category.ReferenceType, 23, 28 ]

[<Test>]
let ``interface implemented in a class``() = 
    checkCategories 20 [ Category.ReferenceType, 21, 32 ]
    checkCategories 21 [ Category.Function, 17, 24 ]

[<Test>]
let ``property with explicit accessors``() = 
    checkCategories 22 [] 
    checkCategories 23 []
    checkCategories 24 [ Category.ValueType, 31, 34 ]

[<Test>]
let ``fully qualified CLI type constructor``() = 
    checkCategories 26 [ Category.ReferenceType, 30, 39 ]

[<Test>]
let ``fully qualified F# type constructor``() = 
    checkCategories 32 [ Category.Module, 18, 20; Category.Module, 15, 17; Category.ReferenceType, 21, 25 ]

[<Test>]
let ``generic class declaration``() = 
    checkCategories 34 [ Category.ReferenceType, 5, 17 ]

[<Test>]
let ``generic class instantiation``() = 
    checkCategories 35 [ Category.ReferenceType, 24, 36; Category.ValueType, 37, 40 ]
    checkCategories 36 [ Category.ReferenceType, 35, 47; Category.Module, 51, 53; Category.Module, 48, 50; Category.ReferenceType, 54, 58 ]
    checkCategories 37 [ Category.ReferenceType, 28, 40; Category.ValueType, 48, 56 ]

[<Test>]
let ``record``() = 
    checkCategories 39 
        [ 
            Category.ReferenceType, 5, 11
            Category.ValueType, 26, 29
            Category.Module, 49, 51; Category.Module, 46, 48; Category.ReferenceType, 52, 56 
        ]

[<Test>]
let ``value type``() = 
    checkCategories 41 [ Category.ValueType, 27, 30 ]
    checkCategories 42 [ Category.ValueType, 22, 27 ]
    checkCategories 43 [ Category.ValueType, 34, 42 ]
    checkCategories 44 [ Category.ValueType, 5, 18 ]
    checkCategories 45 [ Category.ValueType, 5, 30; Category.ValueType, 33, 46 ]
    checkCategories 46 [ Category.ValueType, 20, 33 ] 
    checkCategories 47 [ Category.ValueType, 31, 56; Category.ValueType, 59, 84 ]

[<Test>]
let ``DU case of function``() =
    checkCategories 49 [ Category.ReferenceType, 5, 19; Category.PatternCase, 22, 30; Category.ReferenceType, 35, 39; Category.ReferenceType, 43, 47 ]
    checkCategories 50 [ Category.PatternCase, 5, 13; Category.Function, 14, 22; Category.PatternCase, 26, 34 ]
    checkCategories 51 [ Category.PatternCase, 6, 14; Category.PatternCase, 34, 42; Category.Function, 43, 47; Category.Function, 51, 55 ]

[<Test>]
let ``double quoted function without spaces``() = checkCategories 52 [ Category.Function, 4, 45 ]

[<Test>]
let ``double quoted function with spaces``() = checkCategories 53 [ Category.Function, 4, 42 ]

[<Test>]
let ``fully qualified attribute``() = checkCategories 54 [ Category.ReferenceType, 21, 36 ]

[<Test>]
let ``async type``() = checkCategories 56 [ Category.Function, 4, 16; Category.ReferenceType, 19, 24; Category.Function, 25, 41 ]

[<Test>]
let ``standard computation expression``() = 
    checkCategories 57 []
    checkCategories 58 [ Category.Function, 8, 12 ]
    checkCategories 59 [ Category.Function, 10, 14 ]

[<Test>]
let ``user defined computation expression``() = checkCategories 68 []

[<Test>]
let ``method chain``() =
    checkCategories 69 [ Category.ReferenceType, 15, 26; Category.Function, 39, 46 ]
    
[<Test>]
let ``complex method chain``() =
    checkCategories 70 
        [ Category.ValueType, 15, 19; Category.Function, 20, 27; Category.Function, 30, 38; Category.Function, 44, 53 ]

[<Test>]
let ``generic type with ignored type parameter``() = checkCategories 71 [ Category.ReferenceType, 8, 12 ]

[<Test>]
let ``F# namespace``() = checkCategories 72 [ Category.ReferenceType, 37, 41; Category.ValueType, 42, 45 ]
       
[<Test>]
let ``double quoted member``() = checkCategories 75 [ Category.Function, 12, 25; Category.Function, 28, 37 ]

[<Test>]
let ``indexer``() = checkCategories 77 [ Category.Module, 11, 12; Category.Function, 11, 12 ]

[<Test>]
let ``mutable value``() = checkCategories 78 [ Category.MutableVar, 12, 24 ]

[<Test>]
let ``mutable field``() = 
    checkCategories 80 [ Category.MutableVar, 14, 26; Category.ValueType, 28, 31 ]
    checkCategories 82 [ Category.MutableVar, 16, 28 ]
    checkCategories 84 [ Category.MutableVar, 16, 31 ]

[<Test>]
let ``reference value``() = 
    checkCategories 86 [ Category.MutableVar, 4, 12; Category.Function, 15, 18 ]
    checkCategories 87 [ Category.MutableVar, 0, 8; Category.MutableVar, 13, 21 ]

[<Test>]
let ``reference field``() = 
    checkCategories 89 [ Category.Function, 19, 22; Category.MutableVar, 8, 16 ]
    checkCategories 90 [ Category.MutableVar, 13, 21 ]
    checkCategories 92 [ Category.MutableVar, 6, 11; Category.ValueType, 13, 16; Category.ReferenceType, 17, 20 ]

[<Test>]
let ``single line quotation``() = checkCategories 93 [ Category.Quotation, 8, 19 ]

[<Test>]
let ``multi line quotation``() = 
    checkCategories 94 [ Category.Quotation, 8, 16 ]
    checkCategories 95 [ Category.Quotation, 11, 22 ]

[<Test>]
let ``quotation as function argument``() = 
    checkCategories 96 [ Category.Function, 8, 10; Category.Quotation, 11, 22 ]
    checkCategories 98 [ Category.Function, 8, 9; Category.Quotation, 10, 21; Category.Quotation, 22, 33 ]
    checkCategories 123 [ Category.Quotation, 6, 16 ]
    checkCategories 129 [ Category.Function, 8, 11; Category.Quotation, 16, 23 ]

[<Test>]
let ``quotation in type``() = 
    checkCategories 100 [ Category.Quotation, 12, 23 ]
    checkCategories 101 [ Category.Function, 13, 14; Category.Quotation, 19, 30 ]
    checkCategories 102 [ Category.Quotation, 17, 28 ]

[<Test>]
let ``untyped quotation``() = checkCategories 103 [ Category.Quotation, 8, 17 ]

[<Test>]
let ``complicated quotation layout``() = 
    checkCategories 104 [ Category.Function, 9, 10; Category.Quotation, 11, 15 ]
    checkCategories 105 [ Category.Quotation, 14, 17 ]
    checkCategories 106 [ Category.Quotation, 14, 20; Category.Quotation, 21, 30 ]

[<Test>]
let ``quotation in lambda``() = checkCategories 107 [ Category.Quotation, 17, 24 ]

[<Test>]
let ``quotation in record``() = checkCategories 109 [ Category.Quotation, 18, 25 ]

[<Test>]
let ``quotation in list expression``() = checkCategories 110 [ Category.Quotation, 10, 17 ]

[<Test>]
let ``quotation in seq for expression``() = checkCategories 111 [ Category.Quotation, 34, 41 ]

[<Test>]
let ``quotation as a result of function``() = checkCategories 116 [ Category.Quotation, 4, 11 ]

[<Test>]
let ``quotation as default constructor arguments``() = 
    checkCategories 118 [ Category.ReferenceType, 8, 39; Category.Quotation, 40, 47 ]

[<Test>]
let ``quotation as initialization of auto property``() = 
    checkCategories 125 [ Category.MutableVar, 15, 19; Category.Quotation, 22, 31 ]

[<Test>]
let ``quotation in property setter``() = checkCategories 127 [ Category.Quotation, 31, 40 ]

[<Test>]
let ``quotation in nested module``() = checkCategories 131 [ Category.Quotation, 12, 19 ]

[<Test>]
let ``quotation inside computation expression``() =
    checkCategories 166 [ Category.Quotation, 16, 23 ]
    checkCategories 167 [ Category.Function, 11, 17; Category.Quotation, 18, 25 ]
    checkCategories 168 [ Category.Function, 17, 20; Category.Quotation, 21, 28 ]
    checkCategories 170 [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
    checkCategories 172 [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
    checkCategories 173 [ Category.Function, 12, 19; Category.Quotation, 20, 28 ]
    checkCategories 174 [ Category.Quotation, 14, 21 ]
    checkCategories 177 [ Category.Quotation, 19, 26 ]
    checkCategories 179 [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]

[<Test>]
let ``tuple alias``() = 
    checkCategories 132 [ Category.ReferenceType, 5, 10; Category.ValueType, 13, 16; Category.ReferenceType, 19, 25 ]    
    checkCategories 133 [ Category.Function, 4, 13; Category.ReferenceType, 18, 23; Category.ReferenceType, 27, 32 ]

[<Test>]
let ``multiline method chain``() = 
    checkCategories 136 [ Category.Function, 9, 18 ]
    checkCategories 137 [ Category.Function, 9, 13; Category.Function, 16, 22 ]

[<Test>]
let ``module``() = 
    checkCategories 1 [ Category.Module, 7, 14 ]
    checkCategories 138 [ Category.Module, 7, 14 ]
    checkCategories 139 [ Category.Module, 11, 18 ]

[<Test>]
let ``static CLR class``() = checkCategories 141 [ Category.ReferenceType, 20, 30; Category.Function, 31, 36 ]

[<Test>]
let ``F# external modules``() = 
    checkCategories 142 
        [
            Category.Module, 15, 18; Category.Function, 19, 23
            Category.Module, 27, 30; Category.Function, 31, 37
            Category.Module, 41, 45; Category.Function, 46, 49  
        ]

[<Test>]
let ``byref argument``() = 
    checkCategories 143 
        [ Category.Function, 4, 27
          Category.ReferenceType, 32, 37
          Category.ValueType, 38, 41 ]

[<Test>]
let ``unit of measure``() =
    checkCategories 145 [ Category.ValueType, 7, 10; Category.ReferenceType, 11, 13 ]
    checkCategories 146 [ Category.ReferenceType, 6, 8 ]
    checkCategories 148 [ Category.ValueType, 14, 17; Category.ReferenceType, 18, 20 ]

[<Test>]
let ``custom numeric literal``() = 
    checkCategories 149 []    
    checkCategories 152 []

[<Test>]
let ``anonymous generic parameters``() =
    checkCategories 154 [ Category.Function, 8, 10; Category.ReferenceType, 16, 19; Category.ReferenceType, 34, 37 ]
    checkCategories 155 [ Category.Function, 8, 11; Category.ReferenceType, 16, 19; Category.ReferenceType, 34, 37 ] 
    checkCategories 156 [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
    checkCategories 157 [ Category.Function, 8, 10; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ] 
    checkCategories 158 [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
    checkCategories 159 [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
    checkCategories 160 [ Category.Function, 8, 9; Category.ReferenceType, 42, 46; Category.ReferenceType, 83, 87 ]

[<Test>]
let ``array alias``() =
    checkCategories 161 [ Category.ReferenceType, 5, 15; Category.ValueType, 18, 22 ]

[<Test; Ignore "Lexer cannot recognize (|P|_|) as an Ident at position of the last bar">]
let ``active pattern``() =
    checkCategories 181 [ Category.PatternCase, 6, 19; Category.PatternCase, 28, 32 ]
    checkCategories 182 [ Category.Function, 8, 27 ]