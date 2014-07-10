module FSharpVSPowerTools.Core.Tests.SourceCodeClassifierTests

open System.IO
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier

let fileName = Path.Combine(__SOURCE_DIRECTORY__, "Coloring.fs") //"c:\\File.fs"
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
//let opts() = LanguageServiceTestHelper.projectOptions fileName
let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let (=>) source (expected: (int * ((Category * int * int) list)) list) = 
    let opts = opts source
    let symbolsUses =
        languageService.GetAllUsesOfAllSymbolsInFile (opts, fileName, source, AllowStaleResults.No, true,
                                                      (fun _ -> async { return Some [opts] }))
        |> Async.RunSynchronously

    let sourceLines = source.Split([|"\n"|], System.StringSplitOptions.None)

    let lexer = 
        { new LexerBase() with
            member x.GetSymbolFromTokensAtLocation (_tokens, line, col) =
                let lineStr = sourceLines.[line]
                Lexer.getSymbol source line col lineStr args Lexer.queryLexState
            member x.TokenizeLine line =
                let lineStr = sourceLines.[line]
                Lexer.tokenizeLine source args line lineStr Lexer.queryLexState }

    let parseResults = 
        languageService.ParseFileInProject(opts, fileName, source) |> Async.RunSynchronously

    let actualCategories =
        SourceCodeClassifier.getCategoriesAndLocations (symbolsUses, parseResults.ParseTree, lexer)
        |> Seq.groupBy (fun span -> span.WordSpan.Line)

    let actual =
        expected
        |> List.map (fun (line, _) ->
            match actualCategories |> Seq.tryFind (fun (actualLine, _) -> actualLine = line) with
            | Some (_, spans) -> 
                line,
                spans
                |> Seq.choose (fun span ->
                    match span.Category with 
                    | Category.Other -> None
                    | _ -> Some (span.Category, span.WordSpan.StartCol, span.WordSpan.EndCol))
                |> Seq.sortBy (fun (_, startCol, _) -> startCol)
                |> Seq.toList
            | None -> line, [])
        |> List.sortBy (fun (line, _) -> line)
    let expected = expected |> List.map (fun (line, spans) -> line, spans |> List.sortBy (fun (_, startCol, _) -> startCol))
    try actual |> Collection.assertEquiv (expected |> List.sortBy (fun (line, _) -> line))
    with _ -> debug "AST: %A" parseResults.ParseTree; reraise()

let checkLine line expected = ()

[<Test>]
let ``module value``() = 
    """
let moduleValue = 1
"""
    => [2, []]

[<Test>]
let ``module function``() = 
    """
let moduleFunction x = x + 1
"""
    => [ 2, [ Category.Function, 4, 18 ]]

[<Test>]
let ``module higher order function``() = 
    """
let higherOrderFunction func x = (func x) - 1
"""
   => [ 2, [ Category.Function, 24, 28; Category.Function, 34, 38; Category.Function, 4, 23 ]]

[<Test>]
let ``class let value``() = 
    """
type Class() =
    let value = 1
    member x.M = value
"""
    => [ 3, []]

[<Test>]
let ``class let function``() = 
    """
type Class() =
    let classLetFunction x = x
    member x.M = classLetFunction 1
"""
    => [ 3, [ Category.Function, 8, 24 ]]

[<Test>]
let ``class method``() = 
    """
type Class() =
    member __.Method _ = ()
"""
   => [ 3, [ Category.Function, 14, 20 ]]

[<Test>]
let ``class property``() = 
 """
type Class() =
    member __.Prop = ()
"""
   => [ 3, []]

[<Test>]
let ``static method``() = 
    """
type Class() =
    static member Method _ = ()
"""
    => [3, [ Category.Function, 18, 24 ]]

[<Test>]
let ``static property``() = 
    """
type Class() =
    static member StaticProperty = 1
"""
    => [ 3, []]

[<Test>]
let ``event``() = 
    """
type Class() =
    let event = Event<_>()
    member __.Event = event.Publish
"""
    => [ 3, [ Category.ReferenceType, 16, 21 ]
         4, []]

[<Test>]
let ``static event``() = 
    """
type Class() =
    static let staticEvent = Event<_>()
    static member StaticEvent = staticEvent.Publish
"""
    => [ 3, [ Category.ReferenceType, 29, 34 ]
         4, []]

[<Test>]
let ``constructors``() = 
    """
type Class() =
    new (_: int) = new Class()
"""
    => [ 2, [ Category.ReferenceType, 5, 10 ]
         3, [ Category.ValueType, 12, 15; Category.ReferenceType, 23, 28 ]]

[<Test>]
let ``interface implemented in a class``() = 
    """
type Class() =
    interface System.IDisposable with
        member __.Dispose() = ()
"""
    => [ 3, [ Category.ReferenceType, 21, 32 ]
         4, [ Category.Function, 18, 25 ]]

[<Test>]
let ``property with explicit accessors``() = 
    """
type Class() =
    member __.PropWithGetterAndSetter 
                with get() = 1 
                and set(_: int) = ()
"""
    => [ 3, [] 
         4, []
         5, [ Category.ValueType, 27, 30 ]]

[<Test>]
let ``fully qualified CLI type constructor``() = 
    """
let dateTime = new System.Net.WebClient()
"""
    => [ 2, [ Category.ReferenceType, 30, 39 ]]

[<Test>]
let ``fully qualified F# type constructor``() = 
    """
module M1 =
    module M2 =
        type Type() = class end

let m1m2Type = M1.M2.Type()
"""
    => [ 6, [ Category.Module, 18, 20; Category.Module, 15, 17; Category.ReferenceType, 21, 25 ]]

[<Test>]
let ``generic class declaration``() = 
    """
type GenericClass<'T>() = class end
"""
    => [ 2, [ Category.ReferenceType, 5, 17 ]]

[<Test>]
let ``generic class instantiation``() = 
    """
module M1 =
    module M2 =
        type Type() = class end
type GenericClass<'T>() = class end
let genericClassOfInt = GenericClass<int>()
let genericClassOfUserFSharpType = GenericClass<M1.M2.Type>()
let genericClassOfCLIType = GenericClass<System.DateTime>()
"""
    => [ 6, [ Category.ReferenceType, 24, 36; Category.ValueType, 37, 40 ]
         7, [ Category.ReferenceType, 35, 47; Category.Module, 51, 53; Category.Module, 48, 50; Category.ReferenceType, 54, 58 ]
         8, [ Category.ReferenceType, 28, 40; Category.ValueType, 48, 56 ]]

[<Test>]
let ``record``() = 
    """
module M1 =
    module M2 =
        type Type() = class end
type Record = { IntField: int; UserTypeField: M1.M2.Type }
"""
    => [ 5, [ Category.ReferenceType, 5, 11
              Category.ValueType, 26, 29
              Category.Module, 49, 51; Category.Module, 46, 48; Category.ReferenceType, 52, 56 ]]

//[<Test>]
//let ``value type``() = 
//    checkLine 41 [ Category.ValueType, 27, 30 ]
//    checkLine 42 [ Category.ValueType, 22, 27 ]
//    checkLine 43 [ Category.ValueType, 34, 42 ]
//    checkLine 44 [ Category.ValueType, 5, 18 ]
//    checkLine 45 [ Category.ValueType, 5, 30; Category.ValueType, 33, 46 ]
//    checkLine 46 [ Category.ValueType, 20, 33 ] 
//    checkLine 47 [ Category.ValueType, 31, 56; Category.ValueType, 59, 84 ]
//
//[<Test>]
//let ``DU case of function``() =
//    checkLine 49 [ Category.ReferenceType, 5, 19; Category.PatternCase, 22, 30; Category.ReferenceType, 35, 39; Category.ReferenceType, 43, 47 ]
//    checkLine 50 [ Category.PatternCase, 5, 13; Category.Function, 14, 22; Category.PatternCase, 26, 34 ]
//    checkLine 51 [ Category.PatternCase, 6, 14; Category.PatternCase, 34, 42; Category.Function, 43, 47; Category.Function, 51, 55 ]
//
//[<Test>]
//let ``double quoted function without spaces``() = checkLine 52 [ Category.Function, 4, 45 ]
//
//[<Test>]
//let ``double quoted function with spaces``() = checkLine 53 [ Category.Function, 4, 42 ]
//
//[<Test>]
//let ``fully qualified attribute``() = checkLine 54 [ Category.ReferenceType, 21, 36 ]
//
//[<Test>]
//let ``async type``() = checkLine 56 [ Category.Function, 4, 16; Category.ReferenceType, 19, 24; Category.Function, 25, 41 ]
//
//[<Test>]
//let ``standard computation expression``() = 
//    checkLine 57 []
//    checkLine 58 [ Category.Function, 8, 12 ]
//    checkLine 59 [ Category.Function, 10, 14 ]
//
//[<Test>]
//let ``user defined computation expression``() = checkLine 68 []
//
//[<Test>]
//let ``method chain``() =
//    checkLine 69 [ Category.ReferenceType, 15, 26; Category.Function, 39, 46 ]
//    
//[<Test>]
//let ``complex method chain``() =
//    checkLine 70 
//        [ Category.ValueType, 15, 19; Category.Function, 20, 27; Category.Function, 30, 38; Category.Function, 44, 53 ]
//
//[<Test>]
//let ``generic type with ignored type parameter``() = checkLine 71 [ Category.ReferenceType, 8, 12 ]
//
//[<Test>]
//let ``F# namespace``() = checkLine 72 [ Category.ReferenceType, 37, 41; Category.ValueType, 42, 45 ]
//       
//[<Test>]
//let ``double quoted member``() = checkLine 75 [ Category.Function, 12, 25; Category.Function, 28, 37 ]
//
//[<Test>]
//let ``indexer``() = checkLine 77 [ Category.Module, 11, 12 ] 
//
//[<Test>]
//let ``mutable value``() = checkLine 78 [ Category.MutableVar, 12, 24 ]
//
//[<Test>]
//let ``mutable field``() = 
//    checkLine 80 [ Category.MutableVar, 14, 26; Category.ValueType, 28, 31 ]
//    checkLine 82 [ Category.MutableVar, 16, 28 ]
//    checkLine 85 [ Category.MutableVar, 16, 31; Category.MutableVar, 39, 54 ]
//
//[<Test>]
//let ``reference value``() = 
//    checkLine 86 [ Category.MutableVar, 4, 12; Category.Function, 15, 18 ]
//    checkLine 87 [ Category.MutableVar, 0, 8; Category.MutableVar, 13, 21 ]
//
//[<Test>]
//let ``reference field``() = 
//    checkLine 89 [ Category.Function, 19, 22; Category.MutableVar, 8, 16 ]
//    checkLine 90 [ Category.MutableVar, 13, 21 ]
//    checkLine 92 [ Category.MutableVar, 6, 11; Category.ValueType, 13, 16; Category.ReferenceType, 17, 20 ]
//
//[<Test>]
//let ``single line quotation``() = checkLine 93 [ Category.Quotation, 8, 19 ]
//
//[<Test>]
//let ``multi line quotation``() = 
//    checkLine 94 [ Category.Quotation, 8, 16 ]
//    checkLine 95 [ Category.Quotation, 11, 22 ]
//
//[<Test>]
//let ``quotation as function argument``() = 
//    checkLine 96 [ Category.Function, 8, 10; Category.Quotation, 11, 22 ]
//    checkLine 98 [ Category.Function, 8, 9; Category.Quotation, 10, 21; Category.Quotation, 22, 33 ]
//    checkLine 123 [ Category.Quotation, 6, 16 ]
//    checkLine 129 [ Category.Function, 8, 11; Category.Quotation, 16, 23 ]
//
//[<Test>]
//let ``quotation in type``() = 
//    checkLine 100 [ Category.Quotation, 12, 23 ]
//    checkLine 101 [ Category.Function, 14, 15; Category.Quotation, 20, 31 ]
//    checkLine 102 [ Category.Quotation, 18, 29 ]
//
//[<Test>]
//let ``untyped quotation``() = checkLine 103 [ Category.Quotation, 8, 17 ]
//
//[<Test>]
//let ``complicated quotation layout``() = 
//    checkLine 104 [ Category.Function, 9, 10; Category.Quotation, 11, 15 ]
//    checkLine 105 [ Category.Quotation, 14, 17 ]
//    checkLine 106 [ Category.Quotation, 14, 20; Category.Quotation, 21, 30 ]
//
//[<Test>]
//let ``quotation in lambda``() = checkLine 107 [ Category.Quotation, 17, 24 ]
//
//[<Test>]
//let ``quotation in record``() = checkLine 109 [ Category.Quotation, 18, 25 ]
//
//[<Test>]
//let ``quotation in list expression``() = checkLine 110 [ Category.Quotation, 10, 17 ]
//
//[<Test>]
//let ``quotation in seq for expression``() = checkLine 111 [ Category.Quotation, 34, 41 ]
//
//[<Test>]
//let ``quotation as a result of function``() = checkLine 116 [ Category.Quotation, 4, 11 ]
//
//[<Test>]
//let ``quotation as default constructor arguments``() = 
//    checkLine 118 [ Category.ReferenceType, 8, 39; Category.Quotation, 40, 47 ]
//
//[<Test>]
//let ``quotation as initialization of auto property``() = 
//    checkLine 125 [ Category.MutableVar, 15, 19; Category.Quotation, 22, 31 ]
//
//[<Test>]
//let ``quotation in property setter``() = checkLine 127 [ Category.Quotation, 31, 40 ]
//
//[<Test>]
//let ``quotation in nested module``() = checkLine 131 [ Category.Quotation, 12, 19 ]
//
//[<Test>]
//let ``quotation inside computation expression``() =
//    checkLine 166 [ Category.Quotation, 16, 23 ]
//    checkLine 167 [ Category.Function, 11, 17; Category.Quotation, 18, 25 ]
//    checkLine 168 [ Category.Function, 17, 20; Category.Quotation, 21, 28 ]
//    checkLine 170 [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
//    checkLine 172 [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
//    checkLine 173 [ Category.Function, 12, 19; Category.Quotation, 20, 28 ]
//    checkLine 174 [ Category.Quotation, 14, 21 ]
//    checkLine 177 [ Category.Quotation, 19, 26 ]
//    checkLine 179 [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
//
//[<Test>]
//let ``tuple alias``() = 
//    checkLine 132 [ Category.ReferenceType, 5, 10; Category.ValueType, 13, 16; Category.ReferenceType, 19, 25 ]    
//    checkLine 133 [ Category.Function, 4, 13; Category.ReferenceType, 18, 23; Category.ReferenceType, 27, 32 ]
//
//[<Test>]
//let ``multiline method chain``() = 
//    checkLine 136 [ Category.Function, 9, 18 ]
//    checkLine 137 [ Category.Function, 9, 13; Category.Function, 16, 22 ]
//
//[<Test>]
//let ``module``() = 
//    checkLine 1 [ Category.Module, 7, 14 ]
//    checkLine 138 [ Category.Module, 7, 14 ]
//    checkLine 139 [ Category.Module, 11, 18 ]
//
//[<Test>]
//let ``static CLR class``() = checkLine 141 [ Category.ReferenceType, 20, 30; Category.Function, 31, 36 ]
//
//[<Test>]
//let ``F# external modules``() = 
//    checkLine 142 
//        [
//            Category.Module, 15, 18; Category.Function, 19, 23
//            Category.Module, 27, 30; Category.Function, 31, 37
//            Category.Module, 41, 45; Category.Function, 46, 49  
//        ]
//
//[<Test>]
//let ``byref argument``() = 
//    checkLine 143 
//        [ Category.Function, 4, 27
//          Category.ReferenceType, 32, 37
//          Category.ValueType, 38, 41 ]
//
//[<Test>]
//let ``unit of measure``() =
//    checkLine 145 [ Category.ValueType, 7, 10; Category.ReferenceType, 11, 13 ]
//    checkLine 146 [ Category.ReferenceType, 6, 8 ]
//    checkLine 148 [ Category.ValueType, 14, 17; Category.ReferenceType, 18, 20 ]
//
//[<Test>]
//let ``custom numeric literal``() = 
//    checkLine 149 []    
//    checkLine 152 []
//
//[<Test>]
//let ``anonymous generic parameters``() =
//    checkLine 154 [ Category.Function, 8, 10; Category.ReferenceType, 16, 19; Category.ReferenceType, 34, 37 ]
//    checkLine 155 [ Category.Function, 8, 11; Category.ReferenceType, 16, 19; Category.ReferenceType, 34, 37 ] 
//    checkLine 156 [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
//    checkLine 157 [ Category.Function, 8, 10; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ] 
//    checkLine 158 [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
//    checkLine 159 [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
//    checkLine 160 [ Category.Function, 8, 9; Category.ReferenceType, 42, 46; Category.ReferenceType, 83, 87 ]
//
//[<Test>]
//let ``array alias``() =
//    checkLine 161 [ Category.ReferenceType, 5, 15; Category.ValueType, 18, 22 ]
//
//[<Test; Ignore "Lexer cannot recognize (|P|_|) as an Ident at position of the last bar">]
//let ``active pattern``() =
//    checkLine 181 [ Category.PatternCase, 6, 19; Category.PatternCase, 28, 32 ]
//    checkLine 182 [ Category.Function, 8, 27 ]
//
//[<Test>]
//let ``non public module``() =
//    checkLine 183 [ Category.Module, 15, 28 ]
//
//[<Test>]
//let ``unused non public module function and value``() =
//    checkLine 184 [ Category.Unused, 8, 12 ]  
//    checkLine 185 [ Category.Unused, 8, 13 ]
//
//[<Test>]
//let ``unused default constructor of non public class``() =
//    checkLine 186 [ Category.Unused, 13, 25 ]
//
//[<Test>]
//let ``unused non public class let binding``() =
//    checkLine 188 [ Category.Unused, 8, 16] 
//    checkLine 189 [ Category.Unused, 8, 15]
//
//[<Test>]
//let ``unused non public class member``() =
//    checkLine 190 [ Category.Unused, 22, 26] 
//    checkLine 191 [ Category.Unused, 22, 28]
//
//[<Test>]
//let ``unused self binding``() =
//    checkLine 192 [ Category.Unused, 11, 15; Category.Function, 16, 28 ]
//
//[<Test>]
//let ``used self binding``() =
//    checkLine 196 [ Category.Function, 16, 23 ]
//
//[<Test>]
//let ``unused function / member argument``() =
//    checkLine 193 [ Category.Function, 14, 21; Category.Unused, 23, 27 ]
//    checkLine 198 [ Category.Function, 8, 12; Category.Unused, 13, 17 ]

[<Test>]
let ``unused function / member local binding``() =
    """
type PublicClass() =
    member __.Method() =
        let local = 1
        ()
let func x =
    let local = 1
    x
"""
    => [ 4, [ Category.Unused, 12, 17 ]
         7, [ Category.Unused, 8, 13 ]]

[<Test>]
let ``unused open declaration in top level module``() =
    """
module TopModule
open System
open System.IO
let _ = DateTime.Now
"""
    => [ 3, []
         4, [ Category.Unused, 5, 14 ]]
         
[<Test>]
let ``unused open declaration in namespace``() =
    """
namespace TopNamespace
open System
open System.IO
module Nested =
    let _ = DateTime.Now
"""
    => [ 3, []
         4, [ Category.Unused, 5, 14 ]]
         
[<Test>]
let ``unused open declaration in nested module``() =
    """
namespace TopNamespace
module Nested =
    open System
    open System.IO
    let _ = DateTime.Now
"""
    => [ 4, []
         5, [ Category.Unused, 9, 18 ]]

[<Test>] 
let ``unused open declaration due to partially qualified symbol``() =
    """
module TopModule
open System
open System.IO
let _ = IO.File.Create ""
"""
    => [ 3, []
         4, [ Category.Unused, 5, 14 ]]

[<Test>]
let ``unused parent open declaration due to partially qualified symbol``() =
    """
module TopModule
open System
open System.IO
let _ = File.Create ""
"""
    => [ 3, [ Category.Unused, 5, 11 ]
         4, []]
