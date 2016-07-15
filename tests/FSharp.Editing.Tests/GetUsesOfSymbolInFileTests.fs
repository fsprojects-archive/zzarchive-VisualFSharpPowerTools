#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../bin/FSharpVSPowerTools.Core.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "TestHelpers.fs"
#else
module FSharp.Editing.Tests.GetUsesOfSymbolInFileTests
#endif

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

let (=>) (source, line, col) (expected: (int * ((int * int) list)) list) = 
    let line = line - 1
    let opts = opts source

    let getSymbol line col lineStr =
        Lexer.getSymbol source line col lineStr SymbolLookupKind.Fuzzy LanguageServiceTestHelper.args Lexer.queryLexState
    
    let sourceLines = String.getLines source
    let results = languageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.No) |> Async.RunSynchronously

    let actual = 
        asyncMaybe {
            let! symbol = getSymbol line col sourceLines.[line]
            let! _, _, symbolUses = results.GetUsesOfSymbolInFileAtLocation(line, col, sourceLines.[line], symbol.Text)
            return symbolUses
        }
        |> Async.RunSynchronously
        |> Option.getOrElse [||]
        |> Array.toList
        |> List.groupBy (fun su -> su.RangeAlternate.StartLine)
        |> List.map (fun (line, sus) ->
            line, 
            sus 
            |> List.map (fun su -> su.RangeAlternate.StartColumn, su.RangeAlternate.EndColumn)
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
let ``should find usages of module let bindings``() =
    ("""
let helloWorld = string1 + " " + string2
let substring = helloWorld.[0..6]
let string5 =
    let helloWorld = "Hello F# world"
    helloWorld
""", 2, 5)
    => [2, [4, 14]
        3, [16, 26]]

[<Test>]
let ``should find usages of class members``() =
    ("""
type Vector() =
    member __.Length = 1
let v = Vector()
let _ = v.Length
let _ = v.Length + v.Length
""", 3, 15)
    => [3, [14, 20]
        5, [8, 16]
        6, [8, 16; 19, 27]]

[<Test>]
let ``should find usages of module level events``() =
    ("""
let event = Event<EventHandler, EventArgs>()
event.Publish.AddHandler(EventHandler(fun _ _ -> ()))
event.Trigger(null, EventArgs.Empty)
""", 2, 5) 
    => [2, [4, 9]
        3, [0, 5]
        4, [0, 5]]

[<Test>]
let ``should find usages of symbol defined in a nested module``() =
    ("""
module Nested =
    let foo() = ()
    let _ = foo()
Nested.foo()
""", 3, 9) 
    => [3, [8, 11]
        4, [12, 15]
        5, [0, 10]]

[<Test>]
let ``should find usages of single letter DU case``() =
    ("""
type A = B of int
    let (B b) = B 1
    type C = A list
""", 3, 10) 
    => [2, [9, 10]
        3, [16, 17; 9, 10]]

[<Test>]
let ``should find usages of single letter DU type``() =
    ("""
type A = B of int
    let (B b) = B 1
    type C = A list
""", 2, 6) 
    => [2, [5, 6]
        4, [13, 14]]

[<Test>]
let ``should find usages of operator``() =
    ("""
let ( *. ) x y = x * y
    let func1 x = x *. x + 3
    let func1a (x) = x *. x + 3
""", 2, 8) 
    => [2, [6, 8]
        3, [20, 22]
        4, [23, 25]]

[<Test>]
let ``should find usages of operator >>= containing '>' symbol``() =
    ("""
let (>>=) x y = ()
    1 >>= 2
""", 2, 8) 
    => [2, [5, 8]
        3, [6, 9]]

[<Test>]
let ``should find usages of operator >~>> containing '>' symbol``() =
    ("""
let (>~>>) x y = ()
1 >~>> 2
""", 2, 9) 
    => [2, [5, 9]
        3, [2, 6]]

[<Test>]
let ``should find usages of operator .>> containing '.' symbol``() =
    ("""
let (.>>) x y = ()
1 .>> 2
""", 2, 8) 
    => [2, [5, 8]
        3, [2, 5]]

[<Test>]
let ``should find usages of operator >.> containing '.' symbol``() =
    ("""
let (>.>) x y = ()
1 >.> 2
""", 2, 8) 
    => [2, [5, 8]
        3, [2, 5]]

[<Test>]
let ``should find usages of operator >>. containing '.' symbol``() =
    ("""
let ( >>. ) x y = x
1 >>. 2 >>. 3 |> ignore
""", 2, 9) 
    => [2, [6, 9]
        3, [2, 5; 8, 11]]

[<Test>]
let ``should find usages of "at" operator``() =
    ("""
let (@) x y = ()
let _ = 1 @ 2
""", 2, 6) 
    => [2, [5, 6]
        3, [10, 11]]

[<Test>]
let ``should find usages of "atat" operator``() =
    ("""
let (@@) x y = ()
let _ = 1 @@ 2
""", 2, 7) 
    => [2, [5, 7]
        3, [10, 12]]

[<Test>]
let ``should find usages of "at.at" operator``() =
    ("""
let (@.@) x y = ()
let _ = 1 @.@ 2
""", 2, 8) 
    => [2, [5, 8]
        3, [10, 13]]

[<Test>]
let ``should find usages of symbols if there are operators containing dots on the same line``() =
    ("""
let ( >>. ) x y = x
1 >>. 2 >>. 3 |> ignore
""", 2, 9) 
    => [2, [6, 9]
        3, [2, 5; 8, 11]]

[<Test>]
let ``should find usages of fully qualified operator``() =
    ("""
module M =
    module N =
        let (+.) x y = ()

M.N.(+.) 1 2
""", 4, 15) 
    => [4, [13, 15]
        6, [0, 7]] // ported as is. Should be (6, [5, 7])

[<Test>]
let ``should find usages of symbols followed with a punctuation symbol ','``() =
    ("""
let f (a, b) = a + b
""", 2, 8) 
    => [2, [7, 8; 15, 16]]

[<Test>]
let ``should find usages of symbols followed with a punctuation symbol '<'``() =
    ("""
type C<'a> = C of 'a
""", 2, 6) 
    => [2, [5, 6]]

[<Test>]
let ``should find usages of symbols followed with a punctuation symbol '('``() =
    ("""
let g x = ()
g(2)
""", 3, 1) 
    => [2, [4, 5]
        3, [0, 1]]

[<Test>]
let ``should find all symbols in combined match patterns``() =
    ("""
let _ = match [] with [h] | [_; h] | [h; _] -> h | _ -> 0
""", 2, 24) 
    => [2, [23, 24; 32, 33; 38, 39; 47, 48]]

[<Test>]
let ``should not find usages inside comments``() =
    ("""
// List.length ref
""", 2, 7) 
    => []

[<Test>]
let ``should not find usages inside multi-line comments``() =
    ("""
(* 
    let p = 1
*)
""", 3, 9) 
    => []

[<Test>]
let ``should not find usages inside strings``() =
    ("""
let y = "a message and more"
""", 2, 18) 
    => []

[<Test>]
let ``should not find usages inside multiline strings``() =
    ("printfn \"\"\"\n    let p = 1\n\"\"\"", 2, 9) => []

[<Test>]
let ``should not find usages inside compiler directives``() =
    ("""
#if COMPILED
let x = 1
#endif
""", 2, 12)
    => []

[<Test>]
let ``should find usages of generic parameters``() =
    ("""
type C<'a> = C of 'a
""", 2, 9)
    => [2, [7, 9; 18, 20]]

[<Test>]
let ``should find usages of statically resolved type parameters``() =
    ("""
let inline check< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =
    if (^T : (static member IsInfinity: ^T -> bool) (num)) then None
    else Some num
""", 2, 20)
    => [2, [18, 20; 26, 28; 59, 61; 78, 80; 84, 86]
        3, [8, 10; 40, 42]]

[<Test>]
let ``should find usages of named discriminated union fields``() =
    ("""
type A =
    | B of field1: int * field2: string

let a = B (field1 = 1, field2 = "2")
let b = 
    match a with
    | B (field1 = 1) -> ()
    | B (field2 = "3") -> ()
    | _ -> ()
""", 3, 17)
    => [3, [11, 17]
        5, [11, 17]
        8, [9, 15]]

[<Test>]
let ``should find usages of active patterns``() =
    ("""
let (|A|Bb|Ccc|) (x: int) =
    if x < 0 then A elif x < 1 then Bb true else Ccc "ok"

let _ = 
    match 1 with
    | A -> ()
    | Bb false -> ()
    | Ccc "ok" -> ()
    | _ -> ()
""", 2, 7)
    => [2, [6, 7]
        3, [18, 19]
        7, [6, 7]]

[<Test>]
let ``should find usages of property initializers``() =
    ("""
type T() =
    member val Prop = "" with get, set
let t = T(Prop="a")
""", 3, 19)
    => [3, [15, 19]
        4, [10, 14]]

[<Test>]
let ``should find usages of properties with explicit getters and setters``() =
    ("""
type TypeWithProperties() =
    member x.Name with get() = 0 and set (v: int) = ()
let typeWithProperties = TypeWithProperties()
let _ = typeWithProperties.Name
typeWithProperties.Name <- 1
""", 3, 17)
    => [3, [13, 17]
        5, [8, 31]
        6, [0, 23]]

[<Test>]
let ``should find usages of type name in fully qualified record fields``() =
    ("""
type Record = { Field: int }
let r = { Record.Field = 1 }
""", 2, 11)
    => [2, [5, 11]
        3, [10, 16]]

[<Test>]
let ``should find usages of field name in fully qualified record fields``() =
    ("""
type Record = { Field: int }
let r = { Record.Field = 1 }
""", 2, 21)
    => [2, [16, 21]
        3, [17, 22]]

[<Test>]
let ``should find usages of generic types``() =
    ("""
type Type1<'a, 'b>() =
    static member Member1() = ()
let _ = Type1<_,_>.Member1()
""", 2, 10)
    => [2, [5, 10]
        4, [8, 13; 8, 18]]