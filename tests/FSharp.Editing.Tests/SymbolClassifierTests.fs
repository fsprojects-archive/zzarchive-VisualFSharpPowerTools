module FSharp.Editing.Tests.SymbolClassifierTests

open System.IO
open NUnit.Framework
open FSharp.Editing
open FSharp.Editing.Features

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpCompilerVersion.FSharp_3_1
let languageService = LanguageService()

type private Cat = Category

let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, LanguageServiceTestHelper.args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let (=>) source (expected: (int * ((Cat * int * int) list)) list) = 
    let opts = opts source
    
    let sourceLines = String.getLines source

    let lexer = 
        { new LexerBase() with
            member __.GetSymbolFromTokensAtLocation (_tokens, line, col) =
                let lineStr = sourceLines.[line]
                Lexer.getSymbol source line col lineStr SymbolLookupKind.ByRightColumn LanguageServiceTestHelper.args Lexer.queryLexState
            member __.TokenizeLine line =
                let lineStr = sourceLines.[line]
                Lexer.tokenizeLine source LanguageServiceTestHelper.args line lineStr Lexer.queryLexState 
            member __.LineCount = sourceLines.Length }

    let symbolsUses = 
        languageService.GetAllUsesOfAllSymbolsInFile (opts, fileName, source, AllowStaleResults.No, true)
        |> Async.RunSynchronously

    let checkResults = 
        languageService.ParseAndCheckFileInProject(opts, fileName, source, AllowStaleResults.No) |> Async.RunSynchronously

    let actualCategories =
        SourceCodeClassifier.getCategorizedSpans (symbolsUses, checkResults, lexer, fun line -> sourceLines.[line])
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
                    | Cat.Other -> None
                    | _ -> Some (span.Category, span.WordSpan.StartCol, span.WordSpan.EndCol))
                |> Seq.sortBy (fun (_, startCol, _) -> startCol)
                |> Seq.toList
            | None -> line, [])
        |> List.sortBy (fun (line, _) -> line)
    
    let expected = 
        expected 
        |> List.map (fun (line, spans) -> line, spans |> List.sortBy (fun (_, startCol, _) -> startCol))
        |> List.sortBy (fun (line, _) -> line)
    
    try actual |> Collection.assertEquiv expected
    with _ -> 
        debug "AST: %A" checkResults.ParseTree
        for x in actual do
            debug "Actual: %A" x
        reraise()

[<Test>]
let ``module value``() = 
    """
let moduleValue = 1
"""
    => [2, [Cat.Operator, 16, 17]]

[<Test>]
let ``module function``() = 
    """
let moduleFunction x = x + 1
"""
    => [ 2, [ Cat.Function, 4, 18; Cat.Operator, 21, 22; Cat.Operator, 25, 26 ]]

[<Test>]
let ``module higher order function``() = 
    """
let higherOrderFunction func x = (func x) - 1
"""
   => [ 2, [ Cat.Function, 24, 28; Cat.Function, 34, 38; Cat.Function, 4, 23; Cat.Operator, 31, 32; Cat.Operator, 42, 43 ]]

[<Test>]
let ``class let value``() = 
    """
type Class() =
    let value = 1
    member __.M = value
"""
    => [ 2, [ Cat.ReferenceType, 5, 10; Cat.Operator, 13, 14 ]
         3, [ Cat.Operator, 14, 15 ]
         4, [ Cat.Operator, 16, 17 ]]

[<Test>]
let ``class let function``() = 
    """
type Class() =
    let classLetFunction x = x
    member __.M = classLetFunction 1
"""
    => [ 3, [ Cat.Function, 8, 24; Cat.Operator, 27, 28 ]
         4, [ Cat.Function, 18, 34; Cat.Operator, 16, 17 ]]

[<Test>]
let ``class method``() = 
    """
type Class() =
    member __.Method _ = ()
"""
   => [ 3, [ Cat.Function, 14, 20; Cat.Operator, 23, 24 ]]

[<Test>]
let ``class property``() = 
 """
type Class() =
    member __.Prop = ()
"""
   => [ 3, [ Cat.Operator, 19, 20]]

[<Test>]
let ``static method``() = 
    """
type Class() =
    static member Method _ = ()
"""
    => [3, [ Cat.Function, 18, 24; Cat.Operator, 27, 28 ]]

[<Test>]
let ``static property``() = 
    """
type Class() =
    static member StaticProperty = 1
"""
    => [ 3, [ Cat.Operator, 33, 34 ]]

[<Test>]
let ``event``() = 
    """
type Class() =
    let event = Event<_>()
    member __.Event = event.Publish
"""
    => [ 3, [  Cat.Operator, 14, 15; Cat.ReferenceType, 16, 21 ]
         4, [ Cat.Operator, 20, 21 ]]

[<Test>]
let ``static event``() = 
    """
type Class() =
    static let staticEvent = Event<_>()
    static member StaticEvent = staticEvent.Publish
"""
    => [ 3, [  Cat.Operator, 27, 28; Cat.ReferenceType, 29, 34 ]
         4, [ Cat.Operator, 30, 31 ]]
          
[<Test>]
let ``class constructor``() = 
    """
type Class() =
    new (_: int) = new Class()
"""
    => [ 2, [ Cat.ReferenceType, 5, 10; Cat.Operator, 13, 14 ]
         3, [ Cat.ValueType, 12, 15; Cat.Operator, 17, 18; Cat.ReferenceType, 23, 28 ]]

[<Test>]
let ``generic class constructor``() = 
    """
type Class<'a>() = class end
    let _ = new Class<_>()
"""
    => [ 2, [ Cat.ReferenceType, 5, 10; Cat.Operator, 17, 18 ]
         3, [ Cat.Operator, 10, 11; Cat.ReferenceType, 16, 21 ]]

[<Test>]
let ``interface implemented in a class``() = 
    """
type Class() =
    interface System.IDisposable with
        member __.Dispose() = ()
"""
    => [ 3, [ Cat.ReferenceType, 21, 32 ]
         4, [ Cat.Function, 18, 25; Cat.Operator, 28, 29 ]]

[<Test>]
let ``property with explicit accessors``() = 
    """
type Class() =
    member __.PropWithGetterAndSetter 
                with get() = 1 
                and set(_: int) = ()
"""
    => [ 3, [] 
         4, [ Cat.Operator, 27, 28 ]
         5, [ Cat.ValueType, 27, 30; Cat.Operator, 32, 33 ]]

[<Test>]
let ``fully qualified CLI type constructor``() = 
    """
let dateTime = new System.Net.WebClient()
"""
    => [ 2, [  Cat.Operator, 13, 14; Cat.ReferenceType, 30, 39 ]]

[<Test>]
let ``fully qualified F# type constructor``() = 
    """
module M1 =
    module M2 =
        type Type() = class end

let m1m2Type = M1.M2.Type()
"""
    => [ 6, [  Cat.Operator, 13, 14; Cat.Module, 18, 20; Cat.Module, 15, 17; Cat.ReferenceType, 21, 25 ]]

[<Test>]
let ``generic class declaration``() = 
    """
type GenericClass<'T>() = class end
"""
    => [ 2, [ Cat.ReferenceType, 5, 17; Cat.Operator, 24, 25 ]]

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
    => [ 6, [ Cat.Operator, 22, 23; Cat.ReferenceType, 24, 36; Cat.ValueType, 37, 40 ]
         7, [ Cat.Operator, 33, 34; Cat.ReferenceType, 35, 47; Cat.Module, 51, 53; Cat.Module, 48, 50; Cat.ReferenceType, 54, 58 ]
         8, [ Cat.Operator, 26, 27; Cat.ReferenceType, 28, 40; Cat.ValueType, 48, 56 ]]

[<Test>]
let ``record``() = 
    """
module M1 =
    module M2 =
        type Type() = class end
type Record = { IntField: int; UserTypeField: M1.M2.Type }
"""
    => [ 5, [ Cat.ReferenceType, 5, 11
              Cat.Operator, 12, 13
              Cat.ValueType, 26, 29
              Cat.Module, 49, 51; Cat.Module, 46, 48; Cat.ReferenceType, 52, 56 ]]

[<Test>]
let ``value type``() = 
    """
let valueTypeAbbriviation: int = 1
let valueType: System.Int32 = 2
let valueTypeConstructor = System.DateTime()
type UserValueType = struct end
type UserValueTypeAbbriviation = UserValueType
let userValueType = UserValueType()
let userValueTypeAbbriviation: UserValueTypeAbbriviation = UserValueTypeAbbriviation()
"""
    => [ 2, [ Cat.ValueType, 27, 30; Cat.Operator, 31, 32 ]
         3, [ Cat.ValueType, 22, 27; Cat.Operator, 28, 29 ]
         4, [ Cat.ValueType, 34, 42; Cat.Operator, 25, 26 ]
         5, [ Cat.ValueType, 5, 18; Cat.Operator, 19, 20 ]
         6, [ Cat.ValueType, 5, 30; Cat.Operator, 31, 32; Cat.ValueType, 33, 46 ]
         7, [ Cat.Operator, 18, 19; Cat.ValueType, 20, 33 ] 
         8, [ Cat.ValueType, 31, 56; Cat.Operator, 57, 58; Cat.ValueType, 59, 84 ]]

[<Test>]
let ``DU case of function``() =
    """
type DUWithFunction = FuncCase of (unit -> unit)
let (FuncCase funcCase) = FuncCase (fun() -> ())
match FuncCase (fun() -> ()) with FuncCase func -> func()
"""
    => [ 2, [ Cat.ReferenceType, 5, 19; Cat.Operator, 20, 21; Cat.PatternCase, 22, 30; Cat.ReferenceType, 35, 39; Cat.ReferenceType, 43, 47 ]
         3, [ Cat.PatternCase, 5, 13; Cat.Function, 14, 22; Cat.Operator, 24, 25; Cat.PatternCase, 26, 34 ]
         4, [ Cat.PatternCase, 6, 14; Cat.PatternCase, 34, 42; Cat.Function, 43, 47; Cat.Function, 51, 55 ]]

[<Test>]
let ``single case DU without leading pipe``() =
    """
type DU = Case
"""
    => [2, [Cat.ReferenceType, 5, 7; Cat.Operator, 8, 9; Cat.PatternCase, 10, 14 ]]

[<Test>]
let ``double quoted function without spaces``() = 
    """
let ``double_quoted_function_without_spaces`` () = ()
"""
    => [ 2, [ Cat.Function, 4, 45; Cat.Operator, 49, 50 ]]

[<Test>]
let ``double quoted function with spaces``() = 
    """
let ``double quoted function with spaces`` () = ()
"""
    => [ 2, [ Cat.Function, 4, 42; Cat.Operator, 46, 47 ]]

[<Test>]
let ``fully qualified attribute``() = 
    """
[<System.Diagnostics.DebuggerDisplay "name">]
type TypeWithAttribute() = class end
"""
    => [ 2, [ Cat.ReferenceType, 21, 36 ]]

[<Test>]
let ``async type``() = 
    """
let asyncRunSync = Async.RunSynchronously
"""
    => [ 2, [ Cat.Function, 4, 16; Cat.Operator, 17, 18; Cat.ReferenceType, 19, 24; Cat.Function, 25, 41 ]]

[<Test>]
let ``standard computation expression name``() = 
    """
seq {
    let func x = x
    yield func 1
} |> ignore
"""
    => [ 2, []
         3, [ Cat.Function, 8, 12; Cat.Operator, 15, 16 ]
         4, [ Cat.Function, 10, 14 ]]

[<Test>]
let ``user defined computation expression name``() = 
    """
type CustomBuilder() =
    member __.Yield (()) = ()
    [<CustomOperation ("add", MaintainsVariableSpace = true)>]
    member __.Add (_, pattern: string) = pattern
let customComputationExpression = CustomBuilder()
let _ = customComputationExpression { add "str" }
"""
    => [ 7, [ Cat.Operator, 6, 7 ]]

[<Test>]
let ``method chain``() =
    """
let _ = System.Environment.MachineName.ToLower()
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ReferenceType, 15, 26; Cat.Function, 39, 46 ]]
    
[<Test>]
let ``complex method chain``() =
    """
let _ = System.Guid.NewGuid().ToString("N").Substring(1)
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ValueType, 15, 19; Cat.Function, 20, 27; Cat.Function, 30, 38; Cat.Function, 44, 53 ]]

[<Test>]
let ``generic type with ignored type parameter``() = 
    """
let _ = list<_>.Empty
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ReferenceType, 8, 12 ]]

[<Test>]
let ``F# namespace``() = 
    """
let _ = Microsoft.FSharp.Collections.List<int>.Empty
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ReferenceType, 37, 41; Cat.ValueType, 42, 45 ]]
       
[<Test>]
let ``double quoted member``() = 
    """
type System.String with
    member __.``Long func``() = "x"
let _ = "x".``Long func``().Substring(3)
"""
    => [ 4, [ Cat.Operator, 6, 7; Cat.Function, 12, 25; Cat.Function, 28, 37 ]]

[<Test>]
let ``indexer``() = 
    """
let arr = [|1|]
let _ = arr.[0]
let l, h = 0, 1
let _ = arr.[l..h]
let _ = arr.[0..h]
let _ = arr.[l..1]
let _ = arr.[l..]
let _ = arr.[..h]
"""
    => [ 3, [ Cat.Operator, 6, 7; Cat.Operator, 11, 12; Cat.Module, 11, 12 ]
         5, [ Cat.Operator, 6, 7; Cat.Operator, 11, 12; Cat.Module, 11, 12 ]
         6, [ Cat.Operator, 6, 7; Cat.Operator, 11, 12; Cat.Module, 11, 12 ]
         7, [ Cat.Operator, 6, 7; Cat.Operator, 11, 12; Cat.Module, 11, 12 ]
         8, [ Cat.Operator, 6, 7; Cat.Operator, 11, 12; Cat.Module, 11, 12 ]
         9, [ Cat.Operator, 6, 7; Cat.Operator, 11, 12; Cat.Module, 11, 12 ]]

[<Test>]
let ``mutable value``() = 
    """
let mutable mutableValue = 1
"""
    => [ 2,  [ Cat.MutableVar, 12, 24; Cat.Operator, 25, 26 ]]

[<Test>]
let ``mutable field``() =
    """
type MutableRecord = 
    { mutable MutableField: int }
type MutableClass() = 
    let mutable mutableField = 0
    let _ = mutableField
let func() =
    let mutable mutableLocalVar = 1 in mutableLocalVar
"""
    => [ 3, [ Cat.MutableVar, 14, 26; Cat.ValueType, 28, 31 ]
         5, [ Cat.MutableVar, 16, 28; Cat.Operator, 29, 30 ]
         8, [ Cat.MutableVar, 16, 31; Cat.Operator, 32, 33; Cat.MutableVar, 39, 54 ]]

[<Test>]
let ``reference value``() = 
    """
let refValue = ref 1
refValue := !refValue + 1
""" 
    => [ 2, [ Cat.MutableVar, 4, 12; Cat.Operator, 13, 14; Cat.Function, 15, 18 ]
         3, [ Cat.MutableVar, 0, 8; Cat.Operator, 9, 11; Cat.Operator, 12, 13; Cat.MutableVar, 13, 21
              Cat.Operator, 22, 23 ]]

[<Test>]
let ``reference field``() = 
    """
type ClassWithRefValue() =
    let refValue = ref 1
    let _ = !refValue
type RecordWithRefValue = 
    { Field: int ref }
"""
    => [ 3, [ Cat.Operator, 17, 18; Cat.Function, 19, 22; Cat.MutableVar, 8, 16 ]
         4, [ Cat.Operator, 10, 11; Cat.Operator, 12, 13; Cat.MutableVar, 13, 21 ]
         6, [ Cat.MutableVar, 6, 11; Cat.ValueType, 13, 16; Cat.ReferenceType, 17, 20 ]]

[<Test>]
let ``single line quotation``() = 
    """
let _ = <@ 1 = 1 @>
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Quotation, 8, 19; Cat.Operator, 13, 14 ]]

[<Test>]
let ``multi line quotation``() = 
    """
let _ = <@ 1 = 1
           && 2 = 2 @>
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Quotation, 8, 16; Cat.Operator, 13, 14 ]
         3, [ Cat.Operator, 11, 13; Cat.Quotation, 11, 22; Cat.Operator, 16, 17 ]]

[<Test>]
let ``quotation as function argument``() = 
    """
let _ = id <@ 1 = 1 @>
let f x y = ()
let _ = f <@ 1 = 1 @> <@ 2 = 2 @>
let _ =
    assert true 
    [] 
    |> List.fold (fun acc x -> acc
    ) <@@ () @@>
let qf1 (n, e1) = ()
let _ = qf1 (1, <@ 1 @>)
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 10; Cat.Quotation, 11, 22; Cat.Operator, 16, 17 ]
         4, [ Cat.Operator, 6, 7; Cat.Function, 8, 9; Cat.Quotation, 10, 21; Cat.Operator, 15, 16;
              Cat.Quotation, 22, 33; Cat.Operator, 27, 28 ]
         9, [ Cat.Quotation, 6, 16 ]
         11, [ Cat.Operator, 6, 7; Cat.Function, 8, 11; Cat.Quotation, 16, 23 ]]

[<Test>]
let ``quotation in type``() = 
    """
type TypeWithQuotations() =
    let _ = <@ 1 = 1 @>
    member __.F() = <@ 1 = 1 @>
    member __.P = <@ 1 + 1 @>
"""
    => [ 3, [ Cat.Operator, 10, 11; Cat.Quotation, 12, 23; Cat.Operator, 17, 18 ]
         4, [ Cat.Function, 14, 15; Cat.Operator, 18, 19; Cat.Quotation, 20, 31; Cat.Operator, 25, 26 ]
         5, [ Cat.Operator, 16, 17; Cat.Quotation, 18, 29; Cat.Operator, 23, 24 ]]

[<Test>]
let ``untyped quotation``() = 
    """
let _ = <@@ 1 @@>
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Quotation, 8, 17 ]]

[<Test>]
let ``complicated quotation layout``() = 
    """
let f x y = ()
let _  = f <@ 1
              + 2
              + 3 @> <@@ 1 @@>
"""
    => [ 3, [ Cat.Operator, 7, 8; Cat.Function, 9, 10; Cat.Quotation, 11, 15 ]
         4, [ Cat.Operator, 14, 15; Cat.Quotation, 14, 17 ]
         5, [ Cat.Operator, 14, 15; Cat.Quotation, 14, 20; Cat.Quotation, 21, 30 ]]

[<Test>]
let ``quotation in lambda``() = 
    """
let _ = fun() -> <@ 1 @>
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Quotation, 17, 24 ]]

[<Test>]
let ``quotation in record``() = 
    """
type RecordWithQuotation = { Field: Microsoft.FSharp.Quotations.Expr<int> }
let _ = { Field = <@ 1 @> }
"""
    => [ 3, [ Cat.Operator, 6, 7; Cat.Operator, 16, 17; Cat.Quotation, 18, 25 ]]

[<Test>]
let ``quotation in list expression``() = 
    """
let _ = [ <@ 1 @> ]
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Quotation, 10, 17 ]]

[<Test>]
let ``quotation in seq for expression``() = 
    """
let _ = seq { for i in [1..10] -> <@ i @> }
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Quotation, 34, 41 ]]

[<Test>]
let ``quotation as a result of function``() = 
    """
let qf() : Microsoft.FSharp.Quotations.Expr<int> =
    <@ 1 @>
"""
    => [ 3, [ Cat.Quotation, 4, 11 ]]

[<Test>]
let ``quotation as default constructor arguments``() = 
    """
type ClassWithQuotationInConstructor(expr) = class end
let _ = ClassWithQuotationInConstructor(<@ 1 @>)
"""
    => [ 3, [ Cat.Operator, 6, 7; Cat.ReferenceType, 8, 39; Cat.Quotation, 40, 47 ]]

[<Test>]
let ``quotation as initialization of auto property``() = 
    """
type ClassWithWritableProperty() =
    member val Prop = <@@ 1 @@> with get, set
"""
    => [ 3, [ Cat.MutableVar, 15, 19; Cat.Operator, 20, 21; Cat.Quotation, 22, 31 ]]

[<Test>]
let ``quotation in property setter``() = 
    """
type ClassWithWritableProperty() =
    member val Prop = <@@ 1 @@> with get, set
let clWithWritableProperty = ClassWithWritableProperty()
clWithWritableProperty.Prop <- <@@ 2 @@>
"""
    => [ 5, [ Cat.Quotation, 31, 40 ]]

[<Test>]
let ``quotation in nested module``() = 
    """
module NestedModule =
    let _ = <@ 1 @>
"""
    => [ 3, [ Cat.Operator, 10, 11; Cat.Quotation, 12, 19 ]]

[<Test>]
let ``quotation inside computation expression``() =
    """
let _ = 
    let ret x = async { return x }
    let retZero _ = async { return () }
    async { 
        let _ = <@ 1 @>
        do ignore <@ 1 @>
        let! _ = ret <@ 1 @>
        let! _ = if true then 
                    ret <@ 1 @>
                 else 
                    ret <@ 2 @>
        do! retZero <@ () @>
        match <@ 1 @> with
        | _ -> ()
        if true then 
            return <@ 1 @>
        else
            return! ret <@ 1 @>
    }
"""
    => [ 6, [ Cat.Operator, 14, 15; Cat.Quotation, 16, 23 ]
         7, [ Cat.Function, 11, 17; Cat.Quotation, 18, 25 ]
         8, [ Cat.Operator, 15, 16; Cat.Function, 17, 20; Cat.Quotation, 21, 28 ]
         10, [ Cat.Function, 20, 23; Cat.Quotation, 24, 31 ]
         12, [ Cat.Function, 20, 23; Cat.Quotation, 24, 31 ]
         13, [ Cat.Function, 12, 19; Cat.Quotation, 20, 28 ]
         14, [ Cat.Quotation, 14, 21 ]
         17, [ Cat.Quotation, 19, 26 ]
         19, [ Cat.Function, 20, 23; Cat.Quotation, 24, 31 ]]

[<Test>]
let ``quotation in try / with / finally blocks``() =
    """
try
    try <@ 1 @>
    with _ -> <@ 2 @>
finally ignore <@ 3 @>

async {
    try
        try <@ 1 @>
        with _ -> <@ 2 @>
    finally ignore <@ 3 @> 
    return ()
}
"""
    => [3, [ Cat.Quotation, 8, 15]
        4, [ Cat.Quotation, 14, 21]
        5, [ Cat.Function, 8, 14; Cat.Quotation, 15, 22]
        9, [ Cat.Quotation, 12, 19]
        10, [ Cat.Quotation, 18, 25]
        11, [ Cat.Function, 12, 18; Cat.Quotation, 19, 26]
       ]

[<Test>]
let ``quotation in pattern matching``() = 
    """
let _ = 
    match obj() with
    | <@ 1 @> -> <@@ 2 @@>
    | _ -> <@@ () @@>
let _ =
    match None with
    | Some <@ 1 @> -> ()
    | _ -> ()
"""
    => [ 4, [ Cat.Quotation, 6, 13; Cat.Quotation, 17, 26 ]
         8, [ Cat.Quotation, 11, 18 ]]

[<Test>]
let ``nested multiline quotation``() =
    """
let _ = <@ <@ 1 @>, 2
            @>
"""
    => [2, [ Cat.Operator, 6, 7; Cat.Quotation, 8, 21 ]
        3, [ Cat.Quotation, 12, 14 ]]

[<Test>]
let ``quotation in static members``() =
    """
type EqualOp = EqualOp with
    static member inline f = 
        <@ 1 @>
"""
    => [4, [ Cat.Quotation, 8, 15 ]]

[<Test>]
let ``tuple alias``() = 
    """
type Tuple = int * string
let tupleFunc (x: Tuple) : Tuple = x
"""
    => [ 2, [ Cat.ReferenceType, 5, 10; Cat.Operator, 11, 12; Cat.ValueType, 13, 16; Cat.ReferenceType, 19, 25 ]    
         3, [ Cat.Function, 4, 13; Cat.ReferenceType, 18, 23; Cat.ReferenceType, 27, 32; Cat.Operator, 33, 34 ]]

[<Test>]
let ``multi-line method chain``() = 
    """
let _ =
    "string"
        .Substring(1)
        .Trim().Remove(1)
"""
    => [ 4, [ Cat.Function, 9, 18 ]
         5, [ Cat.Function, 9, 13; Cat.Function, 16, 22 ]]

[<Test>]
let ``module``() = 
    """
module Module1
module Module2 =
    module Module3 =
        let x = ()
"""
    => [ 2, [ Cat.Module, 7, 14 ]
         3, [ Cat.Module, 7, 14; Cat.Operator, 15, 16 ]
         4, [ Cat.Module, 11, 18; Cat.Operator, 19, 20 ]]

[<Test>]
let ``static CLR class``() = 
    """
let _ = System.Linq.Enumerable.Range(0, 1)
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ReferenceType, 20, 30; Cat.Function, 31, 36 ]]

[<Test>]
let ``F# external modules``() = 
    """
let _ = [1] |> Seq.sort |> Seq.toList |> List.rev
"""
    => [ 2, [ Cat.Operator, 6, 7;
              Cat.Operator, 12, 14; Cat.Module, 15, 18; Cat.Function, 19, 23
              Cat.Operator, 24, 26; Cat.Module, 27, 30; Cat.Function, 31, 37
              Cat.Operator, 38, 40; Cat.Module, 41, 45; Cat.Function, 46, 49 ]]

[<Test>]
let ``byref argument``() = 
    """
let ``func with byref arg`` (_: byref<int>) = ()
"""
    => [ 2, [ Cat.Function, 4, 27
              Cat.ReferenceType, 32, 37
              Cat.ValueType, 38, 41
              Cat.Operator, 44, 45 ]]

[<Test>]
let ``unit of measure``() =
    """
[<Measure>] type ms
let _: int<ms> = 
    1<ms>
type RecordWithUnitOfMeasure =
    { Field1: int<ms> }
"""
    => [ 3, [ Cat.ValueType, 7, 10; Cat.ReferenceType, 11, 13; Cat.Operator, 15, 16 ]
         4, [ Cat.ReferenceType, 6, 8 ]
         6, [ Cat.ValueType, 14, 17; Cat.ReferenceType, 18, 20 ]]

[<Test>]
let ``standard and custom numeric literals``() = 
    """
let _ = 1I
module NumericLiteralZ =
    let FromInt32 (i: int) = i
let _ = 77Z
"""
    => [ 2, [ Cat.Operator, 6, 7 ]    
         5, [ Cat.Operator, 6, 7 ]]

[<Test>]
let ``anonymous generic parameters``() =
    """
module AnonymousGenericParameters =
    let f' () : Map<_,   _> = new Map<_,   _>([])
    let f''() : Map<_,   _> =     Map<_,   _>([])
    let g () : Map<'a,  _> = new Map<'a,  _>([])
    let g'() : Map<'a,  _> =     Map<'a,  _>([])
    let h () : Map<_,  'b> = new Map<_,  'b>([])
    let i () : Map<'a, 'b> = new Map<'a, 'b>([])
    let j () : System.Collections.Generic.List<_> = new System.Collections.Generic.List<_>()
"""
    => [ 3, [ Cat.Function, 8, 10; Cat.ReferenceType, 16, 19; Cat.Operator, 28, 29; Cat.ReferenceType, 34, 37 ]
         4, [ Cat.Function, 8, 11; Cat.ReferenceType, 16, 19; Cat.Operator, 28, 29; Cat.ReferenceType, 34, 37 ] 
         5, [ Cat.Function, 8, 9; Cat.ReferenceType, 15, 18; Cat.Operator, 27, 28; Cat.ReferenceType, 33, 36 ]
         6, [ Cat.Function, 8, 10; Cat.ReferenceType, 15, 18; Cat.Operator, 27, 28; Cat.ReferenceType, 33, 36 ] 
         7, [ Cat.Function, 8, 9; Cat.ReferenceType, 15, 18; Cat.Operator, 27, 28; Cat.ReferenceType, 33, 36 ]
         8, [ Cat.Function, 8, 9; Cat.ReferenceType, 15, 18; Cat.Operator, 27, 28; Cat.ReferenceType, 33, 36 ]
         9, [ Cat.Function, 8, 9; Cat.ReferenceType, 42, 46; Cat.Operator, 50, 51; Cat.ReferenceType, 83, 87 ]]

[<Test>]
let ``array alias``() =
    """
type ArrayAlias = byte[]
"""
    => [ 2, [ Cat.ReferenceType, 5, 15; Cat.Operator, 16, 17; Cat.ValueType, 18, 22 ]]

[<Test>]
let ``function type alias``() =
    """
type FuncAlias = unit -> unit
let func: FuncAlias = fun () -> ()
type FuncAliasOfAlias = FuncAlias
let func1: FuncAliasOfAlias = fun () -> ()
"""
    => [ 2, [ Cat.ReferenceType, 5, 14; Cat.Operator, 15, 16; Cat.ReferenceType, 17, 21; Cat.ReferenceType, 25, 29 ]
         3, [ Cat.Function, 4, 8; Cat.ReferenceType, 10, 19; Cat.Operator, 20, 21 ]
         4, [ Cat.ReferenceType, 5, 21; Cat.Operator, 22, 23; Cat.ReferenceType, 24, 33 ]
         5, [ Cat.Function, 4, 9; Cat.ReferenceType, 11, 27; Cat.Operator, 28, 29 ]]

[<Test>]
let ``partial active patterns``() =
    """
let (|ActivePattern|_|) x = Some x
let _ = (|ActivePattern|_|) 1
"""
    => [ 2, [(Cat.PatternCase, 6, 19); (Cat.Operator, 26, 27); (Cat.PatternCase, 28, 32)]
         3, [(Cat.Operator, 6, 7); (Cat.Function, 8, 27)] ]

[<Test>]
let ``total active patterns``() =
    """
let (|A|B|) _ = failwith ""
let _ = (|A|B|) 1
"""
    => [ 2, [(Cat.PatternCase, 6, 7); (Cat.PatternCase, 8, 9); (Cat.Operator, 14, 15); (Cat.Function, 16, 24)]
         3, [(Cat.Operator, 6, 7); (Cat.Function, 8, 15)] ]


[<Test>]
let ``non public module``() =
    """
module private PrivateModule =
    let x = ()
"""
    => [ 2, [ Cat.Module, 15, 28; Cat.Operator, 29, 30 ]]

[<Test>]
let ``open a nested module inside another one is not unused``() =
    """
module Top
module M1 =
    let x = ()
module M2 =
    open M1
    let y = x
"""
    => [ 6, []]

[<Test>]
let ``open a nested module inside another one is not unused, complex hierarchy``() =
    """
module Top =
    module M1 =
        module M11 =
            let x = ()
    module M2 =
        module M22 =
            open M1.M11
            let y = x
"""
    => [ 8, []]

[<Test>]
let ``open a nested module inside another one is not unused, even more complex hierarchy``() =
    """
module Top =
    module M1 =
        module M11 =
            module M111 =
                module M1111 =
                    let x = ()
    module M2 =
        module M22 =
            open M1.M11.M111.M1111
                let y = x
"""
    => [ 10, []]

[<Test>]
let ``printf formatters in bindings``() =
    """
let _ = printfn ""
let _ = printfn "%s %s"
do printfn "%6d %%  % 06d" 1 2
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15 ]
         3, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 17, 19; Cat.Printf, 20, 22 ]
         4, [ Cat.Function, 3, 10; Cat.Printf, 12, 15; Cat.Printf, 16, 18; Cat.Printf, 20, 25 ]]

[<Test>]
let ``printf formatters in try / with / finally``() =
    """
try
    let _ = sprintf "foo %A bar" 0
    try
        printfn "foo %b bar" true
    finally
        printf "foo %i bar" 0
with _ ->
    failwithf "foo %d bar" 0
"""
    =>  [ 3, [ Cat.Operator, 10, 11; Cat.Function, 12, 19; Cat.Printf, 25, 27 ]
          5, [ Cat.Function, 8, 15; Cat.Printf, 21, 23 ]
          7, [ Cat.Function, 8, 14; Cat.Printf, 20, 22 ]
          9, [ Cat.Function, 4, 13; Cat.Printf, 19, 21 ]]

[<Test>]
let ``printf formatters in record / DU members``() =
    """
type R = { Name: string }
    with 
        member __.M _ = 
            sprintf "%A"
        override __.ToString() = 
            sprintf "%d" 1
type DU = DU
    with
        member __.M = 
            sprintf "%A"
        override __.ToString() = 
            sprintf "%d" 1
"""
    => [ 5, [ Cat.Function, 12, 19; Cat.Printf, 21, 23 ]
         7, [ Cat.Function, 12, 19; Cat.Printf, 21, 23 ]
         11, [ Cat.Function, 12, 19; Cat.Printf, 21, 23 ]
         13, [ Cat.Function, 12, 19; Cat.Printf, 21, 23 ]]

[<Test>]
let ``printf formatters in extension members``() =
    """
type System.Object with
    member __.M1 = 
        sprintf "%A" 
"""
    => [ 4, [ Cat.Function, 8, 15; Cat.Printf, 17, 19 ]]

[<Test>]
let ``printf formatters in escaped string``() =
    """
let _ = sprintf @"%A"
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 18, 20 ]]

[<Test>]
let ``printf formatters in triple-quoted string``() =
    "let _ = sprintf \"\"\"%A\"\"\""
    => [ 1, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 19, 21 ]]

[<Test>]
let ``multi-line printf formatters``() =
    """
let _ = printfn "foo %s %d
                 %A bar
%i"
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 21, 23; Cat.Printf, 24, 26 ] 
         3, [ Cat.Printf, 17, 19 ]
         4, [ Cat.Printf, 0, 2 ] ]

[<Test>]
let ``printf formatters in for expressions``() =
    """
for _ in (sprintf "%d" 1).ToCharArray() do
    sprintf "%d" 1 
    |> ignore
[ for _ in (sprintf "%d" 1).ToCharArray() do
    yield sprintf "%s" ]
|> ignore

    """
    => [ 2, [ Cat.Function, 10, 17; Cat.Printf, 19, 21; Cat.Function, 26, 37 ]
         3, [ Cat.Function, 4, 11; Cat.Printf, 13, 15]
         5, [ Cat.Function, 12, 19; Cat.Printf, 21, 23; Cat.Function, 28, 39 ]
         6, [ Cat.Function, 10, 17; Cat.Printf, 19, 21]
    ]

[<Test>]
let ``printf formatters in quoted expressions``() =
    """
let _ = <@ sprintf "%A" @>
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 11, 18; Cat.Printf, 20, 22; Cat.Quotation, 8, 26 ]]

[<Test>]
let ``printf formatters if printf function is namespace qualified``() =
    """
let _ = Microsoft.FSharp.Core.Printf.printf "%A" 0
open Microsoft.FSharp.Core
let _ = Printf.printf "%A" 0
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Module, 30, 36; Cat.Function, 37, 43; Cat.Printf, 45, 47 ]
         4, [ Cat.Operator, 6, 7; Cat.Module, 8, 14; Cat.Function, 15, 21; Cat.Printf, 23, 25 ]]

[<Test>]
let ``printf formatters are not colorized in plane strings``() =
    """
let _ = sprintf "foo", "%A"
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15 ]]

[<Test>]
let ``fprintf formatters``() =
    """
let _ = fprintf null "%A" 0
let _ = Microsoft.FSharp.Core.Printf.fprintf null "%A" 0
let _ = fprintfn null "%A" 0
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 22, 24 ]
         3, [ Cat.Operator, 6, 7; Cat.Module, 30, 36; Cat.Function, 37, 44; Cat.Printf, 51, 53 ]
         4, [ Cat.Operator, 6, 7; Cat.Function, 8, 16; Cat.Printf, 23, 25 ]]

[<Test>]
let ``kprintf and bprintf formatters``() =
    """
let _ = Printf.kprintf (fun _ -> ()) "%A" 1
let _ = Printf.bprintf null "%A" 1
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Module, 8, 14; Cat.Function, 15, 22; Cat.Printf, 38, 40]
         3, [ Cat.Operator, 6, 7; Cat.Module, 8, 14; Cat.Function, 15, 22; Cat.Printf, 29, 31]]

[<Test>]
let ``wildcards in printf formatters``() =
    """
let _ = sprintf "%*d" 1
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 17, 20 ]]

[<Test>]
let ``float printf formatters``() =
    """
let _ = sprintf "%7.1f" 1.0
let _ = sprintf "%-8.1e+567" 1.0
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 17, 22]
         3, [ Cat.Operator, 6, 7; Cat.Function, 8, 15; Cat.Printf, 17, 23]]

[<Test>]
let ``malformed printf formatters``() =
    """
let _ = sprintf "%.7f %7.1A %7.f %--8.1f"
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Function, 8, 15]]

[<Test>]
let ``all escaped symbols in string``() =
    """    
let _ = "\n\r \t\b foo \\ \" \' \u08FF \U0102AABB \u012 \U01234"
"""
    => [ 2, [ Cat.Operator, 6, 7
              Cat.Escaped, 9, 11
              Cat.Escaped, 11, 13
              Cat.Escaped, 14, 16
              Cat.Escaped, 16, 18
              Cat.Escaped, 23, 25
              Cat.Escaped, 26, 28
              Cat.Escaped, 29, 31
              Cat.Escaped, 32, 38
              Cat.Escaped, 39, 49 ]]

[<Test>]
let ``escaped symbols in multi-line string``() =
    """
let _ = "\n
\r" """
    => [ 2, [ Cat.Operator, 6, 7; Cat.Escaped, 9, 11 ]
         3, [ Cat.Escaped, 0, 2 ]]

[<Test>]
let ``escaped symbols in complex multiline string``() =
    """
let _ = "foo \n bar \r baz
\t
 \r f \t\b \\ 
\n"
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.Escaped, 13, 15; Cat.Escaped, 20, 22 ]
         3, [ Cat.Escaped, 0, 2 ]
         4, [ Cat.Escaped, 1, 3; Cat.Escaped, 6, 8; Cat.Escaped, 8, 10; Cat.Escaped, 11, 13 ]
         5, [ Cat.Escaped, 0, 2 ]]

[<Test>]
let ``escaped symbols in method chains``() =
    """
let _ = "a\r\n".Replace("\r\n", "\n").Split('\r')
"""
    => [ 2, [ Cat.Operator, 6, 7; 
              Cat.Escaped, 10, 12; Cat.Escaped, 12, 14; Cat.Function, 16, 23
              Cat.Escaped, 25, 27; Cat.Escaped, 27, 29
              Cat.Escaped, 33, 35; Cat.Function, 38, 43 ]]

[<Test>]
let ``operators``() =
    """
let _ = 1 + 2
let _ = 1 = 2
let (>>=) _x _y = ()
let _ = 1 >>= fun _ -> 2
let _ = match obj() with | :? exn -> () | _ -> ()
"""
    => [ 2, [ Cat.Operator, 10, 11; Cat.Operator, 6, 7 ]
         3, [ Cat.Operator, 10, 11; Cat.Operator, 6, 7 ]
         4, [ Cat.Operator, 5, 8; Cat.Operator, 16, 17 ]
         5, [ Cat.Operator, 10, 13; Cat.Operator, 6, 7 ] 
         6, [ Cat.Operator, 6, 7; Cat.ReferenceType, 14, 17; Cat.Operator, 27, 29; Cat.ReferenceType, 30, 33 ]] 

[<Test>]
let ``lexer-based operator is hidden by symbol-based one``() =
    """
let _ = 1
let a = [||]
let (>>=) _x _y = ()
a.[0] >>= fun _ -> ()
"""
    => [ 2, [ Cat.Operator, 6, 7 ]
         5, [ Cat.Operator, 1, 2; Cat.Module, 1, 2; Cat.Operator, 6, 9 ]] 

[<Test>]
let ``cast operators``() =
    """
let _ = System.DateTime.Now :> obj
let _ = System.DateTime.Now :?> obj
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ValueType, 15, 23; Cat.Operator, 28, 30; Cat.ReferenceType, 31, 34 ] 
         3, [ Cat.Operator, 6, 7; Cat.ValueType, 15, 23; Cat.Operator, 28, 31; Cat.ReferenceType, 32, 35 ]]

[<Test>]
let ``hat operator``() =
    """
let (^) x = x
let _ = List.iter ^ fun _ -> ()
"""
    => [ 2, [ Cat.Operator, 5, 6; Cat.Operator, 10, 11 ]
         3, [ Cat.Operator, 6, 7; Cat.Module, 8, 12; Cat.Function, 13, 17; Cat.Operator, 18, 19 ]]

[<Test>]
let ``enum cases should not be colorized``() =
    """
let _ = System.StringComparison.InvariantCultureIgnoreCase
type InternalEnum = Case1 = 1
let _ = InternalEnum.Case1
"""
    => [ 2, [ Cat.Operator, 6, 7; Cat.ValueType, 15, 31 ]
         4, [ Cat.Operator, 6, 7; Cat.ValueType, 8, 20 ]]