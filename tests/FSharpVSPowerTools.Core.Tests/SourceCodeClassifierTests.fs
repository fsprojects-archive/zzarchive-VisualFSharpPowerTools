module FSharpVSPowerTools.Core.Tests.SourceCodeClassifierTests

open System.IO
open NUnit.Framework
open FSharpVSPowerTools

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpTargetFramework.NET_4_5
let languageService = LanguageService(fun _ -> ())
let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, LanguageServiceTestHelper.args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

let (=>) source (expected: (int * ((Category * int * int) list)) list) = 
    let opts = opts source
    
    let sourceLines = source.Replace("\r\n", "\n").Split([|"\n"|], System.StringSplitOptions.None)

    let lexer = 
        { new LexerBase() with
            member x.GetSymbolFromTokensAtLocation (_tokens, line, col) =
                let lineStr = sourceLines.[line]
                Lexer.getSymbol source line col lineStr LanguageServiceTestHelper.args Lexer.queryLexState
            member x.TokenizeLine line =
                let lineStr = sourceLines.[line]
                Lexer.tokenizeLine source LanguageServiceTestHelper.args line lineStr Lexer.queryLexState }

    let symbolsUses =
        languageService.GetAllUsesOfAllSymbolsInFile (opts, fileName, sourceLines, AllowStaleResults.No, true,
                                                      (fun _ -> async { return Some [opts] }), lexer)
        |> Async.RunSynchronously

    let parseResults = 
        languageService.ParseFileInProject(opts, fileName, source) |> Async.RunSynchronously

    let actualCategories =
        let entities =
            languageService.GetAllEntitiesInProjectAndReferencedAssemblies (opts, fileName, source)
            |> Async.RunSynchronously
        let qualifyOpenDeclarations line endColumn idents = 
            let lineStr = sourceLines.[line - 1]
            languageService.GetIdentTooltip (line, endColumn, lineStr, Array.toList idents, opts, fileName, source)
            |> Async.RunSynchronously
            |> function
               | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
               | None -> []
        let openDeclarations = OpenDeclarationGetter.getOpenDeclarations parseResults.ParseTree entities qualifyOpenDeclarations
        let allEntities =
            entities
            |> Option.map (fun entities -> 
                entities 
                |> List.map (fun e -> e.FullName, e.CleanIdents)
                |> Map.ofList)
        SourceCodeClassifier.getCategoriesAndLocations (symbolsUses, parseResults.ParseTree, lexer, openDeclarations, allEntities)
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
    with _ -> 
        debug "AST: %A" parseResults.ParseTree; 
        reraise()

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
    => [ 2, [ Category.ValueType, 27, 30 ]
         3, [ Category.ValueType, 22, 27 ]
         4, [ Category.ValueType, 34, 42 ]
         5, [ Category.ValueType, 5, 18 ]
         6, [ Category.ValueType, 5, 30; Category.ValueType, 33, 46 ]
         7, [ Category.ValueType, 20, 33 ] 
         8, [ Category.ValueType, 31, 56; Category.ValueType, 59, 84 ]]

[<Test>]
let ``DU case of function``() =
    """
type DUWithFunction = FuncCase of (unit -> unit)
let (FuncCase funcCase) = FuncCase (fun() -> ())
match FuncCase (fun() -> ()) with FuncCase func -> func()
"""
    => [ 2, [ Category.ReferenceType, 5, 19; Category.PatternCase, 22, 30; Category.ReferenceType, 35, 39; Category.ReferenceType, 43, 47 ]
         3, [ Category.PatternCase, 5, 13; Category.Function, 14, 22; Category.PatternCase, 26, 34 ]
         4, [ Category.PatternCase, 6, 14; Category.PatternCase, 34, 42; Category.Function, 43, 47; Category.Function, 51, 55 ]]

[<Test>]
let ``double quoted function without spaces``() = 
    """
let ``double_quoted_function_without_spaces`` () = ()
"""
    => [ 2, [ Category.Function, 4, 45 ]]

[<Test>]
let ``double quoted function with spaces``() = 
    """
let ``double quoted function with spaces`` () = ()
"""
    => [ 2, [ Category.Function, 4, 42 ]]

[<Test>]
let ``fully qualified attribute``() = 
    """
[<System.Diagnostics.DebuggerDisplay "name">]
type TypeWithAttribute() = class end
"""
    => [ 2, [ Category.ReferenceType, 21, 36 ]]

[<Test>]
let ``async type``() = 
    """
let asyncRunSync = Async.RunSynchronously
"""
    => [ 2, [ Category.Function, 4, 16; Category.ReferenceType, 19, 24; Category.Function, 25, 41 ]]

[<Test>]
let ``standard computation expression name``() = 
    """
seq {
    let func x = x
    yield func 1
} |> ignore
"""
    => [ 2, []
         3, [ Category.Function, 8, 12 ]
         4, [ Category.Function, 10, 14 ]]

[<Test>]
let ``used let bindings in computation expression should not be marked as unused``() = 
    """
seq {
    let func x = x
    yield func 1
} |> ignore
"""
    => [ 2, []
         3, [ Category.Function, 8, 12 ]
         4, [ Category.Function, 10, 14 ]]

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
    => [ 7, []]

[<Test>]
let ``method chain``() =
    """
let _ = System.Environment.MachineName.ToLower()
"""
    => [ 2, [ Category.ReferenceType, 15, 26; Category.Function, 39, 46 ]]
    
[<Test>]
let ``complex method chain``() =
    """
let _ = System.Guid.NewGuid().ToString("N").Substring(1)
"""
    => [ 2, [ Category.ValueType, 15, 19; Category.Function, 20, 27; Category.Function, 30, 38; Category.Function, 44, 53 ]]

[<Test>]
let ``generic type with ignored type parameter``() = 
    """
let _ = list<_>.Empty
"""
    => [ 2, [ Category.ReferenceType, 8, 12 ]]

[<Test>]
let ``F# namespace``() = 
    """
let _ = Microsoft.FSharp.Collections.List<int>.Empty
"""
    => [ 2, [ Category.ReferenceType, 37, 41; Category.ValueType, 42, 45 ]]
       
[<Test>]
let ``double quoted member``() = 
    """
type System.String with
    member __.``Long func``() = "x"
let _ = "x".``Long func``().Substring(3)
"""
    => [ 4, [ Category.Function, 12, 25; Category.Function, 28, 37 ]]

[<Test>]
let ``indexer``() = 
    """
let arr = [|1|]
let _ = arr.[0]
"""
    => [ 3, [ Category.Module, 11, 12 ]]

[<Test>]
let ``mutable value``() = 
    """
let mutable mutableValue = 1
"""
    => [ 2,  [ Category.MutableVar, 12, 24 ]]

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
    => [ 3, [ Category.MutableVar, 14, 26; Category.ValueType, 28, 31 ]
         5, [ Category.MutableVar, 16, 28 ]
         8, [ Category.MutableVar, 16, 31; Category.MutableVar, 39, 54 ]]

[<Test>]
let ``reference value``() = 
    """
let refValue = ref 1
refValue := !refValue + 1
""" 
    => [ 2, [ Category.MutableVar, 4, 12; Category.Function, 15, 18 ]
         3, [ Category.MutableVar, 0, 8; Category.MutableVar, 13, 21 ]]

[<Test>]
let ``reference field``() = 
    """
type ClassWithRefValue() =
    let refValue = ref 1
    let _ = !refValue
type RecordWithRefValue = 
    { Field: int ref }
"""
    => [ 3, [ Category.Function, 19, 22; Category.MutableVar, 8, 16 ]
         4, [ Category.MutableVar, 13, 21 ]
         6, [ Category.MutableVar, 6, 11; Category.ValueType, 13, 16; Category.ReferenceType, 17, 20 ]]

[<Test>]
let ``single line quotation``() = 
    """
let _ = <@ 1 = 1 @>
"""
    => [ 2, [ Category.Quotation, 8, 19 ]]

[<Test>]
let ``multi line quotation``() = 
    """
let _ = <@ 1 = 1
           && 2 = 2 @>
"""
    => [ 2, [ Category.Quotation, 8, 16 ]
         3, [ Category.Quotation, 11, 22 ]]

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
    => [ 2, [ Category.Function, 8, 10; Category.Quotation, 11, 22 ]
         4, [ Category.Function, 8, 9; Category.Quotation, 10, 21; Category.Quotation, 22, 33 ]
         9, [ Category.Quotation, 6, 16 ]
         11, [ Category.Function, 8, 11; Category.Quotation, 16, 23 ]]

[<Test>]
let ``quotation in type``() = 
    """
type TypeWithQuotations() =
    let _ = <@ 1 = 1 @>
    member __.F() = <@ 1 = 1 @>
    member __.P = <@ 1 + 1 @>
"""
    => [ 3, [ Category.Quotation, 12, 23 ]
         4, [ Category.Function, 14, 15; Category.Quotation, 20, 31 ]
         5, [ Category.Quotation, 18, 29 ]]

[<Test>]
let ``untyped quotation``() = 
    """
let _ = <@@ 1 @@>
"""
    => [ 2, [ Category.Quotation, 8, 17 ]]

[<Test>]
let ``complicated quotation layout``() = 
    """
let f x y = ()
let _  = f <@ 1
              + 2
              + 3 @> <@@ 1 @@>
"""
    => [ 3, [ Category.Function, 9, 10; Category.Quotation, 11, 15 ]
         4, [ Category.Quotation, 14, 17 ]
         5, [ Category.Quotation, 14, 20; Category.Quotation, 21, 30 ]]

[<Test>]
let ``quotation in lambda``() = 
    """
let _ = fun() -> <@ 1 @>
"""
    => [ 2, [ Category.Quotation, 17, 24 ]]

[<Test>]
let ``quotation in record``() = 
    """
type RecordWithQuotation = { Field: Microsoft.FSharp.Quotations.Expr<int> }
let _ = { Field = <@ 1 @> }
"""
    => [ 3, [ Category.Quotation, 18, 25 ]]

[<Test>]
let ``quotation in list expression``() = 
    """
let _ = [ <@ 1 @> ]
"""
    => [ 2, [ Category.Quotation, 10, 17 ]]

[<Test>]
let ``quotation in seq for expression``() = 
    """
let _ = seq { for i in [1..10] -> <@ i @> }
"""
    => [ 2, [ Category.Quotation, 34, 41 ]]

[<Test>]
let ``quotation as a result of function``() = 
    """
let qf() : Microsoft.FSharp.Quotations.Expr<int> =
    <@ 1 @>
"""
    => [ 3, [ Category.Quotation, 4, 11 ]]

[<Test>]
let ``quotation as default constructor arguments``() = 
    """
type ClassWithQuotationInConstructor(expr) = class end
let _ = ClassWithQuotationInConstructor(<@ 1 @>)
"""
    => [ 3, [ Category.ReferenceType, 8, 39; Category.Quotation, 40, 47 ]]

[<Test>]
let ``quotation as initialization of auto property``() = 
    """
type ClassWithWritableProperty() =
    member val Prop = <@@ 1 @@> with get, set
"""
    => [ 3, [ Category.MutableVar, 15, 19; Category.Quotation, 22, 31 ]]

[<Test>]
let ``quotation in property setter``() = 
    """
type ClassWithWritableProperty() =
    member val Prop = <@@ 1 @@> with get, set
let clWithWritableProperty = ClassWithWritableProperty()
clWithWritableProperty.Prop <- <@@ 2 @@>
"""
    => [ 5, [ Category.Quotation, 31, 40 ]]

[<Test>]
let ``quotation in nested module``() = 
    """
module NestedModule =
    let _ = <@ 1 @>
"""
    => [ 3, [ Category.Quotation, 12, 19 ]]

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
    => [ 6, [ Category.Quotation, 16, 23 ]
         7, [ Category.Function, 11, 17; Category.Quotation, 18, 25 ]
         8, [ Category.Function, 17, 20; Category.Quotation, 21, 28 ]
         10, [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
         12, [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]
         13, [ Category.Function, 12, 19; Category.Quotation, 20, 28 ]
         14, [ Category.Quotation, 14, 21 ]
         17, [ Category.Quotation, 19, 26 ]
         19, [ Category.Function, 20, 23; Category.Quotation, 24, 31 ]]

[<Test>]
let ``tuple alias``() = 
    """
type Tuple = int * string
let tupleFunc (x: Tuple) : Tuple = x
"""
    => [ 2, [ Category.ReferenceType, 5, 10; Category.ValueType, 13, 16; Category.ReferenceType, 19, 25 ]    
         3, [ Category.Function, 4, 13; Category.ReferenceType, 18, 23; Category.ReferenceType, 27, 32 ]]

[<Test>]
let ``multiline method chain``() = 
    """
let _ =
    "string"
        .Substring(1)
        .Trim().Remove(1)
"""
    => [ 4, [ Category.Function, 9, 18 ]
         5, [ Category.Function, 9, 13; Category.Function, 16, 22 ]]

[<Test>]
let ``module``() = 
    """
module Module1
module Module2 =
    module Module3 =
        let x = ()
"""
    => [ 2, [ Category.Module, 7, 14 ]
         3, [ Category.Module, 7, 14 ]
         4, [ Category.Module, 11, 18 ]]

[<Test>]
let ``static CLR class``() = 
    """
let _ = System.Linq.Enumerable.Range(0, 1)
"""
    => [ 2, [ Category.ReferenceType, 20, 30; Category.Function, 31, 36 ]]

[<Test>]
let ``F# external modules``() = 
    """
let _ = [1] |> Seq.sort |> Seq.toList |> List.rev
"""
    => [ 2, [ Category.Module, 15, 18; Category.Function, 19, 23
              Category.Module, 27, 30; Category.Function, 31, 37
              Category.Module, 41, 45; Category.Function, 46, 49 ]]

[<Test>]
let ``byref argument``() = 
    """
let ``func with byref arg`` (_: byref<int>) = ()
"""
    => [ 2, [ Category.Function, 4, 27
              Category.ReferenceType, 32, 37
              Category.ValueType, 38, 41 ]]

[<Test>]
let ``unit of measure``() =
    """
[<Measure>] type ms
let _: int<ms> = 
    1<ms>
type RecordWithUnitOfMeasure =
    { Field1: int<ms> }
"""
    => [ 3, [ Category.ValueType, 7, 10; Category.ReferenceType, 11, 13 ]
         4, [ Category.ReferenceType, 6, 8 ]
         6, [ Category.ValueType, 14, 17; Category.ReferenceType, 18, 20 ]]

[<Test>]
let ``standard and custom numeric literals``() = 
    """
let _ = 1I
module NumericLiteralZ =
    let FromInt32 (i: int) = i
let _ = 77Z
"""
    => [ 2, []    
         5, []]

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
    => [ 3, [ Category.Function, 8, 10; Category.ReferenceType, 16, 19; Category.ReferenceType, 34, 37 ]
         4, [ Category.Function, 8, 11; Category.ReferenceType, 16, 19; Category.ReferenceType, 34, 37 ] 
         5, [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
         6, [ Category.Function, 8, 10; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ] 
         7, [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
         8, [ Category.Function, 8, 9; Category.ReferenceType, 15, 18; Category.ReferenceType, 33, 36 ]
         9, [ Category.Function, 8, 9; Category.ReferenceType, 42, 46; Category.ReferenceType, 83, 87 ]]

[<Test>]
let ``array alias``() =
    """
type ArrayAlias = byte[]
"""
    => [ 2, [ Category.ReferenceType, 5, 15; Category.ValueType, 18, 22 ]]

[<Test; Ignore "Lexer cannot recognize (|P|_|) as an Ident at position of the last bar">]
let ``active pattern``() =
    """
let (|ActivePattern|_|) x = Some x
let _ = (|ActivePattern|_|) 1
"""
    => [ 2, [ Category.PatternCase, 6, 19; Category.PatternCase, 28, 32 ]
         3, [ Category.Function, 8, 27 ]]

[<Test>]
let ``non public module``() =
    """
module private PrivateModule =
    let x = ()
"""
    => [ 2, [ Category.Module, 15, 28 ]]

[<Test>]
let ``unused non public module function and value``() =
    """
module private PrivateModule =
    let func _ = ()
    let value = ()
"""
    => [ 3, [ Category.Unused, 8, 12 ]  
         4, [ Category.Unused, 8, 13 ]]

[<Test>]
let ``unused default constructor of non public class``() =
    """
type private PrivateClass() = class end
"""
    => [ 2, [ Category.Unused, 13, 25 ]]

[<Test>]
let ``unused non public class let binding``() =
    """
type PublicClass() =
    let letValue = 1
    let letFunc _ = ()
    member __.P = ()
"""
    => [ 3, [ Category.Unused, 8, 16] 
         4, [ Category.Unused, 8, 15]]

[<Test>]
let ``unused non public class member``() =
    """
type PublicClass() =
    member private __.Prop = ()
    member private __.Method _ = ()
"""
    => [ 3, [ Category.Unused, 22, 26] 
         4, [ Category.Unused, 22, 28]]

[<Test>]
let ``unused self binding``() =
    """
type PublicClass() =
    member this.PublicMethod _ = ()
""" 
    => [ 3, [ Category.Unused, 11, 15; Category.Function, 16, 28 ]]

[<Test>]
let ``used self binding``() =
    """
type PublicClass() =
    member this.Method2 _ = this
"""
    => [ 3, [ Category.Function, 16, 23 ]]

[<Test>]
let ``unused function / member argument``() =
    """
type PublicClass() =
    member __.Method1 (arg1: int, arg2) = arg2
let func arg1 arg2 = arg2
"""
    => [ 3, [ Category.Function, 14, 21; Category.Unused, 23, 27; Category.ValueType, 29, 32 ]
         4, [ Category.Function, 4, 8; Category.Unused, 9, 13 ]]

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
let ``unused DU field names are not marked as unused even though they are not used anywhere``() =
    """
type DU = Case of field1: int * field2: string
let _ = Case (1, "")
"""
    => [ 2, [ Category.ReferenceType, 5, 7
              Category.PatternCase, 10, 14
              Category.ValueType, 26, 29
              Category.ReferenceType, 40, 46 ]]

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

[<Test>]
let ``open statement duplication in parent module is unused``() =
    """
module TopModule
open System.IO
module Nested =
    open System.IO
    let _ = File.Create ""
"""
    => [ 3, [ Category.Unused, 5, 14 ]
         5, []]

[<Test>]
let ``open statement duplication in parent module is not unused while it's actually used in its scope``() =
    """
module TopModule
open System.IO
module Nested =
    open System.IO
    let _ = File.Create ""
let _ = File.Create ""
"""
    => [ 3, []
         5, []]

[<Test>]
let ``multiple open declaration in the same line``() =
    """
open System.IO; let _ = File.Create "";; open System.IO
"""
    => [ 2, [ Category.ReferenceType, 24, 28; Category.Function, 29, 35; Category.Unused, 46, 55 ]]

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
let ``last of several equivalent open declarations is market as used, the rest of them are marked as unused``() =
    """
module NormalModule =
    [<AutoOpen>]
    module AutoOpenModule1 =
        module NestedNormalModule =
            [<AutoOpen>]
            module AutoOpenModule2 =
                [<AutoOpen>]
                module AutoOpenModule3 =
                    type Class() = class end

open NormalModule.AutoOpenModule1.NestedNormalModule.AutoOpenModule2
open NormalModule.AutoOpenModule1.NestedNormalModule
let _ = Class()
"""
    => [ 12, [ Category.Unused, 5, 68 ]
         13, []]
    
[<Test>]
let ``open declaration is not marked as unused if there is a shortened attribute symbol from it``() =
    """
open System
[<Serializable>]
type Class() = class end
"""
    => [ 2, []]
    
[<Test>]
let ``open declaration is not marked as unused if an extension property is used``() =
    """
module Module =
    type System.String with
        member __.ExtensionProperty = ()
open Module
let _ = "a long string".ExtensionProperty
"""
    => [ 5, []]

[<Test>]
let ``open declaration is marked as unused if an extension property is not used``() =
    """
module Module =
    type System.String with
        member __.ExtensionProperty = ()
open Module
let _ = "a long string".Trim()
"""
    => [ 5, [ Category.Unused, 5, 11 ]]

[<Test>]
let ``open declaration is not marked as unused if an extension method is used``() =
    """
type Class() = class end
module Module =
    type Class with
        member __.ExtensionMethod() = ()
open Module
let x = Class()
let _ = x.ExtensionMethod()
"""
    => [ 6, []]

[<Test>]
let ``open declaration is marked as unused if an extension method is not used``() =
    """
type Class() = class end
module Module =
    type Class with
        member __.ExtensionMethod() = ()
open Module
let x = Class()
"""
    => [ 6, [ Category.Unused, 5, 11 ]]

[<Test>]
let ``open declaration is not marked as unused if a type from it used in a constructor signarute``() =
    """
module M =
    type Class() = class end
open M
type Site (x: Class -> unit) = class end
"""
    => [ 4, []]   

[<Test>]
let ``open declaration is marked as unused if nothing from it is used``() =
    """
module M =
    type Class() = class end
open M
type Site (x: int -> unit) = class end
"""
    => [ 4, [ Category.Unused, 5, 6 ] ]

[<Test>]
let ``static extension method applied to a type results that both namespaces /where the type is declared and where the extension is declared/ is not marked as unudes``() =
    """
module Extensions =
    type System.DateTime with
        static member ExtensionMethod() = ()
open System
open Extensions
let _ = DateTime.ExtensionMethod
"""
    => [ 5, []; 6, []]
    
[<Test>]
let ``static extension property applied to a type results that both namespaces /where the type is declared and where the extension is declared/ is not marked as unudes``() =
    """
module Extensions =
    type System.DateTime with
        static member ExtensionProperty = ()
open System
open Extensions
let _ = DateTime.ExtensionProperty
"""
    => [ 5, []; 6, []]

[<Test>]
let ``accessing property on a variable should not force the namespace in which the type is declared to be marked as used``() =
    """
let dt = System.DateTime.Now
module M =
    open System
    let _ = dt.Hour
"""
    => [4, [ Category.Unused, 9, 15 ]]

[<Test>]
let ``either of two open declarations are not marked as unused if symbols from both of them are used``() =
    """
module M1 =
    module M2 =
        let func1 _ = ()
        module M3 =
            let func2 _ = ()
open M1.M2.M3
open M1.M2
let _ = func1()
let _ = func2()
"""
    => [ 7, []; 8, []]
        
[<Test>]
let ``open module with ModuleSuffix attribute value applied is not marked as unused if a symbol declared in it is used``() =
    """
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module M =
    let func _ = ()
open M
let _ = func()
"""
    => [ 5, []]

[<Test>]
let ``open module all of which symbols are used by qualifier is marked as unused``() =
    """
module M =
    let func _ = ()
open M
let _ = M.func 1
"""
    => [4, [Category.Unused, 5, 6 ]]

[<Test>]
let ``open module is not marked as unused if a symbol defined in it is used in OCaml-style type annotation``() =
    """
module M =
    type Class() = class end
open M
let func (arg: Class list) = ()
"""
    => [ 4, []]

[<Test>]
let ``auto open module``() =
    """
module Top =
    [<AutoOpen>]
    module M =
        let func _ = ()
open Top
let _ = func()
"""
    => [ 6, []]

[<Test>]
let ``auto open module in the middle of hierarchy``() =
    """
namespace Ns
module M1 =
    [<AutoOpen>]
    module MA1 = 
        let func _ = ()
open M1
module M2 =
    let _ = func()
"""
    => [ 7, []]

[<Test>]
let ``open declaration is not marked as unused if a delegate defined in it is used``() =
    """
open System
let _ = Func<int, int>(fun _ -> 1)
"""
    => [ 2, []]

[<Test>]
let ``relative module open declaration``() =
    """
module Top =
    module Nested = 
        let x = 1
open Top
open Nested
let _ = x
"""
    => [ 5, []; 6, []]

[<Test>]
let ``open declaration is used if a symbol defined in it is used in a module top-level do expression``() =
    """
module Top
open System.IO
File.ReadAllLines ""
|> ignore
"""
    => [ 3, []]

[<Test>]
let ``redundant opening a module with ModuleSuffix attribute value is marks as unused``() =
    """
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module InternalModuleWithSuffix =
    let func1 _ = ()
module M =
    open InternalModuleWithSuffix
    let _ = InternalModuleWithSuffix.func1()
"""
    => [ 6, [Category.Unused, 9, 33 ]]
    
[<Test>]
let ``redundant opening a module is marks as unused``() =
    """
module InternalModuleWithSuffix =
    let func1 _ = ()
module M =
    open InternalModuleWithSuffix
    let _ = InternalModuleWithSuffix.func1()
"""
    => [ 5, [Category.Unused, 9, 33 ]]

[<Test>]
let ``usage of an unqualified union case makes opening module in which it's defined not maked as unused``() =
    """
module M =
    type DU = Case1
open M
let _ = Case1
"""
    => [ 4, []]

[<Test>]
let ``usage of qualified union case makes opening module in which it's defined not maked as unused``() =
    """
module M =
    type DU = Case1
open M
let _ = DU.Case1
"""
    => [ 4, []]

[<Test>]
let ``Microsoft FSharp Quotations Expr type``() =
    """
open Microsoft.FSharp.Quotations
let _ = Expr.Coerce (<@@ 1 @@>, typeof<int>)
"""
    => [2, []]



//let _: Expr = <@@ 1 @@>
