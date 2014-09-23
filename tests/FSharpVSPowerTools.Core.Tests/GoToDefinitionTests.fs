#if INTERACTIVE
#r "System.Runtime.Serialization.dll"
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/SignatureGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharpVSPowerTools.Core.Tests.GoToDefinitionTests
#endif

open NUnit.Framework
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.SignatureGenerator
open FSharpVSPowerTools.Core.Tests.CodeGenerationTestInfrastructure

let languageService = LanguageService(fun _ -> ())
let project() = LanguageServiceTestHelper.projectOptions @"C:\file.fs"

let tryGenerateDefinitionFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, LanguageServiceTestHelper.args)
    let projectOptions = project()

    asyncMaybe {
        let! _range, symbolAtPos =
            liftMaybe <| codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)

        let! generatedCode = liftMaybe <| formatSymbol 4 symbolUse.DisplayContext symbolUse.Symbol
        return generatedCode
    }
    |> Async.RunSynchronously

let generateDefinitionFromPos caretPos src =
    Option.get (tryGenerateDefinitionFromPos caretPos src)

[<Test>]
let ``go to Tuple definition`` () =
    [
        // Explicit 'new' statement: symbol is considered as a type
        """open System
let x = new Tuple<int, int>(1, 2)""", (Pos.fromZ 1 12)

        // Implicit 'new' statement: symbol is considered as a function
        """open System
let x = Tuple<int, int>(1, 2)""", (Pos.fromZ 1 8)
    ]
    |> List.map (fun (src, pos) -> generateDefinitionFromPos pos src)
    |> List.iter (fun src ->
        assertSrcAreEqual src
            """namespace System

type Tuple<'T1, 'T2> =
    interface IComparable
    interface Collections.IStructuralComparable
    interface Collections.IStructuralEquatable
    interface ITuple
    new : item1:'T1 * item2:'T2 -> Tuple<'T1, 'T2>
    member Equals : obj:obj -> bool
    member GetHashCode : unit -> int
    member Item1 : 'T1
    member Item2 : 'T2
    member ToString : unit -> string
""")

[<Test>]
let ``adds necessary parenthesis to function parameters`` () =
    """open System
let _ = Async.AwaitTask"""
    |> generateDefinitionFromPos (Pos.fromZ 1 8)
    |> assertSrcAreEqualForFirstLines 8 """namespace Microsoft.FSharp.Control

[<Sealed>]
[<CompiledName("FSharpAsync")>]
[<Class>]
type Async =
    static member AsBeginEnd : computation:('Arg -> Async<'T>) -> ('Arg * AsyncCallback * obj -> IAsyncResult) * (IAsyncResult -> 'T) * (IAsyncResult -> unit)
    static member AwaitEvent : event:IEvent<'Del,'T> * ?cancelAction:(unit -> unit) -> Async<'T>
"""

[<Test>]
let ``adds necessary parenthesis to tuple parameters`` () =
    """
type T() =
    member this.Test(x: int * int, y: int): int = 3
    
    let x = new T()"""
    |> generateDefinitionFromPos (Pos.fromZ 4 16)
    |> assertSrcAreEqual """type T =
    new : unit -> T
    member Test : x:(int * int) * y:int -> int
"""

[<Test>]
let ``members are sorted this way: abstract member/member/static member`` () =
    """
[<AbstractClass>]
type Abstract =
    member this.P = 3
    abstract member AP: float
    static member SP = 3
    static member SM() = ()
    member this.M1() = ()
    abstract member M: unit -> int

let x: Abstract = Unchecked.defaultof<_>"""
    |> generateDefinitionFromPos (Pos.fromZ 10 7)
    |> assertSrcAreEqual """[<AbstractClass>]
type Abstract =
    abstract member AP : float
    abstract member M : unit -> int
    member M1 : unit -> unit
    member P : int
    static member SM : unit -> unit
    static member SP : int
"""

[<Test>]
let ``go to property definition generate enclosing type metadata`` () =
    """open System

let t = Tuple<int, int>(0, 0)
let u = t.Item1"""
    |> generateDefinitionFromPos (Pos.fromZ 3 10)
    |> assertSrcAreEqual """namespace System

type Tuple<'T1, 'T2> =
    interface IComparable
    interface Collections.IStructuralComparable
    interface Collections.IStructuralEquatable
    interface ITuple
    new : item1:'T1 * item2:'T2 -> Tuple<'T1, 'T2>
    member Equals : obj:obj -> bool
    member GetHashCode : unit -> int
    member Item1 : 'T1
    member Item2 : 'T2
    member ToString : unit -> string
"""

[<Test>]
let ``go to method definition generate enclosing type metadata and supports C# events`` () =
    """open System

do Console.WriteLine("xxx")"""
    |> generateDefinitionFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqualForFirstLines 10 """namespace System

[<Class>]
type Console =
    static member add_CancelKeyPress : value:ConsoleCancelEventHandler -> unit
    static member BackgroundColor : ConsoleColor with get, set
    static member Beep : unit -> unit
    static member Beep : frequency:int * duration:int -> unit
    static member BufferHeight : int with get, set
    static member BufferWidth : int with get, set
"""

[<Test>]
let ``go to F# List<'T> definition`` () =
    """open System

let x: List<int> = []"""
    |> generateDefinitionFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Collections

[<DefaultAugmentation(false)>]
[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpList`1")>]
type List<'T> =
    | ( [] )
    | ( :: ) of Head: 'T * Tail: 'T list
    interface IComparable<List<'T>>
    interface IComparable
    interface Collections.IEnumerable
    interface Collections.Generic.IEnumerable<'T>
    interface Collections.IStructuralComparable
    interface Collections.IStructuralEquatable
    member Head : 'T
    member IsEmpty : bool
    member Item : 'T
    member Length : int
    member Tail : 'T list
    static member Cons : head:'T * tail:'T list -> 'T list
    static member Empty : 'T list
"""

[<Test>]
let ``go to interface definition`` () =
    """
type MyInterface =
    abstract Method: int -> unit

let f (x:MyInterface) = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 4 9)
    |> assertSrcAreEqual """[<Interface>]
type MyInterface =
    abstract member Method : int -> unit
"""

[<Test>]
let ``go to union type definition`` () =
    """open System

let x: Choice<'T, 'U> = failwith "Not implemented yet" """
    |> generateDefinitionFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Core

[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpChoice`2")>]
type Choice<'T1, 'T2> =
    | Choice1Of2 of 'T1
    | Choice2Of2 of 'T2
    interface IComparable<Choice<'T1,'T2>>
    interface IComparable
    interface Collections.IStructuralComparable
    interface Collections.IStructuralEquatable
"""

[<Test>]
let ``go to union case`` () =
    """open System

let x = Choice1Of2 () """
    |> generateDefinitionFromPos (Pos.fromZ 2 9)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Core

[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpChoice`2")>]
type Choice<'T1, 'T2> =
    | Choice1Of2 of 'T1
    | Choice2Of2 of 'T2
    interface IComparable<Choice<'T1,'T2>>
    interface IComparable
    interface Collections.IStructuralComparable
    interface Collections.IStructuralEquatable
"""

[<Test>]
let ``go to total active patterns`` () =
    """
let (|Even|Odd|) i = 
    if i % 2 = 0 then Even else Odd

let testNumber i =
    match i with
    | Even -> printfn "%d is even" i
    | Odd -> printfn "%d is odd" i"""
    |> generateDefinitionFromPos (Pos.fromZ 6 7)
    |> assertSrcAreEqual """val |Even|Odd| : int -> Choice<unit,unit>
"""

[<Test>]
let ``go to partial active patterns`` () =
    """
let (|DivisibleBy|_|) by n = 
    if n % by = 0 then Some DivisibleBy else None
    
let fizzBuzz = function 
    | DivisibleBy 3 & DivisibleBy 5 -> "FizzBuzz" 
    | DivisibleBy 3 -> "Fizz" 
    | DivisibleBy 5 -> "Buzz" 
    | _ -> "" """
    |> generateDefinitionFromPos (Pos.fromZ 5 7)
    |> assertSrcAreEqual """val |DivisibleBy|_| : int -> int -> unit option
"""

[<Test; Ignore("We should not generate implicit interface member definition")>]
let ``go to record type definition`` () =
    """open System
[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string -> unit
    }
    interface ICloneable with
        member x.Clone(): obj = null

let r: MyRecord = Unchecked.defaultof<_>"""
    |> generateDefinitionFromPos (Pos.fromZ 10 7)
    |> assertSrcAreEqual """[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string -> unit
    }
    interface ICloneable
"""

let ``go to record field`` () =
    """open System
type MyRecord =
    {
        Field1: int
        Field2: string -> string
    }

let r = { Field1 = 0; Field2 = id }"""
    |> generateDefinitionFromPos (Pos.fromZ 7 11)
    |> assertSrcAreEqual """type MyRecord =
    {
        Field1: int
        Field2: string -> string
    }
"""

[<Test>]
let ``go to struct metadata`` () =
    """
[<Struct>]
type MyStruct =
    val Field1 : int
    val Field2 : string
    new(x, y) = { Field1 = x; Field2 = y }

let x = new MyStruct()"""
    |> generateDefinitionFromPos (Pos.fromZ 7 12)
    |> assertSrcAreEqual """[<Struct>]
type MyStruct =
    interface System.IComparable<MyStruct>
    interface System.IComparable
    interface System.IEquatable<MyStruct>
    interface System.Collections.IStructuralComparable
    interface System.Collections.IStructuralEquatable
    new : x:int * y:string -> MyStruct
    val Field1 : int
    val Field2 : string
"""

[<Test>]
let ``go to empty class metadata`` () =
    """
type MyClass = class end

let x: MyClass = Unchecked.defaultof<_>"""
    |> generateDefinitionFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """type MyClass =
    class
    end
"""

[<Test>]
let ``go to empty interface metadata`` () =
    """
type MyInterface = interface end

let x: MyInterface = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """type MyInterface =
    interface
    end
"""

[<Test>]
let ``go to empty struct metadata`` () =
    """
[<Struct>]
type MyStruct = struct end

let x = new MyStruct()"""
    |> generateDefinitionFromPos (Pos.fromZ 4 12)
    |> assertSrcAreEqual """type MyStruct =
    struct
        interface System.IComparable<MyStruct>
        interface System.IComparable
        interface System.IEquatable<MyStruct>
        interface System.Collections.IStructuralComparable
        interface System.Collections.IStructuralEquatable
    end
"""

[<Test>]
let ``go to enum type definition`` () =
    """
type Enum =
    | A = 0
    | B = 1
    | C = 2

let x = Enum.A"""
    |> generateDefinitionFromPos (Pos.fromZ 6 8)
    |> assertSrcAreEqual """type Enum =
    | A = 0
    | B = 1
    | C = 2
"""

[<Test>]
let ``go to metadata from module and module function`` () =
    let src = """open System

let f x = Option.map(x)"""

    [
        // From module: 'Option'
        Pos.fromZ 2 10
        // From module function: 'map'
        Pos.fromZ 2 17
    ]
    |> List.map (fun pos -> generateDefinitionFromPos pos src)
    |> List.iter (assertSrcAreEqual """[<CompilationRepresentation(4)>]
module Microsoft.FSharp.Core.Option
val isSome : option:'T option -> bool
val isNone : option:'T option -> bool
val get : option:'T option -> 'T
val count : option:'T option -> int
val fold : folder:('State -> 'T -> 'State) -> state:'State -> option:'T option -> 'State
val foldBack : folder:('T -> 'State -> 'State) -> option:'T option -> state:'State -> 'State
val exists : predicate:('T -> bool) -> option:'T option -> bool
val forall : predicate:('T -> bool) -> option:'T option -> bool
val iter : action:('T -> unit) -> option:'T option -> unit
val map : mapping:('T -> 'U) -> option:'T option -> 'U option
val bind : binder:('T -> 'U option) -> option:'T option -> 'U option
val toArray : option:'T option -> 'T []
val toList : option:'T option -> 'T list
""")
        
[<Test>]
let ``interface inheritance by interfaces`` () =
    """
type T =
    abstract member M: unit -> unit

type T2 =
    inherit T"""
    |> generateDefinitionFromPos (Pos.fromZ 4 5)
    |> assertSrcAreEqual """type T2 =
    interface
        inherit T
    end
"""

[<Test>]
let ``go to type abbreviation definition`` () =
    """
let x: string = null"""
    |> generateDefinitionFromPos (Pos.fromZ 1 7)
    |> assertSrcAreEqual """type string = System.String
"""

[<Test>]
let ``go to abstract class definition with default members`` () =
    """
[<AbstractClass>]
type MyAbstractClass =
    abstract member Method: int -> unit
    default this.Method(x) = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 2 5)
    |> assertSrcAreEqual """[<AbstractClass>]
type MyAbstractClass =
    abstract member Method : int -> unit
    override Method : x:int -> unit
"""

[<Test>]
let ``go to non-abstract class definition with virtual member`` () =
    """
type MyBaseClass() =
    abstract member Method: int -> unit
    default this.Method(x) = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyBaseClass =
    new : unit -> MyBaseClass
    abstract member Method : int -> unit
    override Method : x:int -> unit
"""

[<Test>]
let ``go to subclass class definition with override members`` () =
    """
type MyBaseClass() =
    abstract member Method: int -> unit
    default this.Method(x) = ()

type MyClass() =
    inherit MyBaseClass()
    override this.Method(x) = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 5 5)
    |> assertSrcAreEqual """type MyClass =
    inherit MyBaseClass
    new : unit -> MyClass
    override Method : x:int -> unit
"""

[<Test>]
let ``go to class definition with events`` () =
    """
type Class() =
    let event = Event<int>()
    [<CLIEvent>]
    member this.MyEvent = event.Publish"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type Class =
    new : unit -> Class
    member add_MyEvent : Handler<int> -> unit
    [<CLIEvent>]
    member MyEvent : IEvent<int>
    member remove_MyEvent : Handler<int> -> unit
"""

[<Test>]
let ``go to F# exception definition`` () =
    [
        "exception MyEmptyException"
        "exception MyException of int * (int * string) * (int -> unit)"
    ]
    |> List.map (generateDefinitionFromPos (Pos.fromZ 0 10))
    |> assertSrcSeqAreEqual [
        "exception MyEmptyException\n"
        "exception MyException of int * (int * string) * (int -> unit)\n"
    ]

[<Test>]
let ``handle optional parameters`` () =
    """
open System.Runtime.InteropServices 
type T() =
    static member Method([<Optional; DefaultParameterValue(0)>]?x: int) = (defaultArg x 0) * 2
    static member Method2(?x: int) = (defaultArg x 0) * 2
    """
    |> generateDefinitionFromPos (Pos.fromZ 2 5)
    |> assertSrcAreEqual """type T =
    new : unit -> T
    static member Method : ?x:int -> int
    static member Method2 : ?x:int -> int
"""

[<Test>]
let ``handle delegates`` () =
    [
        "type MyDelegate = delegate of unit * int -> unit"
        "type MyDelegate = delegate of arg1:int -> int"
    ]
    |> List.map (generateDefinitionFromPos (Pos.fromZ 0 5))
    |> assertSrcSeqAreEqual [
        """type MyDelegate =
    delegate of unit * int -> unit
"""

        """type MyDelegate =
    delegate of arg1:int -> int
"""
    ]

[<Test>]
let ``handle nested modules`` () =
    """
let x = Array.map
    """
    |> generateDefinitionFromPos (Pos.fromZ 1 9)
    |> assertSrcAreEqual """[<CompilationRepresentation(4)>]
[<RequireQualifiedAccess>]
module Microsoft.FSharp.Collections.Array
val append : array1:'T [] -> array2:'T [] -> 'T []
val average : array: ^T [] ->  ^T
val averageBy : projection:('T ->  ^U) -> array:'T [] ->  ^U
val blit : source:'T [] -> sourceIndex:int -> target:'T [] -> targetIndex:int -> count:int -> unit
val collect : mapping:('T -> 'U []) -> array:'T [] -> 'U []
val concat : arrays:seq<'T []> -> 'T []
val copy : array:'T [] -> 'T []
val create : count:int -> value:'T -> 'T []
val tryPick : chooser:('T -> 'U option) -> array:'T [] -> 'U option
val fill : target:'T [] -> targetIndex:int -> count:int -> value:'T -> unit
val pick : chooser:('T -> 'U option) -> array:'T [] -> 'U
val choose : chooser:('T -> 'U option) -> array:'T [] -> 'U []
val empty : 'T []
val exists : predicate:('T -> bool) -> array:'T [] -> bool
val exists2 : predicate:('T1 -> 'T2 -> bool) -> array1:'T1 [] -> array2:'T2 [] -> bool
val filter : predicate:('T -> bool) -> array:'T [] -> 'T []
val find : predicate:('T -> bool) -> array:'T [] -> 'T
val findIndex : predicate:('T -> bool) -> array:'T [] -> int
val forall : predicate:('T -> bool) -> array:'T [] -> bool
val forall2 : predicate:('T1 -> 'T2 -> bool) -> array1:'T1 [] -> array2:'T2 [] -> bool
val fold : folder:('State -> 'T -> 'State) -> state:'State -> array:'T [] -> 'State
val foldBack : folder:('T -> 'State -> 'State) -> array:'T [] -> state:'State -> 'State
val fold2 : folder:('State -> 'T1 -> 'T2 -> 'State) -> state:'State -> array1:'T1 [] -> array2:'T2 [] -> 'State
val foldBack2 : folder:('T1 -> 'T2 -> 'State -> 'State) -> array1:'T1 [] -> array2:'T2 [] -> state:'State -> 'State
val get : array:'T [] -> index:int -> 'T
val init : count:int -> initializer:(int -> 'T) -> 'T []
val zeroCreate : count:int -> 'T []
val isEmpty : array:'T [] -> bool
val iter : action:('T -> unit) -> array:'T [] -> unit
val iter2 : action:('T1 -> 'T2 -> unit) -> array1:'T1 [] -> array2:'T2 [] -> unit
val iteri : action:(int -> 'T -> unit) -> array:'T [] -> unit
val iteri2 : action:(int -> 'T1 -> 'T2 -> unit) -> array1:'T1 [] -> array2:'T2 [] -> unit
val length : array:'T [] -> int
val map : mapping:('T -> 'U) -> array:'T [] -> 'U []
val map2 : mapping:('T1 -> 'T2 -> 'U) -> array1:'T1 [] -> array2:'T2 [] -> 'U []
val mapi2 : mapping:(int -> 'T1 -> 'T2 -> 'U) -> array1:'T1 [] -> array2:'T2 [] -> 'U []
val mapi : mapping:(int -> 'T -> 'U) -> array:'T [] -> 'U []
val max : array:'T [] -> 'T
val maxBy : projection:('T -> 'U) -> array:'T [] -> 'T
val min : array:'T [] -> 'T
val minBy : projection:('T -> 'U) -> array:'T [] -> 'T
val ofList : list:'T list -> 'T []
val ofSeq : source:seq<'T> -> 'T []
val partition : predicate:('T -> bool) -> array:'T [] -> 'T [] * 'T []
val permute : indexMap:(int -> int) -> array:'T [] -> 'T []
val reduce : reduction:('T -> 'T -> 'T) -> array:'T [] -> 'T
val reduceBack : reduction:('T -> 'T -> 'T) -> array:'T [] -> 'T
val rev : array:'T [] -> 'T []
val scan : folder:('State -> 'T -> 'State) -> state:'State -> array:'T [] -> 'State []
val scanBack : folder:('T -> 'State -> 'State) -> array:'T [] -> state:'State -> 'State []
val set : array:'T [] -> index:int -> value:'T -> unit
val sub : array:'T [] -> startIndex:int -> count:int -> 'T []
val sort : array:'T [] -> 'T []
val sortBy : projection:('T -> 'Key) -> array:'T [] -> 'T []
val sortWith : comparer:('T -> 'T -> int) -> array:'T [] -> 'T []
val sortInPlaceBy : projection:('T -> 'Key) -> array:'T [] -> unit
val sortInPlaceWith : comparer:('T -> 'T -> int) -> array:'T [] -> unit
val sortInPlace : array:'T [] -> unit
val sum : array: ^T [] ->  ^T
val sumBy : projection:('T ->  ^U) -> array:'T [] ->  ^U
val toList : array:'T [] -> 'T list
val toSeq : array:'T [] -> seq<'T>
val tryFind : predicate:('T -> bool) -> array:'T [] -> 'T option
val tryFindIndex : predicate:('T -> bool) -> array:'T [] -> int option
val unzip : array:('T1 * 'T2) [] -> 'T1 [] * 'T2 []
val unzip3 : array:('T1 * 'T2 * 'T3) [] -> 'T1 [] * 'T2 [] * 'T3 []
val zip : array1:'T1 [] -> array2:'T2 [] -> ('T1 * 'T2) []
val zip3 : array1:'T1 [] -> array2:'T2 [] -> array3:'T3 [] -> ('T1 * 'T2 * 'T3) []

module Parallel = 
    val choose : chooser:('T -> 'U option) -> array:'T [] -> 'U []
    val collect : mapping:('T -> 'U []) -> array:'T [] -> 'U []
    val map : mapping:('T -> 'U) -> array:'T [] -> 'U []
    val mapi : mapping:(int -> 'T -> 'U) -> array:'T [] -> 'U []
    val iter : action:('T -> unit) -> array:'T [] -> unit
    val iteri : action:(int -> 'T -> unit) -> array:'T [] -> unit
    val init : count:int -> initializer:(int -> 'T) -> 'T []
    val partition : predicate:('T -> bool) -> array:'T [] -> 'T [] * 'T []

"""

[<Test; Ignore("activate when static constraints are supported")>]
let ``handle static constraints on module functions`` () =
    """
let inline func x = x + x"""
    |> generateDefinitionFromPos (Pos.fromZ 1 11)
    |> assertSrcAreEqual """
    val inline inlineFunc : x: ^a ->  ^b when  ^a : (static member ( + ) :  ^a *  ^a ->  ^b)
"""

[<Test; Ignore("activate when static constraints are supported")>]
let ``handle static constraints on members`` () =
    """
type MyClass() =
    member inline __.Test x = x + x
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyClass =
    new : unit -> MyClass
    member Test : x: ^a ->  ^b when  ^a : (static member ( + ) :  ^a *  ^a ->  ^b)
"""

[<Test>]
let ``handle record extension members`` () =
    """
type MyRecord =
    { A: string; B: int }
    member __.Method1() = ()

type MyRecord with
    member __.Method2() = ()
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyRecord =
    {
        A: string
        B: int
    }
    interface System.IComparable<MyRecord>
    interface System.IComparable
    interface System.IEquatable<MyRecord>
    interface System.Collections.IStructuralComparable
    interface System.Collections.IStructuralEquatable
    member Method1 : unit -> unit
    member Method2 : unit -> unit
"""

[<Test>]
let ``handle union type extension members`` () =
    """
type MyUnion = A of int | B of float
with
    member __.Method1() = ()

type MyUnion with
    member __.Method2() = "allo!"
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyUnion =
    | A of int
    | B of float
    interface System.IComparable<MyUnion>
    interface System.IComparable
    interface System.IEquatable<MyUnion>
    interface System.Collections.IStructuralComparable
    interface System.Collections.IStructuralEquatable
    member Method1 : unit -> unit
    member Method2 : unit -> string
"""

[<Test>]
let ``handle class extension members`` () =
    """
type MyClass() =
    member x.Method() = ()

type MyClass with
    member x.MyExtensionMethod() = ()
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyClass =
    new : unit -> MyClass
    member Method : unit -> unit
    member MyExtensionMethod : unit -> unit
"""

[<Test>]
let ``handle class properties with setter``() =
    """
type MyClass() =
    let mutable instanceValue = 0
    static let mutable staticValue = 0

    member val GetterAndSetter = 0 with get, set
    member __.SetterOnly with set(value) = instanceValue <- value

    static member val StaticGetterAndSetter = 0 with get, set
    static member StaticSetterOnly with set(value) = staticValue <- value
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyClass =
    new : unit -> MyClass
    member GetterAndSetter : int with get, set
    member SetterOnly : int with set
    static member StaticGetterAndSetter : int with get, set
    static member StaticSetterOnly : int with set
"""

[<Test>]
let ``handle union case attributes`` () =
    """
type Union =
    | [<System.Diagnostics.Conditional("MyConditional")>]
      [<System.Obsolete("hello")>]
      Case1 of int
    | [<System.Obsolete("cuir")>] Case2 of string
    | Case3
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type Union =
    | [<Conditional("MyConditional")>]
      [<Obsolete("hello")>]
      Case1 of int
    | [<Obsolete("cuir")>]
      Case2 of string
    | Case3
    interface System.IComparable<Union>
    interface System.IComparable
    interface System.IEquatable<Union>
    interface System.Collections.IStructuralComparable
    interface System.Collections.IStructuralEquatable
"""

[<Test>]
let ``handle record field attributes`` () =
    """open System
open System.Runtime.Serialization
type Record = {
    [<DefaultValue>]
    [<Obsolete("Reason1")>]
    Field1: int

    [<Obsolete("Reason2")>]
    Field2: float
}"""
    |> generateDefinitionFromPos (Pos.fromZ 2 5)
    |> assertSrcAreEqual """type Record =
    {
        [<DefaultValue>]
        [<Obsolete("Reason1")>]
        Field1: int
        [<Obsolete("Reason2")>]
        Field2: float
    }
    interface IComparable<Record>
    interface IComparable
    interface IEquatable<Record>
    interface Collections.IStructuralComparable
    interface Collections.IStructuralEquatable
"""

[<Test; Ignore("activate when method/property attributes are supported by FCS")>]
let ``handle property/method attributes``() =
    """
type MyClass() =
    [<Obsolete("Prop is obsolete")>]
    member __.Prop = 0
    [<Obsolete("Method is obsolete")>]
    member __.Method() = ()
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyClass =
    new : unit -> MyClass
    [<Obsolete("Method is obsolete")>]
    member Method : unit -> unit
    [<Obsolete("Prop is obsolete")>]
    member Prop : int
"""

[<Test>]
let ``handle generic constraints on type`` () =
    [
        """open System
type MyClass<'T, 'U when 'T : null and 'T : (new : unit -> 'T) and 'U : struct>() =
    member x.Method() = ()
"""

        """open System
type MyClass<'T, 'U when 'T :> IComparable and 'U : not struct>() =
    member x.Method() = ()
"""

        """open System
type MyClass<'T, 'U when 'T : comparison and 'U : equality>() =
    member x.Method() = ()
"""

        """open System
type MyClass<'T, 'U when 'T : unmanaged and 'U : enum<uint32>>() =
    member x.Method() = ()
"""

        """open System
type MyClass<'T when 'T : delegate<obj * int, unit>>() =
    member x.Method() = ()
"""
    ]
    |> List.map (generateDefinitionFromPos (Pos.fromZ 1 5))
    |> assertSrcSeqAreEqual [
        """type MyClass<'T, 'U when 'T : null and 'T : (new : unit -> 'T) and 'U : struct> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """type MyClass<'T, 'U when 'T :> IComparable and 'U : not struct> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """type MyClass<'T, 'U when 'T : comparison and 'U : equality> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """type MyClass<'T, 'U when 'T : unmanaged and 'U : enum<uint32>> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """type MyClass<'T when 'T : delegate<obj * int, unit>> =
    new : unit -> MyClass<'T>
    member Method : unit -> unit
"""
    ]

[<Test>]
let ``handle generic constraints on methods`` () =
    """
type MyClass<'T when 'T : struct>() =
    member __.NormalMethod() = ()
    member __.Method<'X when 'X : null>(x: 'X) = 0
    static member StaticMethod<'X when 'X : equality>(x: 'X * 'X) =
        x = x
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """type MyClass<'T when 'T : struct> =
    new : unit -> MyClass<'T>
    member Method : x:'X -> int when 'X : null
    member NormalMethod : unit -> unit
    static member StaticMethod : x:('X * 'X) -> bool when 'X : equality
"""

[<Test>]
let ``handle statically resolved constraints on types`` () =
    [
        """open System
type MyClass<'T when 'T : (static member Create : unit -> 'T) and 'T : (member Prop : int)> =
    class end
"""

        """open System
type MyClass<'T when 'T : (member Create : int * 'T -> 'T)> =
    class end
"""
    ] 
    |> List.map (generateDefinitionFromPos (Pos.fromZ 1 5))
    |> assertSrcSeqAreEqual [
        """type MyClass< ^T when ^T : (static member Create : unit ->  ^T) and ^T : (member get_Prop :  ^T -> int)> =
    class
    end
"""

        """type MyClass< ^T when ^T : (member Create :  ^T * int *  ^T ->  ^T)> =
    class
    end
"""
    ]

// BUG: demangle operator name doesn't escape names with spaces
// TODO: special formatting for instance member constraints
// TODO: special formatting for properties??

// Tests to add:
// TODO: class method arguments attributes
// TODO: xml comments
// TODO: include open directives so that IStructuralEquatable/... are not wiggled
// ENHANCEMENT: special formatting for Events?
// TODO: syntax coloring is deactivated on generated metadata file
// TODO: buffer should have the same behavior as C#'s generated metadata ([from metadata] instead of [read-only] header, preview buffer and not permanent buffer)
// TODO: add test for VS buffer name?
// TODO: set cursor on method/... when symbol is a method/union case/enum value/record field