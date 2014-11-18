#if INTERACTIVE
#r "System.Xml.Linq.dll"
#r "System.Runtime.Serialization.dll"
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/UntypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/IdentifierUtils.fs"
      "../../src/FSharpVSPowerTools.Core/OpenDeclarationsGetter.fs"
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
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.SignatureGenerator
open FSharpVSPowerTools.Core.Tests.CodeGenerationTestInfrastructure
open System
open System.Xml.Linq
open System.Collections.Generic
open System.IO

let languageService = LanguageService()
let project() = LanguageServiceTestHelper.projectOptions @"C:\file.fs"
let xmlFileCache = Dictionary()

let tryGenerateDefinitionFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, LanguageServiceTestHelper.args)
    let projectOptions = project()

    asyncMaybe {
        let! _range, symbolAtPos =
            liftMaybe <| codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)
        let! parseResults =
            codeGenService.ParseFileInProject(document, projectOptions)
            |> liftAsync
        let! parseTree = parseResults.ParseTree |> liftMaybe           
        let getXmlDocBySignature = 
            let xmlFile = symbolUse.Symbol.Assembly.FileName |> Option.map (fun fileName -> Path.ChangeExtension(fileName, ".xml"))
            let xmlMemberMap =
                match xmlFile with
                | Some xmlFile ->
                    match xmlFileCache.TryGetValue(xmlFile) with
                    | true, xmlMemberMap ->
                        xmlMemberMap
                    | false, _ ->
                        let xmlMemberMap = Dictionary()
                        let doc = XDocument.Load(xmlFile)
                        if doc <> null then                            
                            for key, value in 
                              [ for e in doc.Descendants(XName.Get "member") do
                                  let attr = e.Attribute(XName.Get "name") 
                                  if attr <> null && not (String.IsNullOrEmpty(attr.Value)) then 
                                    yield attr.Value, e ] do
                                xmlMemberMap.Add(key, value)
                            xmlFileCache.Add(xmlFile, xmlMemberMap)
                        xmlMemberMap
                | None -> 
                    Dictionary()
            fun signature ->
                match xmlMemberMap.TryGetValue(signature) with
                | true, element ->
                    try [ element.Element(XName.Get "summary").Value ] with _ -> []
                | false, _ ->
                    []
            
        let openDeclarations = OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation caretPos parseTree
        let! generatedCode = liftMaybe <| formatSymbol getXmlDocBySignature 4 symbolUse.DisplayContext openDeclarations symbolUse.Symbol
        return generatedCode
    }
    |> Async.RunSynchronously

let validateSignature source signature =
    let projFileName = @"C:\Project.fsproj"
    let sourceFile = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    File.WriteAllText(sourceFile, source)
    let signatureFile = @"C:\Temp.fsi"
    let opts =
        { ProjectFileName = projFileName
          ProjectFileNames = [| sourceFile; signatureFile|]
          OtherOptions = LanguageServiceTestHelper.args
          ReferencedProjects = Array.empty
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = true
          LoadTime = DateTime.UtcNow
          UnresolvedReferences = None }
    let results =
        languageService.ParseAndCheckFileInProject(opts, signatureFile, signature, AllowStaleResults.No)
        |> Async.RunSynchronously
    results.GetErrors()

let generateDefinitionFromPos caretPos src =
    let signature = Option.get (tryGenerateDefinitionFromPos caretPos src)
    match validateSignature src signature with
    | None | Some [||] -> ()
    | Some errors -> failwithf "Type checking results in errors: %A" errors
    signature

let generateDefinitionFromPosNoValidation caretPos src = 
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
    |> List.map (fun (src, pos) -> generateDefinitionFromPosNoValidation pos src)
    |> List.iter (fun src ->
        assertSrcAreEqual src
            """namespace System

open System

/// Represents a 2-tuple, or pair. 
type Tuple<'T1, 'T2> =
    interface ITuple
    /// Initializes a new instance of the  class.
    new : item1:'T1 * item2:'T2 -> Tuple<'T1, 'T2>
    /// Returns a value that indicates whether the current  object is equal to a specified object.
    member Equals : obj:obj -> bool
    /// Returns the hash code for the current  object.
    member GetHashCode : unit -> int
    /// Gets the value of the current  object's first component.
    member Item1 : 'T1
    /// Gets the value of the current  object's second component.
    member Item2 : 'T2
    /// Returns a string that represents the value of this  instance.
    member ToString : unit -> string
""")

[<Test>]
let ``adds necessary parenthesis to function parameters`` () =
    """open System
let _ = Async.AwaitTask"""
    |> generateDefinitionFromPos (Pos.fromZ 1 8)
    |> assertSrcAreEqualForFirstLines 10 """namespace Microsoft.FSharp.Control

open System

/// This static class holds members for creating and manipulating asynchronous computations.
[<Sealed>]
[<CompiledName("FSharpAsync")>]
[<Class>]
type Async =
    /// Creates three functions that can be used to implement the .NET Asynchronous 
    /// Programming Model (APM) for a given asynchronous computation.
    static member AsBeginEnd : computation:('Arg -> Async<'T>) -> ('Arg * AsyncCallback * obj -> IAsyncResult) * (IAsyncResult -> 'T) * (IAsyncResult -> unit)
"""

[<Test>]
let ``adds necessary parenthesis to tuple parameters`` () =
    """
type T() =
    member this.Test(x: int * int, y: int): int = 3
    
    let x = new T()"""
    |> generateDefinitionFromPos (Pos.fromZ 4 16)
    |> assertSrcAreEqual """module File

type T =
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
    |> assertSrcAreEqual """module File

[<AbstractClass>]
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
    |> generateDefinitionFromPosNoValidation (Pos.fromZ 3 10)
    |> assertSrcAreEqual """namespace System

open System

/// Represents a 2-tuple, or pair. 
type Tuple<'T1, 'T2> =
    interface ITuple
    /// Initializes a new instance of the  class.
    new : item1:'T1 * item2:'T2 -> Tuple<'T1, 'T2>
    /// Returns a value that indicates whether the current  object is equal to a specified object.
    member Equals : obj:obj -> bool
    /// Returns the hash code for the current  object.
    member GetHashCode : unit -> int
    /// Gets the value of the current  object's first component.
    member Item1 : 'T1
    /// Gets the value of the current  object's second component.
    member Item2 : 'T2
    /// Returns a string that represents the value of this  instance.
    member ToString : unit -> string
"""

[<Test>]
let ``go to method definition generate enclosing type metadata and supports C# events`` () =
    """open System

do Console.WriteLine("xxx")"""
    |> generateDefinitionFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqualForFirstLines 11 """namespace System

open System

/// Represents the standard input, output, and error streams for console applications. This class cannot be inherited.
[<Class>]
type Console =
    [<Security.SecuritySafeCritical>]
    static member add_CancelKeyPress : value:ConsoleCancelEventHandler -> unit
    /// Gets or sets the background color of the console.
    static member BackgroundColor : ConsoleColor with get, set
    /// Plays the sound of a beep through the console speaker.
    static member Beep : unit -> unit
"""

[<Test>]
let ``go to type definition that contains C# events`` () =
    """open System.ComponentModel

let f (x: INotifyPropertyChanged) = failwith "" """
    |> generateDefinitionFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqual """namespace System.ComponentModel

open System.ComponentModel

/// Notifies clients that a property value has changed.
[<Interface>]
type INotifyPropertyChanged =
    abstract member add_PropertyChanged : value:PropertyChangedEventHandler -> unit
    abstract member remove_PropertyChanged : value:PropertyChangedEventHandler -> unit
"""

[<Test>]
let ``go to F# List<'T> definition`` () =
    """open System

let x: List<int> = []"""
    |> generateDefinitionFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Collections

/// The type of immutable singly-linked lists.
[<DefaultAugmentation(false)>]
[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpList`1")>]
type List<'T> =
    | ( [] )
    | ( :: ) of Head: 'T * Tail: 'T list
    interface System.Collections.IEnumerable
    interface System.Collections.Generic.IEnumerable<'T>
    /// Gets the first element of the list
    member Head : 'T
    /// Gets a value indicating if the list contains no entries
    member IsEmpty : bool
    /// Gets the element of the list at the given position.
    member Item : 'T
    /// Gets the number of items contained in the list
    member Length : int
    /// Gets the tail of the list, which is a list containing all the elements of the list, excluding the first element 
    member Tail : 'T list
    /// Returns a list with head as its first element and tail as its subsequent elements
    static member Cons : head:'T * tail:'T list -> 'T list
    /// Returns an empty list of a particular type
    static member Empty : 'T list
"""

[<Test>]
let ``go to interface definition`` () =
    """
type MyInterface =
    abstract Method: int -> unit

let f (x:MyInterface) = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 4 9)
    |> assertSrcAreEqual """module File

[<Interface>]
type MyInterface =
    abstract member Method : int -> unit
"""

[<Test>]
let ``go to union type definition`` () =
    """open System

let x: Choice<'T, 'U> = failwith "Not implemented yet" """
    |> generateDefinitionFromPos (Pos.fromZ 2 7)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Core

/// Helper types for active patterns with 2 choices.
[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpChoice`2")>]
type Choice<'T1, 'T2> =
    /// Choice 1 of 2 choices
    | Choice1Of2 of 'T1
    /// Choice 2 of 2 choices
    | Choice2Of2 of 'T2
"""

[<Test>]
let ``go to union case`` () =
    """open System

let x = Choice1Of2 () """
    |> generateDefinitionFromPos (Pos.fromZ 2 9)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Core

/// Helper types for active patterns with 2 choices.
[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpChoice`2")>]
type Choice<'T1, 'T2> =
    /// Choice 1 of 2 choices
    | Choice1Of2 of 'T1
    /// Choice 2 of 2 choices
    | Choice2Of2 of 'T2
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
    |> assertSrcAreEqual """val (|Even|Odd|) : int -> Choice<unit,unit>
"""

[<Test; Ignore("activate when it's possible to know in which module or namespace an active pattern is defined")>]
let ``go to total active patterns should display enclosing module or namespace`` () =
    """
let (|Even|Odd|) i = 
    if i % 2 = 0 then Even else Odd

let testNumber i =
    match i with
    | Even -> printfn "%d is even" i
    | Odd -> printfn "%d is odd" i"""
    |> generateDefinitionFromPos (Pos.fromZ 6 7)
    |> assertSrcAreEqual """module File

val (|Even|Odd|) : int -> Choice<unit,unit>
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
    |> assertSrcAreEqual """val (|DivisibleBy|_|) : int -> int -> unit option
"""

[<Test>]
let ``go to record type definition`` () =
    """
[<CustomComparison>]
[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string -> unit
    }
    interface System.ICloneable with
        member x.Clone(): obj = null

let r: MyRecord = Unchecked.defaultof<_>"""
    |> generateDefinitionFromPos (Pos.fromZ 11 7)
    |> assertSrcAreEqual """module File

[<CustomComparison>]
[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string -> unit
    }
    interface System.ICloneable
"""

let ``go to record field`` () =
    """
type MyRecord =
    {
        Field1: int
        Field2: string -> string
    }

let r = { Field1 = 0; Field2 = id }"""
    |> generateDefinitionFromPos (Pos.fromZ 7 11)
    |> assertSrcAreEqual """module File

type MyRecord =
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
    |> assertSrcAreEqual """module File

[<Struct>]
type MyStruct =
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
    |> assertSrcAreEqual """module File

type MyClass =
    class
    end
"""

[<Test>]
let ``go to empty interface metadata`` () =
    """
type MyInterface = interface end

let x: MyInterface = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 3 7)
    |> assertSrcAreEqual """module File

type MyInterface =
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
    |> assertSrcAreEqual """module File

type MyStruct =
    struct
    end
"""

[<Test>]
let ``go to constructor-less struct metadata`` () =
    """let x: System.Boolean = false"""
    |> generateDefinitionFromPos (Pos.fromZ 0 14)
    |> assertSrcAreEqualForFirstLines 11 """namespace System

/// Represents a Boolean value.
[<System.Runtime.InteropServices.ComVisible(true)>]
[<Struct>]
type Boolean =
    interface System.IConvertible
    /// Compares this instance to a specified object and returns an integer that indicates their relationship to one another.
    member CompareTo : obj:obj -> int
    /// Compares this instance to a specified  object and returns an integer that indicates their relationship to one another.
    member CompareTo : value:bool -> int
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
    |> assertSrcAreEqual """module File

type Enum =
    | A = 0
    | B = 1
    | C = 2
"""

[<Test>]
let ``go to metadata from module and module function`` () =
    let src = """

let f x = Option.map(x)"""

    [
        // From module: 'Option'
        Pos.fromZ 2 10
        // From module function: 'map'
        Pos.fromZ 2 17
    ]
    |> List.map (fun pos -> generateDefinitionFromPos pos src)
    |> List.iter (assertSrcAreEqual """/// Basic operations on options.
[<CompilationRepresentation(enum<CompilationRepresentationFlags> (4))>]
module Microsoft.FSharp.Core.Option

/// Returns true if the option is not None.
val isSome : option:'T option -> bool
/// Returns true if the option is None.
val isNone : option:'T option -> bool
/// Gets the value associated with the option.
val get : option:'T option -> 'T
/// count inp evaluates to match inp with None -> 0 | Some _ -> 1.
val count : option:'T option -> int
/// fold f s inp evaluates to match inp with None -> s | Some x -> f s x.
val fold : folder:('State -> 'T -> 'State) -> state:'State -> option:'T option -> 'State
/// fold f inp s evaluates to match inp with None -> s | Some x -> f x s.
val foldBack : folder:('T -> 'State -> 'State) -> option:'T option -> state:'State -> 'State
/// exists p inp evaluates to match inp with None -> false | Some x -> p x.
val exists : predicate:('T -> bool) -> option:'T option -> bool
/// forall p inp evaluates to match inp with None -> true | Some x -> p x.
val forall : predicate:('T -> bool) -> option:'T option -> bool
/// iter f inp executes match inp with None -> () | Some x -> f x.
val iter : action:('T -> unit) -> option:'T option -> unit
/// map f inp evaluates to match inp with None -> None | Some x -> Some (f x).
val map : mapping:('T -> 'U) -> option:'T option -> 'U option
/// bind f inp evaluates to match inp with None -> None | Some x -> f x
val bind : binder:('T -> 'U option) -> option:'T option -> 'U option
/// Convert the option to an array of length 0 or 1.
val toArray : option:'T option -> 'T []
/// Convert the option to a list of length 0 or 1.
val toList : option:'T option -> 'T list
""")
        
[<Test>]
let ``interface inheritance by interfaces`` () =
    """
type T =
    abstract member M: unit -> unit

type T2 =
    inherit T"""
    |> generateDefinitionFromPosNoValidation (Pos.fromZ 4 5)
    |> assertSrcAreEqual """module File

type T2 =
    interface
        inherit T
    end
"""

[<Test>]
let ``go to type abbreviation definition`` () =
    """
let x: string = null"""
    |> generateDefinitionFromPos (Pos.fromZ 1 7)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Core

/// An abbreviation for the CLI type System.String.
type string = System.String
"""

[<Test>]
let ``go to abstract class definition with default members`` () =
    """
[<AbstractClass>]
type MyAbstractClass =
    abstract member Method: int -> unit
    default this.Method(x) = ()"""
    |> generateDefinitionFromPos (Pos.fromZ 2 5)
    |> assertSrcAreEqual """module File

[<AbstractClass>]
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
    |> assertSrcAreEqual """module File

type MyBaseClass =
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
    |> generateDefinitionFromPosNoValidation (Pos.fromZ 5 5)
    |> assertSrcAreEqual """module File

type MyClass =
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
    |> assertSrcAreEqual """module File

type Class =
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
        """module File

exception MyEmptyException
"""

        """module File

exception MyException of int * (int * string) * (int -> unit)
"""
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
    |> assertSrcAreEqual """module File

type T =
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
        """module File

type MyDelegate =
    delegate of unit * int -> unit
"""

        """module File

type MyDelegate =
    delegate of arg1:int -> int
"""
    ]

[<Test>]
let ``handle nested modules`` () =
    """
let x = Array.map
    """
    |> generateDefinitionFromPos (Pos.fromZ 1 9)
    |> assertSrcAreEqual """/// Basic operations on arrays.
[<CompilationRepresentation(enum<CompilationRepresentationFlags> (4))>]
[<RequireQualifiedAccess>]
module Microsoft.FSharp.Collections.Array

/// Builds a new array that contains the elements of the first array followed by the elements of the second array.
val append : array1:'T [] -> array2:'T [] -> 'T []
/// Returns the average of the elements in the array.
val inline average : array: ^T [] ->  ^T when ^T : (static member ( + ) :  ^T *  ^T ->  ^T) and ^T : (static member DivideByInt :  ^T * int ->  ^T) and ^T : (static member Zero :  ^T)
/// Returns the average of the elements generated by applying the function to each element of the array.
val inline averageBy : projection:('T ->  ^U) -> array:'T [] ->  ^U when ^U : (static member ( + ) :  ^U *  ^U ->  ^U) and ^U : (static member DivideByInt :  ^U * int ->  ^U) and ^U : (static member Zero :  ^U)
/// Reads a range of elements from the first array and write them into the second.
val blit : source:'T [] -> sourceIndex:int -> target:'T [] -> targetIndex:int -> count:int -> unit
/// For each element of the array, applies the given function. Concatenates all the results and return the combined array.
val collect : mapping:('T -> 'U []) -> array:'T [] -> 'U []
/// Builds a new array that contains the elements of each of the given sequence of arrays.
val concat : arrays:seq<'T []> -> 'T []
/// Builds a new array that contains the elements of the given array.
val copy : array:'T [] -> 'T []
/// Creates an array whose elements are all initially the given value.
val create : count:int -> value:'T -> 'T []
/// Applies the given function to successive elements, returning the first
/// result where function returns Some(x) for some x. If the function 
/// never returns Some(x) then None is returned.
val tryPick : chooser:('T -> 'U option) -> array:'T [] -> 'U option
/// Fills a range of elements of the array with the given value.
val fill : target:'T [] -> targetIndex:int -> count:int -> value:'T -> unit
/// Applies the given function to successive elements, returning the first
/// result where function returns Some(x) for some x. If the function 
/// never returns Some(x) then KeyNotFoundException is raised.
val pick : chooser:('T -> 'U option) -> array:'T [] -> 'U
/// Applies the given function to each element of the array. Returns
/// the array comprised of the results "x" for each element where
/// the function returns Some(x)
val choose : chooser:('T -> 'U option) -> array:'T [] -> 'U []
/// Returns an empty array of the given type.
val empty : 'T []
/// Tests if any element of the array satisfies the given predicate.
val exists : predicate:('T -> bool) -> array:'T [] -> bool
/// Tests if any pair of corresponding elements of the arrays satisfies the given predicate.
val exists2 : predicate:('T1 -> 'T2 -> bool) -> array1:'T1 [] -> array2:'T2 [] -> bool
/// Returns a new collection containing only the elements of the collection
/// for which the given predicate returns "true".
val filter : predicate:('T -> bool) -> array:'T [] -> 'T []
/// Returns the first element for which the given function returns 'true'.
/// Raise KeyNotFoundException if no such element exists.
val find : predicate:('T -> bool) -> array:'T [] -> 'T
/// Returns the index of the first element in the array
/// that satisfies the given predicate. Raise KeyNotFoundException if 
/// none of the elements satisy the predicate.
val findIndex : predicate:('T -> bool) -> array:'T [] -> int
/// Tests if all elements of the array satisfy the given predicate.
val forall : predicate:('T -> bool) -> array:'T [] -> bool
/// Tests if all corresponding elements of the array satisfy the given predicate pairwise.
val forall2 : predicate:('T1 -> 'T2 -> bool) -> array1:'T1 [] -> array2:'T2 [] -> bool
/// Applies a function to each element of the collection, threading an accumulator argument
/// through the computation. If the input function is f and the elements are i0...iN then computes 
/// f (... (f s i0)...) iN
val fold : folder:('State -> 'T -> 'State) -> state:'State -> array:'T [] -> 'State
/// Applies a function to each element of the array, threading an accumulator argument
/// through the computation. If the input function is f and the elements are i0...iN then computes 
/// f i0 (...(f iN s))
val foldBack : folder:('T -> 'State -> 'State) -> array:'T [] -> state:'State -> 'State
/// Applies a function to pairs of elements drawn from the two collections, 
/// left-to-right, threading an accumulator argument
/// through the computation. The two input
/// arrays must have the same lengths, otherwise an ArgumentException is
/// raised.
val fold2 : folder:('State -> 'T1 -> 'T2 -> 'State) -> state:'State -> array1:'T1 [] -> array2:'T2 [] -> 'State
/// Apply a function to pairs of elements drawn from the two collections, right-to-left, 
/// threading an accumulator argument through the computation. The two input
/// arrays must have the same lengths, otherwise an ArgumentException is
/// raised.
val foldBack2 : folder:('T1 -> 'T2 -> 'State -> 'State) -> array1:'T1 [] -> array2:'T2 [] -> state:'State -> 'State
/// Gets an element from an array.
val get : array:'T [] -> index:int -> 'T
/// Creates an array given the dimension and a generator function to compute the elements.
val inline init : count:int -> initializer:(int -> 'T) -> 'T []
/// Creates an array where the entries are initially the default value Unchecked.defaultof<'T>.
val zeroCreate : count:int -> 'T []
/// Returns true if the given array is empty, otherwise false.
val isEmpty : array:'T [] -> bool
/// Applies the given function to each element of the array.
val inline iter : action:('T -> unit) -> array:'T [] -> unit
/// Applies the given function to pair of elements drawn from matching indices in two arrays. The
/// two arrays must have the same lengths, otherwise an ArgumentException is
/// raised.
val iter2 : action:('T1 -> 'T2 -> unit) -> array1:'T1 [] -> array2:'T2 [] -> unit
/// Applies the given function to each element of the array. The integer passed to the
/// function indicates the index of element.
val iteri : action:(int -> 'T -> unit) -> array:'T [] -> unit
/// Applies the given function to pair of elements drawn from matching indices in two arrays,
/// also passing the index of the elements. The two arrays must have the same lengths, 
/// otherwise an ArgumentException is raised.
val iteri2 : action:(int -> 'T1 -> 'T2 -> unit) -> array1:'T1 [] -> array2:'T2 [] -> unit
/// Returns the length of an array. You can also use property arr.Length.
val length : array:'T [] -> int
/// Builds a new array whose elements are the results of applying the given function
/// to each of the elements of the array.
val inline map : mapping:('T -> 'U) -> array:'T [] -> 'U []
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise. The two input
/// arrays must have the same lengths, otherwise an ArgumentException is
/// raised.
val map2 : mapping:('T1 -> 'T2 -> 'U) -> array1:'T1 [] -> array2:'T2 [] -> 'U []
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise, also passing the index of 
/// the elements. The two input arrays must have the same lengths, otherwise an ArgumentException is
/// raised.
val mapi2 : mapping:(int -> 'T1 -> 'T2 -> 'U) -> array1:'T1 [] -> array2:'T2 [] -> 'U []
/// Builds a new array whose elements are the results of applying the given function
/// to each of the elements of the array. The integer index passed to the
/// function indicates the index of element being transformed.
val mapi : mapping:(int -> 'T -> 'U) -> array:'T [] -> 'U []
/// Returns the greatest of all elements of the array, compared via Operators.max on the function result.
val inline max : array:'T [] -> 'T when 'T : comparison
/// Returns the greatest of all elements of the array, compared via Operators.max on the function result.
val inline maxBy : projection:('T -> 'U) -> array:'T [] -> 'T when 'U : comparison
/// Returns the lowest of all elements of the array, compared via Operators.min.
val inline min : array:'T [] -> 'T when 'T : comparison
/// Returns the lowest of all elements of the array, compared via Operators.min on the function result.
val inline minBy : projection:('T -> 'U) -> array:'T [] -> 'T when 'U : comparison
/// Builds an array from the given list.
val ofList : list:'T list -> 'T []
/// Builds a new array from the given enumerable object.
val ofSeq : source:seq<'T> -> 'T []
/// Splits the collection into two collections, containing the 
/// elements for which the given predicate returns "true" and "false"
/// respectively.
val partition : predicate:('T -> bool) -> array:'T [] -> 'T [] * 'T []
/// Returns an array with all elements permuted according to the
/// specified permutation.
val permute : indexMap:(int -> int) -> array:'T [] -> 'T []
/// Applies a function to each element of the array, threading an accumulator argument
/// through the computation. If the input function is f and the elements are i0...iN 
/// then computes f (... (f i0 i1)...) iN.
/// Raises ArgumentException if the array has size zero.
val reduce : reduction:('T -> 'T -> 'T) -> array:'T [] -> 'T
/// Applies a function to each element of the array, threading an accumulator argument
/// through the computation. If the input function is f and the elements are i0...iN 
/// then computes f i0 (...(f iN-1 iN)).
/// Raises ArgumentException if the array has size zero.
val reduceBack : reduction:('T -> 'T -> 'T) -> array:'T [] -> 'T
/// Returns a new array with the elements in reverse order.
val rev : array:'T [] -> 'T []
/// Like fold, but return the intermediary and final results.
val scan : folder:('State -> 'T -> 'State) -> state:'State -> array:'T [] -> 'State []
/// Like foldBack, but return both the intermediary and final results.
val scanBack : folder:('T -> 'State -> 'State) -> array:'T [] -> state:'State -> 'State []
/// Sets an element of an array.
val set : array:'T [] -> index:int -> value:'T -> unit
/// Builds a new array that contains the given subrange specified by
/// starting index and length.
val sub : array:'T [] -> startIndex:int -> count:int -> 'T []
/// Sorts the elements of an array, returning a new array. Elements are compared using Operators.compare. 
val sort : array:'T [] -> 'T [] when 'T : comparison
/// Sorts the elements of an array, using the given projection for the keys and returning a new array. 
/// Elements are compared using Operators.compare.
val sortBy : projection:('T -> 'Key) -> array:'T [] -> 'T [] when 'Key : comparison
/// Sorts the elements of an array, using the given comparison function as the order, returning a new array.
val sortWith : comparer:('T -> 'T -> int) -> array:'T [] -> 'T []
/// Sorts the elements of an array by mutating the array in-place, using the given projection for the keys. 
/// Elements are compared using Operators.compare.
val sortInPlaceBy : projection:('T -> 'Key) -> array:'T [] -> unit when 'Key : comparison
/// Sorts the elements of an array by mutating the array in-place, using the given comparison function as the order.
val sortInPlaceWith : comparer:('T -> 'T -> int) -> array:'T [] -> unit
/// Sorts the elements of an array by mutating the array in-place, using the given comparison function. 
/// Elements are compared using Operators.compare.
val sortInPlace : array:'T [] -> unit when 'T : comparison
/// Returns the sum of the elements in the array.
val inline sum : array: ^T [] ->  ^T when ^T : (static member ( + ) :  ^T *  ^T ->  ^T) and ^T : (static member Zero :  ^T)
/// Returns the sum of the results generated by applying the function to each element of the array.
val inline sumBy : projection:('T ->  ^U) -> array:'T [] ->  ^U when ^U : (static member ( + ) :  ^U *  ^U ->  ^U) and ^U : (static member Zero :  ^U)
/// Builds a list from the given array.
val toList : array:'T [] -> 'T list
/// Views the given array as a sequence.
val toSeq : array:'T [] -> seq<'T>
/// Returns the first element for which the given function returns true.
/// Return None if no such element exists.
val tryFind : predicate:('T -> bool) -> array:'T [] -> 'T option
/// Returns the index of the first element in the array
/// that satisfies the given predicate.
val tryFindIndex : predicate:('T -> bool) -> array:'T [] -> int option
/// Splits an array of pairs into two arrays.
val unzip : array:('T1 * 'T2) [] -> 'T1 [] * 'T2 []
/// Splits an array of triples into three arrays.
val unzip3 : array:('T1 * 'T2 * 'T3) [] -> 'T1 [] * 'T2 [] * 'T3 []
/// Combines the two arrays into an array of pairs. The two arrays must have equal lengths, otherwise an ArgumentException is
/// raised.
val zip : array1:'T1 [] -> array2:'T2 [] -> ('T1 * 'T2) []
/// Combines three arrays into an array of pairs. The three arrays must have equal lengths, otherwise an ArgumentException is
/// raised.
val zip3 : array1:'T1 [] -> array2:'T2 [] -> array3:'T3 [] -> ('T1 * 'T2 * 'T3) []

/// Provides parallel operations on arrays 
module Parallel = 

    /// Apply the given function to each element of the array. Return
    /// the array comprised of the results "x" for each element where
    /// the function returns Some(x).
    val choose : chooser:('T -> 'U option) -> array:'T [] -> 'U []
    /// For each element of the array, apply the given function. Concatenate all the results and return the combined array.
    val collect : mapping:('T -> 'U []) -> array:'T [] -> 'U []
    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array.
    val map : mapping:('T -> 'U) -> array:'T [] -> 'U []
    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array. The integer index passed to the
    /// function indicates the index of element being transformed.
    val mapi : mapping:(int -> 'T -> 'U) -> array:'T [] -> 'U []
    /// Apply the given function to each element of the array. 
    val iter : action:('T -> unit) -> array:'T [] -> unit
    /// Apply the given function to each element of the array. The integer passed to the
    /// function indicates the index of element.
    val iteri : action:(int -> 'T -> unit) -> array:'T [] -> unit
    /// Create an array given the dimension and a generator function to compute the elements.
    val init : count:int -> initializer:(int -> 'T) -> 'T []
    /// Split the collection into two collections, containing the 
    /// elements for which the given predicate returns "true" and "false"
    /// respectively 
    val partition : predicate:('T -> bool) -> array:'T [] -> 'T [] * 'T []

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
    |> assertSrcAreEqual """module File

type MyRecord =
    {
        A: string
        B: int
    }
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
    |> assertSrcAreEqual """module File

type MyUnion =
    | A of int
    | B of float
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
    |> assertSrcAreEqual """module File

type MyClass =
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
    |> assertSrcAreEqual """module File

type MyClass =
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
    |> assertSrcAreEqual """module File

type Union =
    | [<System.Diagnostics.Conditional ("MyConditional")>]
      [<System.Obsolete ("hello")>]
      Case1 of int
    | [<System.Obsolete ("cuir")>]
      Case2 of string
    | Case3
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
    |> generateDefinitionFromPos(Pos.fromZ 2 5)
    |> assertSrcAreEqual """module File

open System

type Record =
    {
        [<DefaultValue>]
        [<Obsolete ("Reason1")>]
        Field1: int
        [<Obsolete ("Reason2")>]
        Field2: float
    }
"""

[<Test>]
let ``handle property/method attributes``() =
    """
type MyClass() =
    [<System.Obsolete("Prop is obsolete")>]
    member __.Prop = 0
    [<System.Obsolete("Method is obsolete")>]
    member __.Method() = ()
"""
    |> generateDefinitionFromPos (Pos.fromZ 1 5)
    |> assertSrcAreEqual """module File

type MyClass =
    new : unit -> MyClass
    [<System.Obsolete ("Method is obsolete")>]
    member Method : unit -> unit
    [<System.Obsolete ("Prop is obsolete")>]
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
        """module File

type MyClass<'T, 'U when 'T : null and 'T : (new : unit -> 'T) and 'U : struct> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """module File

open System

type MyClass<'T, 'U when 'T :> IComparable and 'U : not struct> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """module File

type MyClass<'T, 'U when 'T : comparison and 'U : equality> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """module File

type MyClass<'T, 'U when 'T : unmanaged and 'U : enum<uint32>> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
"""

        """module File

type MyClass<'T when 'T : delegate<obj * int, unit>> =
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
    |> assertSrcAreEqual """module File

type MyClass<'T when 'T : struct> =
    new : unit -> MyClass<'T>
    member Method : x:'X -> int when 'X : null
    member NormalMethod : unit -> unit
    static member StaticMethod : x:('X * 'X) -> bool when 'X : equality
"""

[<Test>]
let ``handle generic constraints on module functions and values`` () =
    [
        """let func<'X when 'X : null and 'X : comparison>(x: 'X) = 0"""
        """let value<'X when 'X : null and 'X : comparison> = typeof<'X>.ToString()"""
    ]
    |> List.map (generateDefinitionFromPos (Pos.fromZ 0 4))
    |> assertSrcSeqAreEqual [
        """module File

val func : x:'X -> int when 'X : null and 'X : comparison
"""

        """module File

val value : string when 'X : null and 'X : comparison
"""
    ]

[<Test>]
let ``operator names are demangled`` () =
    """let inline func x = x + x"""
    |> generateDefinitionFromPos (Pos.fromZ 0 11)
    |> assertSrcAreEqual """module File

val inline func : x: ^a ->  ^b when ^a : (static member ( + ) :  ^a *  ^a ->  ^b)
"""

[<Test>]
let ``double-backtick identifiers are supported`` () =
    [
        """type ``My class``() =
    member __.``a property`` = 0
    member __.``a method``() = ()""", Pos.fromZ 0 5

        """module ``My module``

let ``a value`` = 0
let f (``a param``: int) = ``a param`` * 2""", Pos.fromZ 0 9

        """type ``My abbrev`` = int""", Pos.fromZ 0 5

        """type Union = ``My Case`` of int | Case2""", Pos.fromZ 0 5

        """type Record = { ``My Record`` : string }""", Pos.fromZ 0 5

        """exception ``My exception``""", Pos.fromZ 0 12

        """exception ``My exception 2`` of int""", Pos.fromZ 0 12

        """type ``My delegate``= delegate of int -> int""", Pos.fromZ 0 5
    ]
    |> List.map (fun (src, pos) -> generateDefinitionFromPosNoValidation pos src)
    |> assertSrcSeqAreEqual [
        """module File

type ``My class`` =
    new : unit -> ``My class``
    member ``a method`` : unit -> unit
    member ``a property`` : int
"""

        """module ``My module``

val ``a value`` : int
val f : ``a param``:int -> int
"""

        """module File

type ``My abbrev`` = int
"""

        """module File

type Union =
    | ``My Case`` of int
    | Case2
"""

        """module File

type Record =
    {
        ``My Record``: string
    }
"""

        """module File

exception ``My exception``
"""

        """module File

exception ``My exception 2`` of int
"""

        """module File

type ``My delegate`` =
    delegate of int -> int
"""
    ]

[<Test>]
let ``handle statically resolved constraints`` () =
    [
        """
type MyClass<'T when 'T : (static member Create : unit -> 'T) and 'T : (member Prop : int)> =
    class end
"""

        """
type MyClass<'T when 'T : (member Create : int * 'T -> 'T)> =
    class end
"""

        """
type MyClass<'T when 'T : (static member MyProp : int)> =
    class end
"""

        """
type MyClass() =
    member inline __.Method< ^T when ^T : (member ConstraintMethod : unit -> unit)>(t : ^T) = ()
    static member inline StaticMethod< ^T when ^T : (static member Create : unit -> ^T)>() =
        (^T : (static member Create : unit -> ^T) ())
"""
    ] 
    |> List.map (generateDefinitionFromPos (Pos.fromZ 1 5))
    |> assertSrcSeqAreEqual [
        """module File

type MyClass< ^T when ^T : (static member Create : unit ->  ^T) and ^T : (member Prop : int)> =
    class
    end
"""

        """module File

type MyClass< ^T when ^T : (member Create : int *  ^T ->  ^T)> =
    class
    end
"""

        """module File

type MyClass< ^T when ^T : (static member MyProp : int)> =
    class
    end
"""

        """module File

type MyClass =
    new : unit -> MyClass
    member inline Method : t: ^T -> unit when ^T : (member ConstraintMethod : unit -> unit)
    static member inline StaticMethod : unit ->  ^T when ^T : (static member Create : unit ->  ^T)
"""
    ]

[<Test>]
let ``handle double-backtick identifiers on member constraints`` () =
    [
        """
type MyClass<'T when 'T : (static member ``A static member`` : unit -> 'T)> =
    class end
"""

        """
type MyClass<'T when 'T : (member ``A property`` : int)> =
    class end
"""
    ] 
    |> List.map (generateDefinitionFromPos (Pos.fromZ 1 5))
    |> assertSrcSeqAreEqual [
        """module File

type MyClass< ^T when ^T : (static member ``A static member`` : unit ->  ^T)> =
    class
    end
"""

        """module File

type MyClass< ^T when ^T : (member ``A property`` : int)> =
    class
    end
"""
    ]

[<Test>]
let ``type abbreviations with generic params`` () =
    """let x: option<'T> = failwith "" """
    |> generateDefinitionFromPos (Pos.fromZ 0 11)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Core

/// The type of optional values. When used from other CLI languages the
/// empty option is the null value. 
type option<'T> = Option<'T>
"""

[<Test>]
let ``type abbreviations for basic types`` () =
    """let x: ResizeArray<'T> = failwith "" """
    |> generateDefinitionFromPos (Pos.fromZ 0 8)
    |> assertSrcAreEqual """namespace Microsoft.FSharp.Collections

/// An abbreviation for the CLI type System.Collections.Generic.List<_>
type ResizeArray<'T> = System.Collections.Generic.List<'T>
"""

[<Test>]
let ``handle operators as compiled members`` () =
    """let x: System.DateTime = failwith "" """
    |> generateDefinitionFromPosNoValidation (Pos.fromZ 0 20)
    |> fun str -> str.Contains("static member op_GreaterThanOrEqual : t1:System.DateTime * t2:System.DateTime -> bool")
    |> assertEqual true

[<Test>]
let ``handle uninstantiated type parameters`` () =
    """let f x = array2D x"""
    |> generateDefinitionFromPos (Pos.fromZ 0 12)
    // Resulting types from FCS are non-deterministic so we are not yet able to assert a specific answer.
    |> fun str -> str.Contains("val array2D : rows:seq") && not <| str.Contains("'?")
    |> assertEqual true

let ``handle generic definitions`` () =
    """open System
let x: Collections.Generic.List<'T> = failwith "" """
    |> generateDefinitionFromPos (Pos.fromZ 1 30)
    |> fun str -> str.Contains("member GetEnumerator : unit -> System.Collections.Generic.List<'T>.Enumerator")
    |> assertEqual true

[<Test>]
let ``handle generic definitions 2`` () =
    """open System
let x: Collections.Generic.Dictionary<'K, 'V> = failwith "" """
    |> generateDefinitionFromPosNoValidation (Pos.fromZ 1 30)
    |> fun str -> str.Contains("member Values : System.Collections.Generic.Dictionary<'TKey,'TValue>.ValueCollection")
    |> assertEqual true

[<Test>]
let ``handle active patterns as parts of module declarations`` () =
    """open Microsoft.FSharp.Quotations
let f = Patterns.(|AddressOf|_|)"""
    |> generateDefinitionFromPosNoValidation (Pos.fromZ 1 13)
    |> fun str -> str.Contains("val (|AddressSet|_|) : input:Expr -> (Expr * Expr) option")
    |> assertEqual true

let generateFileNameForSymbol caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, LanguageServiceTestHelper.args)
    let projectOptions = project()

    asyncMaybe {
        let! _range, symbolAtPos =
            liftMaybe <| codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)
        
        return getFileNameFromSymbol symbolUse.Symbol
    }
    |> Async.RunSynchronously
    |> Option.get

[<Test>]
let ``file names for union cases should refer to union type names`` () =
    """let x = Some 0 "" """
    |> generateFileNameForSymbol (Pos.fromZ 0 8)
    |> assertSrcAreEqual "Microsoft.FSharp.Core.option.fsi"

[<Test>]
let ``file names for record fields should refer to record type names`` () =
     """
type MyRecord =
    {
        Field1: int
        Field2: string -> string
    }

let r = { Field1 = 0; Field2 = id }"""
    |> generateFileNameForSymbol (Pos.fromZ 7 11)
    |> assertSrcAreEqual "File.MyRecord.fsi"

[<Test>]
let ``file names for members should refer to type names`` () =
    """open System
let _ = Async.AwaitTask"""
    |> generateFileNameForSymbol (Pos.fromZ 1 16)
    |> assertSrcAreEqual "Microsoft.FSharp.Control.FSharpAsync.fsi"

// Tests to add:
// TODO: buffer should have the same behavior as C#'s generated metadata ([from metadata] instead of [read-only] header, preview buffer and not permanent buffer)
// TODO: set cursor on method/... when symbol is a method/union case/enum value/record field
