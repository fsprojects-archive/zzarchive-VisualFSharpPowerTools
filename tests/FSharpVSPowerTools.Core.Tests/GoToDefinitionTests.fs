#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
#load "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/SignatureGenerator.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharpVSPowerTools.Core.Tests.GoToDefinitionTests
#endif

open NUnit.Framework
open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.SignatureGenerator
open FSharpVSPowerTools.Core.Tests.CodeGenerationTestInfrastructure

let args = 
    [|
        "--noframework"; "--debug-"; "--optimize-"; "--tailcalls-"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll"
        @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"
    |]

let languageService = LanguageService(fun _ -> ())
let project() =
    let fileName = @"C:\file.fs"
    let projFileName = @"C:\Project.fsproj"
    let files = [| fileName |]
    { ProjectFileName = projFileName
      ProjectFileNames = files
      ProjectOptions = args
      ReferencedProjects = Array.empty
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.UtcNow
      UnresolvedReferences = None }

let tryGenerateDefinitionFromPos caretPos src =
    let document: IDocument = upcast MockDocument(src)
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationTestService(languageService, args)
    let projectOptions = project()

    asyncMaybe {
        let! _range, symbolAtPos =
            liftMaybe <| codeGenService.GetSymbolAtPosition(projectOptions, document, caretPos)
        let! _range, _symbol, symbolUse = 
            codeGenService.GetSymbolAndUseAtPositionOfKind(projectOptions, document, caretPos, symbolAtPos.Kind)

        let generatedCode = formatSymbol symbolUse.DisplayContext symbolUse.Symbol
        return generatedCode
    }
    |> Async.RunSynchronously

let generateDefinitionFromPos caretPos src =
    Option.get (tryGenerateDefinitionFromPos caretPos src)

//type Interface =
//    abstract member M: int -> unit
//
//type T() =
//    let mutable x = 0
//    member this.Property with get() = x and set value = x <- value

[<Test>]
let ``go to Tuple<'T1, 'T2> definition`` () =
    let _ = new Tuple<int, int>(1, 2)

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
    member ToString : unit -> string
    member Item1 : 'T1
    member Item2 : 'T2
""")

let _ =
    """open System
open System.Runtime.InteropServices
type X() =
    member this.Test([<OptionalArgument>] ?x: int) = ()

let x = X()"""
    |> generateDefinitionFromPos (Pos.fromZ 5 8)

[<Test>]
let ``adds necessary parenthesis to function parameters`` () =
    """open System
let _ = Async.AwaitTask"""
    |> generateDefinitionFromPos (Pos.fromZ 1 8)
    |> assertSrcAreEqualForFirstLines 6 """namespace Microsoft.FSharp.Control

[<Sealed>]
[<CompiledName("FSharpAsync")>]
[<Class>]
type Async =
    static member AsBeginEnd : computation:('Arg -> Async<'T>) -> ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) * (System.IAsyncResult -> 'T) * (System.IAsyncResult -> unit)
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
    member ToString : unit -> string
    member Item1 : 'T1
    member Item2 : 'T2
"""

[<Test>]
let ``go to method definition generate enclosing type metadata`` () =
    """open System

do Console.WriteLine("xxx")"""
    |> generateDefinitionFromPos (Pos.fromZ 2 11)
    |> assertSrcAreEqualForFirstLines 6 """namespace System

[<Class>]
type Console =
    static member Beep : unit -> unit
    static member Beep : frequency:int * duration:int -> unit
"""

[<Test>]
let ``go to Microsoft.FSharp.Collections.List<'T> definition`` () =
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
    static member Cons : head:'T * tail:'T list -> 'T list
    member Tail : 'T list
    member Length : int
    member Item : 'T
    member IsEmpty : bool
    member Head : 'T
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
let ``go to record type definition`` () =
    """open System
[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string
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
    interface ICloneable"""

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
module Microsoft.FSharp.Core.OptionModule = 
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

// Tests to add:
// TODO: property/method attributes
// TODO: method arguments attributes
// TODO: xml comments
// TODO: include open directives so that IStructuralEquatable/... are not wiggled
// TODO: sort members by display name
// TODO: class extension members?
// TODO: union type extension members?
// TODO: record type fields attributes
// TODO: record type extension members?
// TODO: enum type attributes
// TODO: handle abstract method with default implementation
// TODO: handle override methods
// TODO: handle inherited classes
// TODO: fix empty interfaces/classes/structs display (see Microsoft.FSharp.Compiler.Range module)
// TODO: fix nested module naming issue (see Microsoft.FSharp.Compiler.Range module)
//
//type MyInterface() =
//    abstract member Method: int -> unit
//    default this.Method(x) = ()
//
//type MyInterface2() =
//    inherit MyInterface() with
//        override this.Method(x) = ()
//
// TODO: display event handlers (see System.Console.CancelKeyPress)
// TODO: display static member getter/setter availability
// TODO: handle optional parameters (see Async.AwaitEvent)
// TODO: handle abbreviation (try string vs System.String...)
// TODO: special formatting for Events?
// TODO: syntax coloring is deactivated on generated metadata file
// TODO: buffer should have the same behavior as C#'s generated metadata ([from metadata] instead of [read-only] header, preview buffer and not permanent buffer)
// FIXME: buffer name should have the enclosing type name (if symbol = field, method, ...)
// TODO: set cursor on method when symbol is a method
// TODO: set cursor on union case when symbol is a union case
// TODO: set cursor on enum case when symbol is an enum case
// TODO: set cursor on field when symbol is a record field

#if INTERACTIVE
#time "on";;
let result =
    """open System

let x: Int32 = 0"""
    |> generateDefinitionFromPos (Pos.fromZ 2 7)
#endif
