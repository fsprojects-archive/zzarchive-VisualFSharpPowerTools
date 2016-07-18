#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/CodeGeneration.fs"
      "../../src/FSharpVSPowerTools.Core/UnopenedNamespacesResolver.fs"
      "TestHelpers.fs"
      "CodeGenerationTestInfrastructure.fs"
#else
module FSharp.Editing.Tests.UnopenedNamespacesResolverTests
#endif

open NUnit.Framework
open FSharp.Editing
open FSharp.Editing.Features

// Entity.TryCreate tests

type EntiryCreationArgs = 
    { ns: string option
      scope: string 
      currentIdent: string 
      requireQualifiedAccessParent: string option 
      autoOpenParent: string option
      entityNs: string option 
      entityFullName: string }

let assertEntityCreation args res = 
    Entity.tryCreate 
            (args.ns |> Option.map (fun x -> x.Split '.'),
             args.scope.Split '.', 
             args.currentIdent.Split '.',
             args.requireQualifiedAccessParent |> Option.map (fun x -> x.Split '.'), 
             args.autoOpenParent |> Option.map (fun x -> x.Split '.'),
             args.entityNs |> Option.map (fun x -> x.Split '.'),
             args.entityFullName.Split '.')
    |> assertEqual res

let (=>) args res = assertEntityCreation args [|res|]

let (!=>) = assertEntityCreation

[<Test>] 
let ``fully qualified external entities``() =
    { 
        ns = Some "TopNs"
        scope = ""
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = Some "System"
        entityFullName = "System.DateTime.Now" 
    } 
    => 
    { 
        FullRelativeName = "System.DateTime.Now"
        Qualifier = "System.DateTime.Now"
        Namespace = Some "System.DateTime"
        Name = "" 
    }
    // -----------------------------------------
    { 
        ns = Some "TopNs"
        scope = ""
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = None
        entityFullName = "System.Now" 
    } 
    => 
    { 
        FullRelativeName = "System.Now"
        Qualifier = "System.Now"
        Namespace = Some "System"
        Name = "" 
    }
    // -----------------------------------------
    { 
        ns = Some "Myns"
        scope = "Myns"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = None
        entityFullName = "System.Now" 
    } 
    => 
    { 
        FullRelativeName = "System.Now"
        Qualifier = "System.Now"
        Namespace = Some "System"
        Name = "" 
    }
    // -----------------------------------------
    { 
        ns = Some "Myns"
        scope = "Myns.Nested"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = None
        entityFullName = "System.Now" 
    } 
    => 
    { 
        FullRelativeName = "System.Now"
        Qualifier = "System.Now"
        Namespace = Some "System"
        Name = "" 
    }

[<Test>] 
let ``fully qualified external entities with require qualified access module``() =
    { 
        ns = Some "TopNs"
        scope = ""
        currentIdent = "Now"
        requireQualifiedAccessParent = Some "System"
        autoOpenParent = None
        entityNs = Some "System"
        entityFullName = "System.DateTime.Now" 
    } 
    => 
    { 
        FullRelativeName = "System.DateTime.Now"
        Qualifier = "System.DateTime.Now"
        Namespace = None
        Name = "System.DateTime.Now" 
    }
    // -----------------------------------------
    { 
        ns = Some "TopNs"
        scope = ""
        currentIdent = "Now"
        requireQualifiedAccessParent = Some "System.DateTime"
        autoOpenParent = None
        entityNs = Some "System"
        entityFullName = "System.DateTime.Now" 
    } 
    => 
    { 
        FullRelativeName = "System.DateTime.Now"
        Qualifier = "System.DateTime.Now"
        Namespace = Some "System"
        Name = "DateTime.Now" 
    }

[<Test>]
let ``simple entities``() =
    { 
        ns = Some "TopNs"
        scope = ""
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = None
        entityFullName = "Now" 
    } 
    !=> [||]

[<Test>]
let ``internal entities``() =
    { 
        ns = Some "Myns"
        scope = "Myns"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = Some "Myns"
        entityFullName = "Myns.Nested.Now" 
    } 
    => 
    { 
        FullRelativeName = "Nested.Now"
        Qualifier = "Nested.Now"
        Namespace = Some "Nested"
        Name = "" 
    }
    // -----------------------------------------
    { 
        ns = Some "Myns"
        scope = "Myns.Nested"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = Some "Myns"
        entityFullName = "Myns.Nested.Nested2.Now" 
    } 
    => 
    { 
        FullRelativeName = "Nested2.Now"
        Qualifier = "Nested2.Now"
        Namespace = Some "Nested2"
        Name = "" 
    }
    // -----------------------------------------
    { 
        ns = Some "Myns"
        scope = "Myns.Nested"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = Some "Myns"
        entityFullName = "" 
    } 
    !=> [||]

[<Test>]
let ``internal entities in different sub namespace``() =
    { 
        ns = Some "Myns.Nested1"
        scope = "Myns.Nested1"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = Some "Myns.Nested2"
        entityFullName = "Myns.Nested2.Now" 
    } 
    => 
    { 
        FullRelativeName = "Myns.Nested2.Now"
        Qualifier = "Myns.Nested2.Now"
        Namespace = Some "Myns.Nested2"
        Name = "" 
    }

[<Test>] 
let ``internal entities with require qualified access module``() =
    { 
        ns = Some "Myns"
        scope = "Myns"
        currentIdent = "Now"
        requireQualifiedAccessParent = Some "Myns.Nested"
        autoOpenParent = None
        entityNs = Some "Myns"
        entityFullName = "Myns.Nested.Now" 
    } 
    => 
    { 
        FullRelativeName = "Nested.Now"
        Qualifier = "Nested.Now"
        Namespace = None
        Name = "Nested.Now"
    }

[<Test>]
let ``entities in auto open module``() =
    { 
        ns = Some "Myns"
        scope = "Myns"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = Some "Myns.Nested"
        entityNs = Some "Myns"
        entityFullName = "Myns.Nested.Now" 
    } 
    !=> [||]
    // -----------------------------------------
    { 
        ns = Some "Myns"
        scope = "Myns"
        currentIdent = "Now"
        requireQualifiedAccessParent = None
        autoOpenParent = Some "Myns.Nested.AutoOpenNested"
        entityNs = Some "Myns"
        entityFullName = "Myns.Nested.AutoOpenNested.Now" 
    } 
    => 
    { 
        FullRelativeName = "Nested.AutoOpenNested.Now"
        Qualifier = "Nested.AutoOpenNested.Now"
        Namespace = Some "Nested"
        Name = ""
    }

[<Test>] 
let ``fully qualified external entities / partially qualified name``() =
    { 
        ns = Some "TopNs"
        scope = ""
        currentIdent = "Threading.Tasks.Task.Factory.StartNew"
        requireQualifiedAccessParent = None
        autoOpenParent = None
        entityNs = Some "System.Threading.Tasks"
        entityFullName = "System.Threading.Tasks.Task" 
    } 
    => 
    { 
        FullRelativeName = "System.Threading.Tasks.Task"
        Qualifier = "System.Threading"
        Namespace = Some "System"
        Name = ""
    }

// ParsedInput.getEntityKind tests

open FSharp.Editing

let file = "/File.fs"
let languageService = LanguageService()

type Source = string

let parseSource (source: Source) =
    let parseResult = 
        languageService.ParseFileInProject(LanguageServiceTestHelper.projectOptions file, file, source) 
        |> Async.RunSynchronously
    if parseResult.ParseHadErrors then failwithf "Cannot parse input: %s, errors: %A" source parseResult.Errors
    match parseResult.ParseTree with
    | None -> failwithf "ParseTree is None for input: %s" source
    | Some tree -> tree

open Microsoft.FSharp.Compiler.Range
open CodeGenerationTestInfrastructure

type Line = int
type Col = int

let checkEntity source (points: (Line * Col * EntityKind option) list) =
    for line, col, kind in points do
        let input = parseSource source
        try
            Assert.That(ParsedInput.getEntityKind input (Pos.fromZ line col), Is.EqualTo kind, sprintf "Line = %d, Col = %d" line col)
        with _ ->
            printfn "Ast: %A" input
            reraise()

let (==>) = checkEntity

[<Test>]
let ``symbol at function position in binding is FuncOrConstructor``() =
    """
module TopLevel
module Nested =
    type range() = class end
module Nested1 =
    let range x = x
let _ = range()
""" 
    ==> [6, 9, Some (FunctionOrValue false) ]

[<Test>]
let ``return type annotation is a Type``() =
    """
module TopLevel
let x: DateTime = ()
type T() =
    let field: DateTime option = None
    member x.Prop: DateTime option = None
    member x.Method(): DateTime option = None
""" 
    ==> [2, 8, Some Type
         4, 17, Some Type
         5, 21, Some Type
         6, 25, Some Type]

[<Test>]
let ``type name in expression is a Type``() =
    """
module TopLevel
let x = DateTime.Now
let x = new Task()
type T() =
    let field = DateTime.Now
    member x.Prop = DateTime.Now
    member x.Method() = DateTime.Now
    static member StaticMethod (arg: int) =
        let a = 1
        { Field = new Task<_>() }
""" 
    ==> [2, 10, Some (FunctionOrValue false)
         3, 13, Some Type
         5, 18, Some (FunctionOrValue false)
         6, 22, Some (FunctionOrValue false)
         7, 26, Some (FunctionOrValue false)
         10, 23, Some Type]

[<Test>]
let ``type name in interface declaration is a Type``() =
    """
module TopLevel
type T() =
    abstract Prop: DateTime
    abstract Method: Task<_> -> DateTime
""" 
    ==> [3, 20, Some Type
         4, 22, Some Type
         4, 33, Some Type]

[<Test>]
let ``type name in attribute argument is a Type``() =
    """
module TopLevel
[<Attribute (Type.Literal)>]
let x = 1
""" 
    ==> [2, 15, Some Type]

[<Test>]
let ``argument type annotation is a Type``() =
    """
module TopLevel
let f (arg: DateTime) = ()
type T() =
    member x.Method (arg1: DateTime, arg2: TimeSpan) = ()
""" 
    ==> [2, 15, Some Type
         4, 29, Some Type
         4, 45, Some Type]

[<Test>]
let ``type annotation in statically resolved type parameters is a Type``() =
    """
module TopLevel
let inline func< ^a, 'b when ^a: (member Prop: IList<Task>)> x =
    (^a: (member Prop: IList<Task>) x)
"""
    ==> [2, 54, Some Type
         3, 30, Some Type]

[<Test>]
let ``type annotation in type constraint is a Type``() =
    """
module TopLevel
type T<'a when 'a :> Task>() = class end
"""
    ==> [2, 22, Some Type]

[<Test>]
let ``type name in type extension is a Type``() =
    """
module TopLevel
type DateTime with
    member x.Foo = ()
"""
    ==> [2, 6, Some Type]

[<Test>]
let ``attribute is an Attribute``() =
    """
[<Attribute>]
module TopLevel
[<Attribute>]
let f ([<Attribute>] arg: DateTime) = ()
[<Attribute>]
type T() =
    [<Attribute>]
    member x.Prop = ()
    [<Attribute>] static member StaticMember ([<Attribute>] arg) = ()
[<Attribute>]
type R = { [<Attribute>] F: int }
type I =
    [<Attribute>]
    abstract Method: unit -> DateTime
[<Attribute>]
module M = 
    let x = ()
""" 
    ==> [1, 4, None
         3, 4, Some Attribute
         4, 11, Some Attribute
         5, 4, Some Attribute
         7, 8, Some Attribute
         9, 8, Some Attribute
         9, 51, Some Attribute
         10, 4, Some Attribute
         11, 15, Some Attribute
         13, 7, Some Attribute
         15, 4, Some Attribute]

[<Test>]
let ``type in an object expression method body is a FuncOrConstructor``() =
    """
module TopLevel
let _ = { new IMy with 
            method x.Method x = DateTime.Now }
""" 
    ==> [3, 34, Some (FunctionOrValue false)]

[<Test>]
let ``type in a let binging inside CE is a FuncOrConstructor``() =
    """
module TopLevel
let _ = 
    async { 
        let _ = DateTime.Now
        return () 
    }
""" 
    ==> [4, 18, Some (FunctionOrValue false)]

[<Test>]
let ``type as a qualifier in a function application in argument position is FuncOrConstructor``() =
    """
module TopLevel
let _ = func (DateTime.Add 1)
let _ = func1 1 (2, DateTime.Add 1)
""" 
    ==> [2, 16, Some (FunctionOrValue false)
         3, 22, Some (FunctionOrValue false)]

[<Test>]
let ``constructor as argument is a FuncOrConstructor``() =
    """
module TopLevel
let _ = x.func (DateTime())
""" 
    ==> [2, 17, Some (FunctionOrValue false)]

[<Test>]
let ``type in match is a FunctionOrConstructor``() =
    """
module TopLevel
let _ = 
    match 1 with
    | Case1 -> DateTime.Now
""" 
    ==> [4, 17, Some (FunctionOrValue false)]

[<Test>]
let ``DU type in match is a Type``() =
    """
module TopLevel
let _ = 
    match 1 with
    | Du.Case1 -> ()
""" 
    ==> [4, 7, Some Type]

[<Test>]
let ``generic type is a Type``() =
    """
module TopLevel
let _ = Class<DateTime>()
type R = {
    Field: Task<_>
}
let _ = new Task<_>()
let _ = { Field = new Task<_>() }
""" 
    ==> [2, 9, Some (FunctionOrValue false)
         4, 12, Some Type
         6, 13, Some Type
         7, 23, Some Type]

[<Test>]
let ``generic type argument is a Type``() =
    """
module TopLevel
let _ = Class<DateTime>()
""" 
    ==> [2, 15, Some Type]

[<Test>]
let ``type name in object expression is a Type``() =
    """
module TopLevel
let _ = { new IMy with 
    member __.Member _ = () }
""" 
    ==> [2, 15, Some Type]

[<Test>]
let ``upcast type is a FunctionOrValue``() =
    """
module TopLevel
let x: IMy<_, _, _> = upcast My(arg)
type T() =
    let x: IMy<_, _, _> = upcast My(arg)
""" 
    ==> [2, 30, Some (FunctionOrValue false)
         4, 34, Some (FunctionOrValue false)]

[<Test>]
let ``open declaration is not an entity``() =
    """
module TopLevel
open System.Threading.Tasks
module M =
    open System
    let () = ()
""" 
    ==> [2, 5, None; 2, 13, None; 2, 24, None; 4, 11, None]
    
[<Test>]
let ``module value is not an entity``() =
    """
module TopLevel
let value = ()
""" 
    ==> [2, 6, None]

[<Test>]
let ``class member is not an entity``() =
    """
module TopLevel
type C() =
    member x.Member x = ()
    member x.Prop = ()
""" 
    ==> [3, 15, None; 4, 15, None]

[<Test>]
let ``module name is not an entity``() =
    """
module TopLevel
type Class() = class end
module Nested =
    type Record = { F: int }
""" 
    ==> [1, 9, None; 2, 7, Some EntityKind.Type; 3, 9, None; 4, 11, Some EntityKind.Type]

[<Test; Ignore "Cannot extract arg name from Named">]
let ``argument name is not an entity``() =
    """
module TopLevel
let func (arg: int) = ()
type Class() =
    let func (arg: int) = ()
    member x.Method (arg: int) = ()
""" 
    ==> [2, 12, None; 4, 18, None; 5, 25, None]

[<Test>]
let ``wildcard generic type argument is not an entity``() =
    """
module TopLevel
let _ = Class<_>()
""" 
    ==> [2, 15, None]

[<Test>]
let ``module top level do expression is FunctionOrValue``() =
    """
DateTime
""" 
    ==> [1, 1, Some (FunctionOrValue false)]

[<Test>]
let ``second and subsequent parts in long ident is not an entity``() =
    """
let _ = System.empty
let _ = System.Foo.empty
""" 
    ==> [1, 16, None
         2, 16, None
         2, 20, None ]

[<Test>]
let ``exception type in "with" block``() =
    """
module M = 
    exception Exn
module N =
    try raise (Exn())
    with
    | :? Exn -> ()
"""
    ==> [6, 10, Some Type ]

// open declaration insertion integration tests

let forLine (line: Line) (source: Source) = source, line
let forIdent ident (source, line) = ident, source, line

let forEntity (ns: LongIdent) (fullName: LongIdent) (ident: string, source: Source, line) =
    let ast = parseSource source
    match ParsedInput.tryFindInsertionContext line ast (ident.Split '.') (None, None, Some (ns.Split '.'), fullName.Split '.') with
    | [||] -> failwith "Cannot find nearest open statement block"
    | [|e, ctx|] -> source, e, ctx, ast
    | es -> failwithf "More than one entity: %A" es

let result (expected: Source) (source: Source, (entity: Entity), ctx: InsertContext, ast) = 
    let lines = srcToLineArray source

    let doc =
        { new IInsertContextDocument<string[]> with
              member __.GetLineStr (lines, line) = lines.[line]
              member __.Insert (lines, line, lineStr) =  
                Array.append (
                    Array.append lines.[0..line - 1] [| lineStr |]) 
                    lines.[line..] }

    let result = InsertContext.insertOpenDeclaration lines doc ctx entity.Namespace.Value

    try result |> Collection.assertEqual (srcToLineArray expected)
    with _ ->
        let withLineNumbers xs = 
            xs
            |> List.mapi (fun i x -> sprintf "%d: %s" i x)
            |> String.concat "\r\n"

        printfn 
            "Expected:\n%s\nActual:\n%s\nAST:\n%A" 
            (expected |> srcToLineArray |> Array.toList |> withLineNumbers) 
            (result |> Array.toList |> withLineNumbers)
            ast
        reraise()

[<Test>]
let ``external top level symbol, no other open declarations``() =
    """
module TopLevel

let _ = DateTime.Now
"""
    |> forLine 3
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

open System

let _ = DateTime.Now
"""

[<Test>]
let ``external top level symbol, another open declaration is present``() =
    """
module TopLevel

open Another

let _ = DateTime.Now
"""
    |> forLine 5
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

open Another
open System

let _ = DateTime.Now
"""

[<Test>]
let ``external top level symbol, other open declarations are present``() =
    """
module TopLevel

open Another
open OneMore

let _ = DateTime.Now
"""
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

open Another
open OneMore
open System

let _ = DateTime.Now
"""

[<Test>]
let ``external symbol in a nested module, no other open declarations``() =
    """
module TopLevel

module Nested =
    let _ = DateTime.Now
"""
    |> forLine 4
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

module Nested =
    open System

    let _ = DateTime.Now
"""

[<Test>]
let ``external symbol in a nested module, another open declaration is present``() =
    """
module TopLevel

module Nested =
    open Another
    let _ = DateTime.Now
"""
    |> forLine 5
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

module Nested =
    open Another
    open System

    let _ = DateTime.Now
"""

[<Test>]
let ``external symbol in a nested module, other open declarations are present``() =
    """
module TopLevel

module Nested =
    open Another
    open OneMore

    let _ = DateTime.Now
"""
    |> forLine 7
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

module Nested =
    open Another
    open OneMore
    open System

    let _ = DateTime.Now
"""

[<Test>]
let ``external symbol in a double nested module, no other open declarations are present``() =
    """
module TopLevel

module Nested =
    module DoubleNested =
        let _ = DateTime.Now
"""
    |> forLine 5
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

module Nested =
    module DoubleNested =
        open System

        let _ = DateTime.Now
"""

[<Test>]
let ``external symbol in a double nested module, other open declarations on all levels are present``() =
    """
module TopLevel

open Another

module Nested =
    open OneMore

    module DoubleNested =
        open OneMore1

        let _ = DateTime.Now
"""
    |> forLine 11
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
module TopLevel

open Another

module Nested =
    open OneMore

    module DoubleNested =
        open OneMore1
        open System

        let _ = DateTime.Now
"""

[<Test>]
let ``top level symbol declared in a nested module in the same file``() =
    """
module TopLevel

module Nested =
    type DateTime() = class end

let marker = ()
let _ = DateTime.Now
"""
    |> forLine 7
    |> forIdent "DateTime"
    |> forEntity "" "TopLevel.Nested.DateTime"
    |> result """
module TopLevel

module Nested =
    type DateTime() = class end

open Nested

let marker = ()
let _ = DateTime.Now
"""

[<Test>]
let ``top level symbol declared in a nested module in the same file, there is another module below``() =
    """
module TopLevel

module Nested =
    type DateTime() = class end

let _ = DateTime.Now

module Below =
    let x = ()
"""
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "" "TopLevel.Nested.DateTime"
    |> result """
module TopLevel

module Nested =
    type DateTime() = class end

open Nested

let _ = DateTime.Now

module Below =
    let x = ()
"""

[<Test>]
let ``symbol declared in another module in the same namespace``() =
    """
namespace TopNs

module Nested =
    type DateTime() = class end

module Another =
    let _ = DateTime.Now
"""
    |> forLine 7
    |> forIdent "DateTime"
    |> forEntity "TopNs" "TopNs.Nested.DateTime"
    |> result """
namespace TopNs

module Nested =
    type DateTime() = class end

open Nested

module Another =
    let _ = DateTime.Now
"""

[<Test>]
let ``symbol declared in another module in the same namespace declared as top level module``() =
    """
module TopNs.TopM

module Nested =
    type DateTime() = class end

module Another =
    let _ = DateTime.Now
"""
    |> forLine 7
    |> forIdent "DateTime"
    |> forEntity "TopNs" "TopNs.TopM.Nested.DateTime"
    |> result """
module TopNs.TopM

module Nested =
    type DateTime() = class end

open Nested

module Another =
    let _ = DateTime.Now
"""

[<Test>]
let ``symbol declared in a top level record in a namespace, no other open statements``() =
    """
namespace TopNs

type Record = 
    { F: DateTime }
"""
    |> forLine 4
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
namespace TopNs

open System

type Record = 
    { F: DateTime }
"""

[<Test>]
let ``symbol declared in a top level record in a namespace, there's another open statement``() =
    """
namespace TopNs

open Another

type Record = 
    { F: DateTime }
"""
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
namespace TopNs

open Another
open System

type Record = 
    { F: DateTime }
"""

[<Test>]
let ``respects existing open statements indentation``() =
    """
namespace TopNs

  open Another

  type Record = 
      { F: DateTime }
"""
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
namespace TopNs

  open Another
  open System

  type Record = 
      { F: DateTime }
"""

[<Test>]
let ``respects top level block indentation in case where are no other open statements``() =
    """
namespace TopNs

 type Record = 
   { F: DateTime }
"""
    |> forLine 4
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
namespace TopNs

 open System

 type Record = 
   { F: DateTime }
"""

[<Test>]
let ``respects block indentation in case where are no other open statements``() =
    """
namespace TopNs

module M =
 type Record = 
   { F: DateTime }
"""
    |> forLine 5
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
namespace TopNs

module M =
 open System

 type Record = 
   { F: DateTime }
"""

[<Test>]
let ``implicit module, no other open statements exist``() =
    """
type T = { F: DateTime }
"""
    |> forLine 2
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """open System

type T = { F: DateTime }
"""

[<Test>]
let ``nested module in implicit top level module``() =
    """
module M =
    let x = ()
let _ = DateTime.Now
"""
    |> forLine 4
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """open System

module M =
    let x = ()
let _ = DateTime.Now
"""

[<Test>]
let ``implicit module, no other open statements exist, references exist``() =
    """
#r "System.Runtime"
type T = { F: DateTime }
"""
    |> forLine 3
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
#r "System.Runtime"

open System

type T = { F: DateTime }
"""

[<Test>]
let ``implicit module with other open statements``() =
    """
open Another
type T = { F: DateTime }
"""
    |> forLine 2
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result """
open Another
open System

type T = { F: DateTime }
"""

[<Test>]
let ``open module consisting of type aliases``() =
    """
module TypeAlias =
    type MyInt = int
module Usage =
    let f (x:MyInt) = x
"""
    |> forLine 5
    |> forIdent "MyInt"
    |> forEntity "TypeAlias" "TypeAlias.MyInt"
    |> result """
module TypeAlias =
    type MyInt = int
module Usage =
    open TypeAlias

    let f (x:MyInt) = x
"""

[<Test>]
let ``partially qualified symbol``() =
    """
module TopLevel

let _ = Threading.Tasks.Task.Factory.StartNew(fun _ -> ())
"""
    |> forLine 3
    |> forIdent "Threading.Tasks.Task.Factory.StartNew"
    |> forEntity "System.Threading.Tasks" "System.Threading.Tasks.Task"
    |> result """
module TopLevel

open System

let _ = Threading.Tasks.Task.Factory.StartNew(fun _ -> ())
"""
