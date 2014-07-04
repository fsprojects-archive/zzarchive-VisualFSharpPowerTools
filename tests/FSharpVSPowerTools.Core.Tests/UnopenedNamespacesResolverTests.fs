module FSharpVSPowerTools.Core.Tests.UnopenedNamespacesResolverTests

open NUnit.Framework
open FSharpVSPowerTools

let (=>) (ns: string option, 
          scope: string, 
          currentIdent, 
          requireQualifiedAccessParent: string option, 
          autoOpenParent: string option,
          entityNs: string option, 
          entityFullName: string) res = 

    Entity.tryCreate 
            (ns |> Option.map (fun x -> x.Split '.'))
            (scope.Split '.') 
            currentIdent 
            (requireQualifiedAccessParent |> Option.map (fun x -> x.Split '.')) 
            (autoOpenParent |> Option.map (fun x -> x.Split '.'))
            (entityNs |> Option.map (fun x -> x.Split '.'))
            (entityFullName.Split '.')
    |> assertEqual (res |> Option.map (fun (fullRelativeName, ns, name) -> 
        { FullRelativeName = fullRelativeName; Namespace = ns; Name = name }))

[<Test>] 
let ``fully qualified external entities``() =
    (Some "TopNs", "", "Now", None, None, Some "System", "System.DateTime.Now") => Some ("System.DateTime.Now", Some "System.DateTime", "Now")
    (Some "TopNs", "", "Now", None, None, None, "System.Now") => Some ("System.Now", Some "System", "Now")
    (Some "Myns", "Myns", "Now", None, None, None, "System.Now") => Some ("System.Now", Some "System", "Now")
    (Some "Myns", "Myns.Nested", "Now", None, None, None, "System.Now") => Some ("System.Now", Some "System", "Now")

[<Test>] 
let ``fully qualified external entities with require qualified access module``() =
    (Some "TopNs", "", "Now", Some "System", None, Some "System", "System.DateTime.Now") => 
        Some ("System.DateTime.Now",  None, "System.DateTime.Now")

    (Some "TopNs", "", "Now", Some "System.DateTime", None, Some "System",  "System.DateTime.Now") => 
        Some ("System.DateTime.Now", Some "System", "DateTime.Now")

[<Test>]
let ``simple entities``() =
    (Some "TopNs", "", "Now", None, None, None, "Now") => None
    (Some "Myns", "Myns", "Now", None, None, None, "Now") => None

[<Test>]
let ``internal entities``() =
    (Some "Myns", "Myns", "Now", None, None, Some "Myns", "Myns.Nested.Now") => Some ("Nested.Now", Some "Nested", "Now")   
    (Some "Myns", "Myns.Nested", "Now", None, None, Some "Myns", "Myns.Nested.Nested2.Now") => Some ("Nested2.Now", Some "Nested2", "Now")
    (Some "Myns", "Myns.Nested", "Now", None, None, Some "Myns", "") => None

[<Test>]
let ``internal entities in different sub namespace``() =
    (Some "Myns.Nested1", "Myns.Nested1", "Now", None, None, Some "Myns.Nested2", "Myns.Nested2.Now") => 
        Some ("Myns.Nested2.Now", Some "Myns.Nested2", "Now")   

[<Test>] 
let ``internal entities with require qualified access module``() =
    (Some "Myns", "Myns", "Now", Some "Myns.Nested", None, Some "Myns", "Myns.Nested.Now") => Some ("Nested.Now", None, "Nested.Now")   

[<Test>]
let ``entities in auto open module``() =
    (Some "Myns", "Myns", "Now", None, Some "Myns.Nested", Some "Myns", "Myns.Nested.Now") => None
    (Some "Myns", "Myns", "Now", None, Some "Myns.Nested.AutoOpenNested", Some "Myns", "Myns.Nested.AutoOpenNested.Now") => 
        Some ("Nested.AutoOpenNested.Now", Some "Nested", "Now")


open FSharpVSPowerTools.Core.Tests.CodeGenerationTestInfrastructure 

let file = "C:\\file.fs"
let languageService = LanguageService(fun _ -> ())

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
open Ast

type Line = int
type Col = int

let checkEntity source (points: (Line * Col * EntityKind option) list) =
    for line, col, kind in points do
        let tree = parseSource source
        try
            Assert.That(Ast.getEntityKind tree (Pos.fromZ line col), Is.EqualTo kind, sprintf "Line = %d, Col = %d" line col)
        with _ ->
            printfn "Ast: %A" tree
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
    ==> [6, 9, Some FunctionOrValue ]

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
    ==> [2, 10, Some FunctionOrValue
         3, 13, Some Type
         5, 18, Some FunctionOrValue
         6, 22, Some FunctionOrValue
         7, 26, Some FunctionOrValue
         10, 23, Some Type]

[<Test>]
let ``type name in interface declaration``() =
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
let ``type name in type extention is a Type``() =
    """
module TopLevel
type DateTime with
    member x.Foo = ()
"""
    ==> [2, 6, Some Type]

[<Test>]
let ``attribute is an Attribute``() =
    """
module TopLevel
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
""" 
    ==> [2, 11, Some Attribute
         3, 4, Some Attribute
         5, 8, Some Attribute
         7, 8, Some Attribute
         7, 51, Some Attribute
         8, 4, Some Attribute
         9, 15, Some Attribute
         11, 7, Some Attribute]

[<Test>]
let ``type in an object expression method body is a FuncOrConstructor``() =
    """
module TopLevel
let _ = { new IMy with 
            method x.Method x = DateTime.Now }
""" 
    ==> [3, 34, Some FunctionOrValue]

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
    ==> [4, 18, Some FunctionOrValue]

[<Test>]
let ``type as a qualifier in a function application in argument position is FuncOrConstructor``() =
    """
module TopLevel
let _ = func (DateTime.Add 1)
let _ = func1 1 (2, DateTime.Add 1)
""" 
    ==> [2, 16, Some FunctionOrValue
         3, 22, Some FunctionOrValue]

[<Test>]
let ``constructor as argument is a FuncOrConstructor``() =
    """
module TopLevel
let _ = x.func (DateTime())
""" 
    ==> [2, 17, Some FunctionOrValue]

[<Test>]
let ``type in match is a FunctionOrConstructor``() =
    """
module TopLevel
let _ = 
    match 1 with
    | Case1 -> DateTime.Now
""" 
    ==> [4, 17, Some FunctionOrValue]

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
    ==> [2, 9, Some FunctionOrValue
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
let ``upcast type is a FunctionOrValue``() =
    """
module TopLevel
let x: IMy<_, _, _> = upcast My(arg)
type T() =
    let x: IMy<_, _, _> = upcast My(arg)
""" 
    ==> [2, 30, Some FunctionOrValue
         4, 34, Some FunctionOrValue]

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

//[<Test>]
//let ``for interactive only``() =
//    """
//module M
//
//
//
//
//
//let main () = 
//    let _ = DateTime.Now
//    0
//""" 
//    ==> [0, 15, Some Type]

let forLine (line: Line) (source: Source) = source, line
let forIdent ident (source, line) = ident, source, line

let forEntity (ns: LongIdent) (entity: LongIdent) (ident, source: Source, line) =
    let tree = parseSource source
    match Ast.tryFindNearestOpenStatementBlock line tree ident (None, None, Some (ns.Split '.'), entity.Split '.') with
    | None -> failwith "Cannot find nearest open statement block"
    | Some (e, ctx) -> source, e, ctx.ScopeKind, ctx.Pos

let result (expectedScopeKind: ScopeKind) (expected: Source) (source: Source, entity, scopeKind: ScopeKind, pos) = 
    Assert.AreEqual (expectedScopeKind, scopeKind, "Scope Kind")
    let lines = srcToLineArray source
    let line = pos.Line - 1
    if lines.Length < line + 1 then 
        failwithf "Pos.Line = %d is out of bound (source contain %d lines)" pos.Line lines.Length
    let result = 
        Array.append (
            Array.append 
                lines.[0..line - 1] 
                [| (String.replicate pos.Col " ") + "open " + entity.Namespace.Value|]) 
            lines.[line..]
    try result |> Collection.assertEqual (srcToLineArray expected)
    with _ ->
        let withLineNumbers xs = 
            xs
            |> List.mapi (fun i x -> sprintf "%d: %s" i x)
            |> String.concat "\r\n"

        printfn 
            "Expected:\n%s\nActual:\n%s" 
            (expected |> srcToLineArray |> Array.toList |> withLineNumbers) 
            (result |> Array.toList |> withLineNumbers)
        reraise()

open FSharpVSPowerTools.Ast

[<Test>]
let ``external top level symbol, no other open declarations``() =
    """
module TopLevel

let _ = DateTime.Now
"""
    |> forLine 3
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result TopModule """
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
    |> result OpenDeclaration """
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
    |> result OpenDeclaration """
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
    |> result NestedModule """
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
    |> result OpenDeclaration """
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
    |> result OpenDeclaration """
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
    |> result NestedModule """
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
    |> result OpenDeclaration """
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
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "" "TopLevel.Nested.DateTime"
    |> result NestedModule """
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
    |> result NestedModule """
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
    |> result NestedModule """
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
    |> result NestedModule """
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
    |> result Namespace """
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
    |> result OpenDeclaration """
namespace TopNs

open Another
open System

type Record = 
    { F: DateTime }
"""

[<Test>]
let ``respects existing open statements identation``() =
    """
namespace TopNs

  open Another

  type Record = 
      { F: DateTime }
"""
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result OpenDeclaration """
namespace TopNs

  open Another
  open System

  type Record = 
      { F: DateTime }
"""


[<Test>]
let ``respects top level block identation in case where are no other open statements``() =
    """
namespace TopNs

 type Record = 
   { F: DateTime }
"""
    |> forLine 4
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result Namespace """
namespace TopNs

 open System
 type Record = 
   { F: DateTime }
"""

[<Test>]
let ``respects block identation in case where are no other open statements``() =
    """
namespace TopNs

module M =
 type Record = 
   { F: DateTime }
"""
    |> forLine 5
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result NestedModule """
namespace TopNs

module M =
 open System
 type Record = 
   { F: DateTime }
"""

[<Test>]
let ``anonymous module with other open statements``() =
    """
open Another
type T = { F: DateTime }
"""
    |> forLine 2
    |> forIdent "DateTime"
    |> forEntity "System" "System.DateTime"
    |> result OpenDeclaration """
open Another
open System
type T = { F: DateTime }
"""

