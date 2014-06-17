module FSharpVSPowerTools.Core.Tests.UnopenedNamespacesResolverTests

open NUnit.Framework
open FSharpVSPowerTools

let (==>) (ns, ident, fullName) res = 
    Entity.tryCreate (Array.ofList ns) ident fullName 
    |> assertEqual (res |> Option.map (fun (ns, name) -> { Namespace = ns; Name = name }))

[<Test>] 
let ``fully qualified external entities``() =
    ([], "Now", "System.DateTime.Now") ==> Some ("System.DateTime", "Now")
    ([], "Now", "System.Now") ==> Some ("System", "Now")
    (["Myns"], "Now", "System.Now") ==> Some ("System", "Now")
    (["Myns.Nested"], "Now", "System.Now") ==> Some ("System", "Now")

[<Test>]
let ``simple entities``() =
    ([], "Now", "Now") ==> None  
    (["Myns"], "Now", "Now") ==> None

[<Test>]
let ``internal entities``() =
    (["Myns"], "Now", "Myns.Nested.Now") ==> Some ("Nested", "Now")   
    (["Myns"; "Nested"], "Now", "Myns.Nested.Nested2.Now") ==> Some ("Nested2", "Now")
    (["Myns"; "Nested"], "Now", "Myns.Nested.Now") ==> None
    (["Myns"; "Nested"], "Now", "Myns.Nested2.Now") ==> Some ("Nested2", "Now")

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

type Line = int
type Col = int

let isEntity source (line: Line) (col: Col) =
    Ast.isEntity (parseSource source) (Pos.fromZ line col)

[<Test>]
let ``type name in a binding is an entity``() =
    let isEntity = isEntity """
module TopLevel
let x: DateTime = DateTime.Now
""" 
    isEntity 2 10 |> assertTrue
    isEntity 2 20 |> assertTrue

[<Test>]
let ``open declaration is not an entity``() =
    let isEntity = isEntity """
module TopLevel
open System.Threading.Tasks
""" 
    isEntity 2 5 |> assertFalse
    isEntity 2 13 |> assertFalse
    isEntity 2 24 |> assertFalse

[<Test>]
let ``argument type annotation is an entity``() =
    let isEntity = isEntity """
module TopLevel
let f (arg: DateTime) = ()
""" 
    isEntity 2 15 |> assertTrue

[<Test>]
let ``attribute is an entity``() =
    let isEntity = isEntity """
module TopLevel
let f ([<Attribute1>] arg: DateTime) = ()
type T() =
    [<Attribute2>]
    member x.Prop = ()
    [<Attribute3>] static member StaticMember ([<Attribute4>] arg) = ()
""" 
    isEntity 2 11 |> assertTrue
    isEntity 4 8 |> assertTrue
    isEntity 6 8 |> assertTrue
    isEntity 6 51 |> assertTrue


type FullEntityName = string

let forLine (line: Line) (source: Source) = source, line
let forIdent ident (source, line) = ident, source, line

let forEntity (entity: FullEntityName) (ident, source: Source, line) =
    let tree = parseSource source
    match Ast.findNearestOpenStatementBlock line tree ident entity with
    | None -> failwith "Cannot find nearest open statement block"
    | Some (e, pos) -> source, e, pos

let result (expected: Source) (source: Source, entity, pos) = 
    let lines = srcToLineArray source
    let line = pos.Line - 1
    if lines.Length < line + 1 then 
        failwithf "Pos.Line = %d is out of bound (source contain %d lines)" pos.Line lines.Length
    let result = 
        Array.append (
            Array.append 
                lines.[0..line - 1] 
                [| (String.replicate pos.Col " ") + "open " + entity.Namespace|]) 
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

[<Test>]
let ``external top level symbol, no other open declarations``() =
    """
module TopLevel

let _ = DateTime.Now
"""
    |> forLine 3
    |> forIdent "DateTime"
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forEntity "System.DateTime"
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
    |> forLine 6
    |> forIdent "DateTime"
    |> forEntity "TopLevel.Nested.DateTime"
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
    |> forEntity "TopLevel.Nested.DateTime"
    |> result """
module TopLevel

module Nested =
    type DateTime() = class end
open Nested

let _ = DateTime.Now

module Below =
    let x = ()
"""

