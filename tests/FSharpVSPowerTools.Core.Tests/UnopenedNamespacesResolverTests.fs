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

type Line = int
type Source = string
type FullEntityName = string

let forLine (line: Line) (source: Source) = source, line

let andEntity (entity: FullEntityName) (source, line) =
    let parseResult =
        languageService.ParseFileInProject(LanguageServiceTestHelper.projectOptions file, file, source) 
        |> Async.RunSynchronously
    if parseResult.ParseHadErrors then failwithf "Cannot parse input: %s, errors: %A" source parseResult.Errors
    match parseResult.ParseTree with
    | None -> failwithf "ParseTree is None for input: %s" source
    | Some tree ->
        source, Ast.findNearestOpenStatementBlock line tree

let ns (expected: string) (source, actual) =
    match actual with
    | None -> failwithf "Expected Some (%A) but was None" expected
    | Some (ns', pos) -> 
        ns' |> assertEqual (expected.Split '.')
        source, pos

let position expected (source, pos) = 
    let lines = srcToLineArray source
    let line = pos.Line - 1
    if lines.Length < line + 1 then 
        failwithf "Pos.Line = %d is out of bound (source contain %d lines)" pos.Line lines.Length
    let result = Array.append (Array.append lines.[0..line - 1] [| (String.replicate pos.Col " ") + "#"|]) lines.[line..]
    result |> Collection.assertEqual (srcToLineArray expected)

[<Test>]
let ``external top level symbol, no other open declarations``() =
    """
module TopLevel

let _ = DateTime.Now
"""
    |> forLine 3
    |> andEntity "System.DateTime"
    |> ns "TopLevel"
    |> position """
module TopLevel
#

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
    |> andEntity "System.DateTime"
    |> ns "TopLevel"
    |> position """
module TopLevel

open Another
#

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
    |> andEntity "System.DateTime"
    |> ns "TopLevel"
    |> position """
module TopLevel

open Another
open OneMore
#

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
    |> andEntity "System.DateTime"
    |> ns "TopLevel.Nested"
    |> position """
module TopLevel

module Nested =
    #
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
    |> andEntity "System.DateTime"
    |> ns "TopLevel.Nested"
    |> position """
module TopLevel

module Nested =
    open Another
    #
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
    |> andEntity "System.DateTime"
    |> ns "TopLevel.Nested"
    |> position """
module TopLevel

module Nested =
    open Another
    open OneMore
    #

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
    |> andEntity "System.DateTime"
    |> ns "TopLevel.Nested.DoubleNested"
    |> position """
module TopLevel

module Nested =
    module DoubleNested =
        #
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
    |> andEntity "System.DateTime"
    |> ns "TopLevel.Nested.DoubleNested"
    |> position """
module TopLevel

open Another

module Nested =
    open OneMore

    module DoubleNested =
        open OneMore1
        #

        let _ = DateTime.Now
"""

[<Test>]
let ``top level symbol declared in a nested module in the same file``() =
    """
module TopLevel

module Nested =
    type DateTime() = class end

let _ = DateTime.Now
"""
    |> forLine 6
    |> andEntity "TopLevel.Nested.DateTime"
    |> ns "TopLevel"
    |> position """
module TopLevel

module Nested =
    type DateTime() = class end
#
let _ = DateTime.Now
"""

