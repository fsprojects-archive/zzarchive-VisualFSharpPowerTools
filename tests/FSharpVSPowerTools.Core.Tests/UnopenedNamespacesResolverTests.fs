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

let forLine line source =
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
    let result = Array.append (Array.append lines.[0..line - 1] [|"#"|]) lines.[line..]
    result |> Collection.assertEqual (srcToLineArray expected)

[<Test>]
let ``find line to insert open statement for an external top level unresolved symbol``() =
    """
module TopLevel

let _ = DateTime.Now
"""
    |> forLine 3
    |> ns "TopLevel"
    |> position """
module TopLevel
#

let _ = DateTime.Now
"""