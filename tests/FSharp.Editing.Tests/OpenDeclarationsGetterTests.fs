#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "../../src/FSharpVSPowerTools.Core/Utils.fs"
      "../../src/FSharpVSPowerTools.Core/CompilerLocationUtils.fs"
      "../../src/FSharpVSPowerTools.Core/TypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/UntypedAstUtils.fs"
      "../../src/FSharpVSPowerTools.Core/Lexer.fs"
      "../../src/FSharpVSPowerTools.Core/AssemblyContentProvider.fs"
      "../../src/FSharpVSPowerTools.Core/LanguageService.fs"
      "../../src/FSharpVSPowerTools.Core/IdentifierUtils.fs"
      "../../src/FSharpVSPowerTools.Core/OpenDeclarationsGetter.fs"      
      "TestHelpers.fs"
#else
module FSharp.Editing.Tests.OpenDeclarationsGetterTests
#endif

open System.IO
open NUnit.Framework
open FSharp.Editing
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open FSharp.Editing.Features

let openDeclWithAutoOpens decls =
    { Declarations = 
        decls |> List.map (fun (decl: string) -> decl.Split '.')
      Parent = None
      IsUsed = false }

(*** OpenDeclWithAutoOpens ***)

[<Test>]
let ``OpenDeclWithAutoOpens becomes used after updated with a matching symbol prefix``() =
    let decl = openDeclWithAutoOpens ["System.IO"]
    let matched, updatedDecl = OpenDeclWithAutoOpens.updateBySymbolPrefix [|"System"; "IO"|] decl
    assertTrue matched
    assertTrue updatedDecl.IsUsed

[<Test>]
let ``OpenDeclWithAutoOpens does not become used after updated with a not matching symbol prefix``() =
    let decl = openDeclWithAutoOpens ["System.IO"]
    let matched, updatedDecl = OpenDeclWithAutoOpens.updateBySymbolPrefix [|"System"|] decl
    assertFalse matched
    assertFalse updatedDecl.IsUsed

[<Test>]
let ``OpenDeclWithAutoOpens stays used after updated with a not matching symbol prefix``() =
    let decl = { openDeclWithAutoOpens ["System.IO"] with IsUsed = true }
    let matched, updatedDecl = OpenDeclWithAutoOpens.updateBySymbolPrefix [|"System"|] decl
    assertFalse matched
    assertTrue updatedDecl.IsUsed

[<Test>]
let ``OpenDeclWithAutoOpens with many declarations becomes used after updated with a matching symbol prefix``() =
    let decl = openDeclWithAutoOpens 
                 ["System.IO"
                  "System.IO.AutoOpenModule1"
                  "System.IO.AutoOpenModule2"]
    let matched, updatedDecl = OpenDeclWithAutoOpens.updateBySymbolPrefix [|"System"; "IO"; "AutoOpenModule1"|] decl
    assertTrue matched
    assertTrue updatedDecl.IsUsed

(*** OpenDeclaration ***)

let range (startLine, startCol) (endLine, endCol) =
    Range.mkRange "range" (Range.mkPos startLine startCol) (Range.mkPos endLine endCol)

let openDecl decls =
    { Declarations = decls
      DeclarationRange = range (0, 0) (1, 1)
      ScopeRange = range (0, 0) (100, 100)
      IsUsed = false }

[<Test>]
let ``OpenDecl becomes used if any of its declarations is used``() =
    let decl = openDecl
                 [ openDeclWithAutoOpens ["System.IO"]
                   openDeclWithAutoOpens ["Top.Module"]]
    let matched, updatedDecl = OpenDeclaration.updateBySymbolPrefix [|"Top"; "Module"|] decl
    assertTrue matched
    assertTrue updatedDecl.IsUsed
    assertEqual [false; true] (updatedDecl.Declarations |> List.map (fun decl -> decl.IsUsed))

[<Test>]
let ``OpenDecl stays used if any of its declarations is used``() =
    let decl = { openDecl
                  [ { openDeclWithAutoOpens ["System.IO"] with IsUsed = true }
                    openDeclWithAutoOpens ["Top.Module"]] 
                 with IsUsed = true }

    let matched, updatedDecl = OpenDeclaration.updateBySymbolPrefix [|"Not"; "Matching"|] decl
    assertFalse matched
    assertTrue updatedDecl.IsUsed
    assertEqual [true; false] (updatedDecl.Declarations |> List.map (fun decl -> decl.IsUsed))

[<Test>]
let ``OpenDecl marks matching child decl even though another one is already marked as used``() =
    let decl = { openDecl
                  [ { openDeclWithAutoOpens ["System.IO"] with IsUsed = true }
                    openDeclWithAutoOpens ["Top.Module"]] 
                 with IsUsed = true }

    let matched, updatedDecl = OpenDeclaration.updateBySymbolPrefix [|"Top"; "Module"|] decl
    assertTrue matched
    assertTrue updatedDecl.IsUsed
    assertEqual [true; true] (updatedDecl.Declarations |> List.map (fun decl -> decl.IsUsed))

(*** OpenDeclarationGetter ***)
 
[<Test>]
let ``first matched decl become Used, the rest decls - do not``() =
    // declaration here and in the rest of the tests are in REVERSE ORDER
    let decls =
        [ openDecl [ openDeclWithAutoOpens ["Not.Matching"]]
          openDecl [ openDeclWithAutoOpens ["System.IO.AutoOpenModule"]]
          openDecl [ openDeclWithAutoOpens ["System.IO"; "System.IO.AutoOpenModule"]]
        ]

    let updatedDecls = 
        OpenDeclarationGetter.updateOpenDeclsWithSymbolPrefix 
            [|"System"; "IO"; "AutoOpenModule"|] (range (1, 1) (2, 2)) decls

    CollectionAssert.AreEqual ([false; true; false], updatedDecls |> List.map (fun decl -> decl.IsUsed))

[<Test>]
let ``matched decl become Used, there is another already matched one below it``() =
    // declaration here and in the rest of the tests are in REVERSE ORDER
    let decls =
        [ { openDecl [ { openDeclWithAutoOpens ["Other.Already.Matched"] with IsUsed = true }] with IsUsed = true }
          openDecl [ openDeclWithAutoOpens ["System.IO"]]
        ]

    let updatedDecls = 
        OpenDeclarationGetter.updateOpenDeclsWithSymbolPrefix 
            [|"System"; "IO"|] (range (1, 1) (2, 2)) decls

    CollectionAssert.AreEqual ([true; true], updatedDecls |> List.map (fun decl -> decl.IsUsed))

(*** setOpenDeclsIsUsedFlag ***)

[<Test>]
let ``set IsUsed flag to all declarations which have a given parent``() =
    let decls =
        [ { openDecl [ { openDeclWithAutoOpens ["Top.Module.AlreadyUsed"] with IsUsed = true }] with IsUsed = true }
          openDecl [ openDeclWithAutoOpens ["Top.Module.NotUsed"]]
          openDecl [ openDeclWithAutoOpens ["System.IO"]]
          openDecl [ openDeclWithAutoOpens ["System.IO.Module"]]
        ]
    let updatedDecls =  OpenDeclarationGetter.setOpenDeclsIsUsedFlag [|"System"; "IO"|] decls
    CollectionAssert.AreEqual ([true; false; true; false], (updatedDecls |> List.map (fun d -> d.IsUsed)))

(*** spreadIsUsedFlagToParents ***)

[<Test>]
let ``spread IsUsed flag up to parents``() =
    let decls =
        [ { openDecl [ { openDeclWithAutoOpens ["System.IO.Module"] with Parent = Some [|"System"; "IO"|]; IsUsed = true }]
            with IsUsed = true }
          openDecl [ openDeclWithAutoOpens ["System.IO"]]
        ]
    let updatedDecls =  OpenDeclarationGetter.spreadIsUsedFlagToParents decls
    CollectionAssert.AreEqual ([true; true], (updatedDecls |> List.map (fun d -> d.IsUsed)))

[<Test>]
let ``spread IsUsed flag up to parents /two levels/``() =
    let decls =
        [ { openDecl [ { openDeclWithAutoOpens ["System.IO.Module"] with Parent = Some [|"System"; "IO"|]; IsUsed = true }]
            with IsUsed = true }
          openDecl [ { openDeclWithAutoOpens ["System.IO"] with Parent = Some [|"System"|] } ]
          openDecl [ openDeclWithAutoOpens ["System.IO"]]
        ]
    let updatedDecls =  OpenDeclarationGetter.spreadIsUsedFlagToParents decls
    CollectionAssert.AreEqual ([true; true; true], (updatedDecls |> List.map (fun d -> d.IsUsed)))

(*** Integration tests ***)

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let framework = FSharpCompilerVersion.FSharp_3_1
let languageService = LanguageService()
let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, LanguageServiceTestHelper.args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

type Line = int
type Parent = string
type Decl = string
type OpenDecl = Parent option * Decl list

let (=>) source (expected: (Line * (OpenDecl list)) list) = 
    let opts = opts source
    let sourceLines = String.getLines source
    let parseResults = languageService.ParseFileInProject(opts, fileName, source) |> Async.RunSynchronously

    let actualOpenDeclarations =
        let entities = 
            languageService.GetAllEntitiesInProjectAndReferencedAssemblies (opts, fileName, source)
            |> Async.RunSynchronously
        let qualifyOpenDeclarations line endColumn idents = async {
            let lineStr = sourceLines.[line - 1]
            let! tooltip = languageService.GetIdentTooltip (line, endColumn, lineStr, Array.toList idents, opts, fileName, source)
            return 
                match tooltip with
                | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
                | None -> []
        }
        OpenDeclarationGetter.getOpenDeclarations parseResults.ParseTree entities qualifyOpenDeclarations
        |> Async.RunSynchronously

    let actual =
        expected
        |> List.map (fun (line, _) ->
            match actualOpenDeclarations |> Seq.tryFind (fun decl -> decl.DeclarationRange.StartLine = line) with
            | Some decl -> 
                line,
                decl.Declarations 
                |> List.map (fun decl -> 
                    decl.Parent |> Option.map (fun x -> System.String.Join (".", x)), 
                    decl.Declarations |> List.map (fun idents -> System.String.Join (".", idents)))
                //|> List.concat
            | None -> line, [])
        |> List.sortBy (fun (line, _) -> line)
    try actual |> Collection.assertEquiv (expected |> List.sortBy (fun (line, _) -> line))
    with _ -> 
        debug "AST: %A" parseResults.ParseTree
        reraise()

[<Test>]
let ``single-ident namespace``() =
    """
open System
"""
    => [2, [None, ["System"]]]
    
[<Test>]
let ``two-idents namespace``() =
    """
open System.IO
"""
    => [2, [None, ["System.IO"]]]

[<Test>]
let ``module``() =
    """
module Module =
    let x = ()
open Module
"""
    => [4, [Some "OpenDeclarationsGetterTests", ["OpenDeclarationsGetterTests.Module"]]]

[<Test>]
let ``relative open decl opens two modules in different top modules``() =
    """
module Top1 =
    module Nested = 
        let x = 1
module Top2 =
    module Nested =
        let y = 1
open Top1
open Top2
open Nested
"""
    => [10, [Some "OpenDeclarationsGetterTests.Top2", ["OpenDeclarationsGetterTests.Top2.Nested"]
             Some "OpenDeclarationsGetterTests.Top1", ["OpenDeclarationsGetterTests.Top1.Nested"]]]

[<Test>]
let ``module with ModuleSuffix itself``() =
    """
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module InternalModuleWithSuffix =
    let x = 1
module M =
    open InternalModuleWithSuffix
"""
    => [ 6, [Some "OpenDeclarationsGetterTests", ["OpenDeclarationsGetterTests.InternalModuleWithSuffix"]]]

let (|->) (source, isSignature) ((pos, expected): pos * string list) = 
    let opts = opts source
    let fileName = Path.ChangeExtension(fileName, if isSignature then ".fsi" else ".fs")
    let parseResults = languageService.ParseFileInProject(opts, fileName, source) |> Async.RunSynchronously
     
    let actual = OpenDeclarationGetter.getEffectiveOpenDeclarationsAtLocation pos (Option.get parseResults.ParseTree)

    try actual |> Collection.assertEquiv expected
    with _ -> 
        debug "AST: %A" parseResults.ParseTree
        reraise()

let pos startLine startCol =
    Range.mkPos startLine startCol

[<Test>]
let ``open declarations from nested modules``() =
    ("""
open System
open System.IO
module M =
    module N =
        open InternalModuleWithSuffix
        let x = 0
""", false)
    |-> (pos 7 13, ["System"; "System.IO"; "InternalModuleWithSuffix"; "OpenDeclarationsGetterTests.M.N"])

[<Test>]
let ``open declarations from namespaces``() =
    ("""
namespace M
    open System
    open System.IO
    module N =
        open InternalModuleWithSuffix
        let x = 0
""", false)
    |-> (pos 7 13, ["System"; "System.IO"; "InternalModuleWithSuffix"; "M.N"])

[<Test>]
let ``open declarations with duplication``() =
    ("""
open System
open System.IO
module M =
    open System.IO
    open InternalModuleWithSuffix
    open System
    let x = 0
    open System.Collections.Generic
open System.Collections.Generic
""", false)
    |-> (pos 8 9, ["System"; "System.IO"; "InternalModuleWithSuffix"; "OpenDeclarationsGetterTests.M"])

[<Test>]
let ``open declarations with global prefix``() =
    ("""
open global.System
open System.IO
module M =
    open System.IO
    open InternalModuleWithSuffix
    open System
    let x = 0
""", false)
    |-> (pos 8 9, ["System"; "System.IO"; "InternalModuleWithSuffix"; "OpenDeclarationsGetterTests.M"])

[<Test>]
let ``open declarations from nested modules in signatures``() =
    ("""
open System
open System.IO
module M =
    open InternalModuleWithSuffix
    val x: int
""", true)
    |-> (pos 6 9, ["System"; "System.IO"; "InternalModuleWithSuffix"; "OpenDeclarationsGetterTests.M"])

[<Test>]
let ``open declarations with duplication in signatures``() =
    ("""
open System
open System.IO
module M =
    open System.IO
    open InternalModuleWithSuffix
    open System
    val x: int
    open System.Collections.Generic
open System.Collections.Generic
""", true)
    |-> (pos 8 9, ["System"; "System.IO"; "InternalModuleWithSuffix"; "OpenDeclarationsGetterTests.M"])

[<Test>]
let ``open declarations with global prefix in signatures``() =
    ("""
open global.System
open System.IO
module M =
    open System.IO
    open InternalModuleWithSuffix
    open System
    val x: int
""", true)
    |-> (pos 8 9, ["System"; "System.IO"; "InternalModuleWithSuffix"; "OpenDeclarationsGetterTests.M"])