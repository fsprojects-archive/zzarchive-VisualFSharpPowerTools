module FSharpVSPowerTools.Core.Tests.OpenDeclarationsGetterTests

open System.IO
open NUnit.Framework
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier
open Microsoft.FSharp.Compiler

let fileName = Path.Combine (__SOURCE_DIRECTORY__, __SOURCE_FILE__)
let projectFileName = Path.ChangeExtension(fileName, ".fsproj")
let sourceFiles = [| fileName |]
let args = 
  [|"--noframework"; "--debug-"; "--optimize-"; "--tailcalls-";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll";
    @"-r:C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Windows.Forms.dll"|]

let framework = FSharpTargetFramework.NET_4_5
let languageService = LanguageService(fun _ -> ())
let opts source = 
    let opts = 
        languageService.GetCheckerOptions (fileName, projectFileName, source, sourceFiles, args, [||], framework) 
        |> Async.RunSynchronously
    { opts with LoadTime = System.DateTime.UtcNow }

type Line = int
type OpenDecl = string

let (=>) source (expected: (Line * (OpenDecl list)) list) = 
    let opts = opts source
    let sourceLines = source.Replace("\r\n", "\n").Split([|"\n"|], System.StringSplitOptions.None)
    let parseResults = languageService.ParseFileInProject(opts, fileName, source) |> Async.RunSynchronously

    let actualOpenDeclarations =
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
        OpenDeclarationGetter.getOpenDeclarations parseResults.ParseTree entities qualifyOpenDeclarations

    let actual =
        expected
        |> List.map (fun (line, _) ->
            match actualOpenDeclarations |> Seq.tryFind (fun decl -> decl.DeclarationRange.StartLine = line) with
            | Some decl -> 
                line,
                decl.Idents |> List.map (fun idents -> System.String.Join (".", idents))
            | None -> line, [])
        |> List.sortBy (fun (line, _) -> line)
    try actual |> Collection.assertEquiv (expected |> List.sortBy (fun (line, _) -> line))
    with _ -> 
        debug "AST: %A" parseResults.ParseTree; 
        reraise()

[<Test>]
let ``single-ident namespace``() =
    """
open System
"""
    => [2, ["System"]]

[<Test>]
let ``two-idents namespace``() =
    """
open System.IO
"""
    => [2, ["System.IO"]]

[<Test>]
let ``module``() =
    """
module Module =
    let x = ()
open Module
"""
    => [4, ["OpenDeclarationsGetterTests.Module"]]

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
    => [10, ["OpenDeclarationsGetterTests.Top2.Nested"
             "OpenDeclarationsGetterTests.Top1.Nested"]]