module FSharp.Editing.Tests.GoToDefinition.LoadDirectiveTests

open System.IO
open Microsoft.FSharp.Compiler.Range
open NUnit.Framework
open FSharp.Editing
open FSharp.Editing.UntypedAstUtils.HashDirectiveInfo
open FSharp.Editing.Tests

[<Literal>]
let dataFolderName = __SOURCE_DIRECTORY__ + "/../data/"
type dataFolder = FSharp.Management.FileSystem<dataFolderName>

let languageService = LanguageService()


let canonicalizeFilename filename = Path.GetFullPathSafe filename //(new FileInfo(filename)).FullName

let getAst filename = 
    let contents = File.ReadAllText(filename)
    
    // Get compiler options for the 'project' implied by a single script file
    let projOptions = 
        languageService.GetScriptCheckerOptions(filename, filename + ".fsproj", contents, FSharpCompilerVersion.FSharp_3_1)
        |> Async.RunSynchronously

    let parseFileResults = 
        languageService.ParseFileInProject(projOptions, filename, contents) 
        |> Async.RunSynchronously

    match parseFileResults.ParseTree with
    | Some tree -> tree
    | None -> failwith "Something went wrong during parsing!"


[<Test>]
let ``test1.fsx: verify parsed #load directives``() =
   let ast = getAst dataFolder.ParseLoadDirectives.``test1.fsx``
   let directives = getIncludeAndLoadDirectives ast
   
   let expectedMatches =
       [
       Some <| FileInfo(dataFolder.ParseLoadDirectives.includes.``a.fs``).FullName
       Some <| FileInfo(dataFolder.ParseLoadDirectives.includes.``b.fs``).FullName
       Some <| FileInfo(dataFolder.ParseLoadDirectives.includes.``b.fs``).FullName
       ]
   
   let results =
       directives
       |> Seq.map (function
          | Load(ExistingFile(filename), _) -> Some ((new FileInfo(filename)).FullName)
          | _ -> None
       )
       |> Seq.filter (Option.isSome)
       |> Seq.toList
   
   assertEqual expectedMatches results

[<Test>]
let ``test1.fsx: verify parsed position lookup of individual #load directives``() =
   let ast = getAst dataFolder.ParseLoadDirectives.``test1.fsx``
   
   let expectations = [
     (mkPos 1 1,       Some dataFolder.ParseLoadDirectives.includes.``a.fs``)
     (mkPos 1 5,       Some dataFolder.ParseLoadDirectives.includes.``a.fs``)
     (mkPos 2 1,       Some dataFolder.ParseLoadDirectives.includes.``b.fs``)
     (mkPos 2 5,       Some dataFolder.ParseLoadDirectives.includes.``b.fs``)
     (mkPos 3 1000,    None)
     (mkPos 4 5,       Some dataFolder.ParseLoadDirectives.includes.``b.fs``)
   ]
   
   let results =
       expectations
       |> Seq.map fst
       |> Seq.map (fun pos -> 
          let result = getHashLoadDirectiveResolvedPathAtPosition pos ast
          match result with
          | None      -> pos, None
          | Some path -> pos, Some (canonicalizeFilename path)
       )
       |> Seq.toList
   
   assertEqual expectations results
