// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "FSharpVSPowerTools.Core.dll" ]
// Web site location for the generated documentation
let website = "."

let githubLink = "http://github.com/fsprojects/VisualFSharpPowerTools"

// Specify more information about your project
let info =
  [ "project-name", "Visual F# Power Tools"
    "project-author", "Anh-Dung Phan, Vasily Kirichenko, Denis Ok"
    "project-summary", "A collection of additional commands for F# in Visual Studio"
    "project-github", githubLink
    "project-nuget", "http://nuget.com/packages/VisualFSharpPowerTools.Core" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#I "../../packages/FSharp.Formatting/lib/net40"
#I "../../packages/RazorEngine/lib/net40"
// Use this until FSharp.Formatting upgrades to a new version of FCS
#I "../../packages/FSharp.Compiler.Service.0.0.61/lib/net45"
#r "../../packages/Microsoft.AspNet.Razor/lib/net40/System.Web.Razor.dll"
#r "../../packages/FAKE/tools/FakeLib.dll"
#r "RazorEngine.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"

open Fake
open System.IO
open Fake.FileHelper
open FSharp.Literate
open FSharp.MetadataFormat

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../bin"
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  CopyRecursive files output true |> Log "Copying file: "
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let refRoot = website + "/.."
#else
let refRoot = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Build API reference from XML comments
let buildReference () =
  CleanDir (output @@ "reference")
  for lib in referenceBinaries do
    MetadataFormat.Generate
      ( bin @@ lib, output @@ "reference", layoutRoots, 
        parameters = ("root", refRoot)::info,
        sourceRepo = githubLink @@ "tree/master",
        sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
        libDirs = [ bin ] )

#if RELEASE
let docRoot = website
#else
let docRoot = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    Literate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", docRoot)::info,
        lineNumbers = false,
        layoutRoots = layoutRoots,
        generateAnchors = true )

// Generate
copyFiles()
buildDocumentation()
buildReference()
