// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"

open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
open System.Xml

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharpVSPowerTools"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "A collection of additional commands for F# in Visual Studio"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = """The core project of Visual F# Power Tools includes IDE-agnostic features intended to be used in different F# IDEs and editors."""
// List of author names (for NuGet package)
let authors = [ "Anh-Dung Phan"; "Vasily Kirichenko"; "Denis Ok" ]
// Tags for your project (for NuGet package)
let tags = "F# fsharp formatting editing highlighting navigation refactoring"

// File system information 
// (<solutionFile>.sln is built during the building process)
let solutionFile  = "FSharpVSPowerTools"
// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*.Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "VisualFSharpPowerTools"
let cloneUrl = "https://github.com/fsprojects/VisualFSharpPowerTools.git"

// Default to MSBuild 12 if present
let msBuildPath = (ProgramFilesX86 @@ @"\MSBuild\12.0\Bin\MSBuild.exe")
if File.Exists msBuildPath then
    setEnvironVar "MSBuild" msBuildPath
// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

let isAppVeyorBuild = environVar "APPVEYOR" <> null
let buildVersion = sprintf "%s-a%s" release.NugetVersion (DateTime.UtcNow.ToString "yyMMddHHmm")

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let shared =
      [ Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ] 

  CreateCSharpAssemblyInfo "src/FSharpVSPowerTools/Properties/AssemblyInfo.cs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Tests" :: Attribute.Title "FSharpVSPowerTools" :: shared)

  CreateFSharpAssemblyInfo "src/FSharpVSPowerTools.Core/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Core.Tests" :: Attribute.Title "FSharpVSPowerTools.Core" :: shared)

  CreateFSharpAssemblyInfo "src/FSharpVSPowerTools.Logic/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Tests" :: Attribute.Title "FSharpVSPowerTools.Logic" :: shared)

  CreateFSharpAssemblyInfo "src/FSharpVSPowerTools.Logic.VS2013/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Tests" :: Attribute.Title "FSharpVSPowerTools.Logic.VS2013" :: shared)
      
  CreateFSharpAssemblyInfo "src/FSharpVSPowerTools.Logic.VS2015/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Tests" :: Attribute.Title "FSharpVSPowerTools.Logic.VS2015" :: shared) 
)

Target "VsixManifest" (fun _ ->
    let version = sprintf "%s.%s" release.AssemblyVersion AppVeyor.AppVeyorEnvironment.BuildNumber
    let manifest = "./src/FSharpVSPowerTools/source.extension.vsixmanifest"
    let doc = new XmlDocument(PreserveWhitespace=true) in
    doc.Load manifest
    doc.GetElementsByTagName("Identity") 
      |> Seq.cast<XmlNode> 
      |> Seq.head 
      |>(fun node -> let currentVersion = node.Attributes.GetNamedItem("Version").Value
                     node.Attributes.GetNamedItem("Version").Value <- sprintf "%s.%s" currentVersion AppVeyor.AppVeyorEnvironment.BuildNumber)
    doc.Save manifest
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"; "nuget"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    // We would like to build only one solution
    !! (solutionFile + ".sln")
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

// Build test projects in Debug mode in order to provide correct paths for multi-project scenarios
Target "BuildTests" (fun _ ->    
    !! "tests/data/**/*.sln"
    |> MSBuildDebug "" "Rebuild"
    |> ignore
)

let count label glob =
    let (fileCount, lineCount) =
        !! glob
        |> Seq.map (fun path ->
            File.ReadLines(path) |> Seq.length)
        |> Seq.fold (fun (fileCount, lineCount) lineNum -> (fileCount+1, lineCount + lineNum)) (0, 0)
    printfn "%s - File Count: %i, Line Count: %i." label fileCount lineCount

Target "RunStatistics" (fun _ ->
    count "F# Source" "src/**/*.fs"
    count "C# Source" "src/**/*.cs"
    count "F# Test" "tests/**/*.fs"
    count "C# Test" "tests/**/*.cs"
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "UnitTests" (fun _ ->
    !! testAssemblies 
    |> NUnit (fun p ->
        let param =
            { p with
                DisableShadowCopy = true
                TimeOut = TimeSpan.FromMinutes 20.
                Framework = "4.5"
                Domain = NUnitDomainModel.MultipleDomainModel
                OutputFile = "TestResults.xml" }
        if isAppVeyorBuild then { param with ExcludeCategory = "AppVeyorLongRunning" } else param)
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner in parallel

Target "ParallelUnitTests" (fun _ ->
    !! testAssemblies 
    |> NUnitParallel (fun p ->
        let param =
            { p with
                DisableShadowCopy = true
                TimeOut = TimeSpan.FromMinutes 20.
                Framework = "4.5"
                Domain = NUnitDomainModel.MultipleDomainModel
                OutputFile = "TestResults.xml" }
        if isAppVeyorBuild then { param with ExcludeCategory = "AppVeyorLongRunning" } else param)
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "PublishNuGet" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project + ".Core"
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = true
            Dependencies = [ "FSharp.Compiler.Service", GetPackageVersion "packages" "FSharp.Compiler.Service" ] })
        (project + ".Core.nuspec")
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" cloneUrl "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Git.Commit.Commit tempDocsDir (sprintf "[skip ci] Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

let readString prompt echo : string =
  let rec loop cs =
    let key = Console.ReadKey(not echo)
    match key.Key with
    | ConsoleKey.Backspace -> match cs with
                              | [] -> loop []
                              | _::cs -> loop cs
    | ConsoleKey.Enter -> cs
    | _ -> loop (key.KeyChar :: cs)

  printf "%s" prompt
  let input =
    loop []
    |> List.rev
    |> Array.ofList
    |> fun cs -> new String(cs)
  if not echo then
    printfn ""
  input

#I @"packages/build/Selenium.Support/lib/net40"
#I @"packages/build/Selenium.WebDriver/lib/net40"
#r @"packages/build/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r @"packages/build/Selenium.Support/lib/net40/WebDriver.Support.dll"
#r @"packages/build/Selenium.WebDriver/lib/net40/WebDriver.dll"
#r @"packages/build/canopy/lib/canopy.dll"
#r @"packages/build/SizSelCsZzz/lib/SizSelCsZzz.dll"
open canopy
open runner
open System

Target "UploadToGallery" (fun _ ->
    canopy.configuration.chromeDir <- @"./packages/build/Selenium.WebDriver.ChromeDriver/driver"
    start chrome

    let vsixGuid = "136b942e-9f2c-4c0b-8bac-86d774189cff"
    let galleryUrl = sprintf "https://visualstudiogallery.msdn.microsoft.com/%s/edit?newSession=True" vsixGuid

    let username,password =
        let lines = File.ReadAllLines("gallerycredentials.txt")
        lines.[0],lines.[1]

    // log in to msdn
    url galleryUrl    
    "#i0116" << username
    "#i0118" << password

    click "#idSIButton9"

    sleep 5
    // start a new upload session - via hacky form link
    js (sprintf "$('form[action=\"/%s/edit/changeContributionUpload\"]').submit();" vsixGuid) |> ignore

    // select "upload the vsix"    
    let fi = System.IO.FileInfo("bin/FSharpVSPowerTools.vsix")
    
    ".uploadFileInput" << fi.FullName 
    click "#setContributionTypeButton"
    
    sleep 15

    click "#uploadButton"

    sleep 15

    quit()
)


Target "Release" (fun _ ->
    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion

    let user = readString "Username: " true
    let pw = readString "Password: " false

    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    |> uploadFile "./bin/FSharpVSPowerTools.vsix"
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "ReleaseAll"  DoNothing

// --------------------------------------------------------------------------------------
// Run main targets by default. Invoke 'build <Target>' to override

Target "Main" DoNothing

Target "All" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  =?> ("VsixManifest", isAppVeyorBuild)
  ==> "Build"
  ==> "BuildTests"
  //==> "UnitTests"
  ==> "Main"

"Clean"
  ==> "Build"
  ==> "BuildTests"
  ==> "ParallelUnitTests"

"Clean"
 ==> "RunStatistics"

"Release"
  ==> "UploadToGallery"
  ==> "PublishNuGet"
  ==> "ReleaseAll"

"Main"
  ==> "All"

"Main" 
  ==> "CleanDocs"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "Main"
