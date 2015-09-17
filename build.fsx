// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake 
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO

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
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "VisualFSharpPowerTools"
let cloneUrl = "git@github.com:fsprojects/VisualFSharpPowerTools.git"

// Ensure to use MSBuild 12 for the main solution if it exists, otherwise use version 14
let msBuildVersionToTry = ["12.0";"14.0"]

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
// Run the integration tests using test runner

Target "IntegrationTests" (fun _ ->
    !! "tests/**/bin/Release/FSharpVSPowerTools.IntegrationTests.dll" 
    |> MSTest.MSTest (fun p ->
        { p with
            TimeOut = TimeSpan.FromMinutes 20.
        })
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

#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion

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
  ==> "Build"
  ==> "BuildTests"
  ==> "UnitTests"
  ==> "Main"

"Clean"
 ==> "RunStatistics"

"Release"
  ==> "PublishNuGet"
  ==> "ReleaseAll"

"Main"
  =?> ("IntegrationTests", isLocalBuild)
  ==> "All"

"Main" 
  ==> "CleanDocs"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "Main"
