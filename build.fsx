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
open System.Text.RegularExpressions

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
let gitHome = "https://github.com/fsprojects"
// The name of the project on GitHub
let gitName = "VisualFSharpPowerTools"
let cloneUrl = "git@github.com:fsprojects/VisualFSharpPowerTools.git"

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
      (Attribute.Title "FSharpVSPowerTools.Core" :: shared)

  CreateFSharpAssemblyInfo "src/FSharpVSPowerTools.Logic/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Tests" :: Attribute.Title "FSharpVSPowerTools.Logic" :: shared)

  CreateFSharpAssemblyInfo "src/FSharpVSPowerTools.Logic.VS2013/AssemblyInfo.fs"
      (Attribute.InternalsVisibleTo "FSharpVSPowerTools.Tests" :: Attribute.Title "FSharpVSPowerTools.Logic.VS2013" :: shared) 
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ ->
    CleanDirs ["bin"; "bin/vsix"; "temp"; "nuget"]
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

Target "CleanVSIX" (fun _ ->
    ZipHelper.Unzip "bin/vsix" "bin/FSharpVSPowerTools.vsix"
    let regex = Regex("bin")
    let filesToKeep =
      Directory.GetFiles("bin", "*.dll")
      |> Seq.map (fun fileName -> regex.Replace(fileName, "bin/vsix", 1))
    let filesToDelete = 
      Seq.fold (--) (!! "bin/vsix/*.dll") filesToKeep
        -- "bin/vsix/FsXaml.Wpf.TypeProvider.dll"
        ++ "bin/vsix/Microsoft.VisualStudio*"
        ++ "bin/vsix/Microsoft.Build*"
    DeleteFiles filesToDelete
    ZipHelper.Zip "bin/vsix" "bin/FSharpVSPowerTools.vsix" (!! "bin/vsix/**")
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

Target "NuGet" (fun _ ->
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
            ToolPath = ".nuget/nuget.exe"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = true
            Dependencies = ["FSharp.Compiler.Service", "0.0.57"] })
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
    Commit tempDocsDir (sprintf "[skip ci] Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

Target "Release" (fun _ ->
    StageAll ""
    Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
)

// --------------------------------------------------------------------------------------
// Run main targets by default. Invoke 'build <Target>' to override

Target "Main" DoNothing

Target "All" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "UnitTests"
  ==> "Main"

"Build"
  ==> "CleanVSIX"

"Build"
  ==> "NuGet"

"Main"
  =?> ("IntegrationTests", isLocalBuild)
  ==> "All"

"Main" 
  ==> "CleanDocs"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "Main"
