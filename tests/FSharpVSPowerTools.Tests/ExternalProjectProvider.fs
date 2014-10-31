namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open Microsoft.Build

type ExternalProjectProvider(projectFileName) =    
    // We use Debug mode since test assemblies are created in Debug mode in BuildTests target
    let projectResolver = FSharpProjectFileInfo.Parse(projectFileName,properties = [("Configuration", "Debug")])
    let fullProjectFileName = projectResolver.FullPath
    let frameworkVersion = 
        projectResolver.FrameworkVersion 
        |> Option.map (function 
            | "v2.0" -> FSharpTargetFramework.NET_2_0
            | "v3.0" -> FSharpTargetFramework.NET_3_0
            | "v3.5" -> FSharpTargetFramework.NET_3_5
            | "v4.0" -> FSharpTargetFramework.NET_4_0
            | _ -> FSharpTargetFramework.NET_4_5)
        |> Option.getOrElse FSharpTargetFramework.NET_4_5
    let compilerOptions = projectResolver.Options |> List.toArray
    let sourceFiles = projectResolver.CompileFiles |> List.toArray
    let outputPath = projectResolver.OutputFile
    let referencedProjectFileNames = projectResolver.ProjectReferences
    let referencedProjects: IProjectProvider list = 
        referencedProjectFileNames
        |> List.map (fun projectFile -> ExternalProjectProvider(projectFile) :> _)
    member __.ReferencedProjects = referencedProjects
    interface IProjectProvider with
        member __.IsForStandaloneScript = false
        member __.ProjectFileName = fullProjectFileName
        member __.TargetFramework = frameworkVersion
        member __.CompilerOptions = compilerOptions
        member __.SourceFiles = sourceFiles
        member __.FullOutputFilePath = outputPath
        member __.GetReferencedProjects() = referencedProjects
        member __.GetAllReferencedProjectFileNames() = referencedProjectFileNames
        member __.GetProjectCheckerOptions languageService =
            async {
                let referencedProjectOptions = 
                    referencedProjects 
                    |> List.choose (fun p -> 
                        p.FullOutputFilePath |> Option.map (fun x -> x, p.GetProjectCheckerOptions languageService |> Async.RunSynchronously))
                    |> List.toArray
                let opts = languageService.GetProjectCheckerOptions (fullProjectFileName, sourceFiles, compilerOptions, referencedProjectOptions)
                //printfn "Current project options: %A" opts
                return opts
            }