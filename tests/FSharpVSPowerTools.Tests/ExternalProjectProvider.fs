namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices

type ExternalProjectProvider(projectFileName) =    
    // We use Debug mode since test assemblies are created in Debug mode in BuildTests target
    let projectOptions = ProjectCracker.GetProjectOptionsFromProjectFile(projectFileName, properties = [("Configuration", "Debug")])
    let fullProjectFileName = projectOptions.ProjectFileName
    let frameworkVersion = FSharpTargetFramework.NET_4_5
    let opts = projectOptions.OtherOptions
    let _ = opts
//        |> Option.map (function 
//            | "v2.0" -> FSharpTargetFramework.NET_2_0
//            | "v3.0" -> FSharpTargetFramework.NET_3_0
//            | "v3.5" -> FSharpTargetFramework.NET_3_5
//            | "v4.0" -> FSharpTargetFramework.NET_4_0
//            | "v4.5" -> FSharpTargetFramework.NET_4_5
//            | _ -> FSharpTargetFramework.NET_4_6)
//        |> Option.getOrElse FSharpTargetFramework.NET_4_6
    let compilerOptions = projectOptions.OtherOptions
    let sourceFiles = [||] // projectResolver. CompileFiles |> List.toArray
    //let outputPath = projectResolver.OutputFile
    let referencedProjects = Array.toList projectOptions.ReferencedProjects
    let referencedProjectFileNames = referencedProjects |> List.map fst
    let referencedProjects: IProjectProvider list = 
        referencedProjectFileNames 
        |> List.map (fun projectFile -> ExternalProjectProvider projectFile :> IProjectProvider)

    member __.ReferencedProjects = referencedProjects
    interface IProjectProvider with
        member __.IsForStandaloneScript = false
        member __.ProjectFileName = fullProjectFileName
        member __.TargetFramework = frameworkVersion
        member __.CompilerOptions = compilerOptions
        member __.CompilerVersion = None
        member __.SourceFiles = sourceFiles
        member __.FullOutputFilePath = Some "" //outputPath
        member __.GetReferencedProjects() = referencedProjects
        member __.GetAllReferencedProjectFileNames() = referencedProjectFileNames
        member __.GetProjectCheckerOptions _ = async.Return projectOptions