namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices

type ExternalProjectProvider(projectFileName) =    
    // We use Debug mode since test assemblies are created in Debug mode in BuildTests target
    let projectOptions = ProjectCracker.GetProjectOptionsFromProjectFile(projectFileName, properties = [("Configuration", "Debug")])
    do System.Diagnostics.Debug.WriteLine(printfn "%A" projectOptions)
    let fullProjectFileName = projectOptions.ProjectFileName
    let frameworkVersion = FSharpTargetFramework.NET_4_5
    let compilerOptions = projectOptions.OtherOptions
    let sourceFiles = projectOptions.ProjectFileNames
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
        member __.FullOutputFilePath = None
        member __.GetReferencedProjects() = referencedProjects
        member __.GetAllReferencedProjectFileNames() = referencedProjectFileNames
        member __.GetProjectCheckerOptions _ = async.Return projectOptions