namespace FSharp.Editing.VisualStudio.Tests

open FSharp.Editing
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing.Infrastructure

type ExternalProjectProvider(projectFileName) =    
    // We use Debug mode since test assemblies are created in Debug mode in BuildTests target
    let projectOptions = ProjectCracker.GetProjectOptionsFromProjectFile(projectFileName, properties = [("Configuration", "Debug")])
    do System.Diagnostics.Debug.WriteLine(printfn "%A" projectOptions)
    let sourceFiles = 
        projectOptions.OtherOptions
        |> Array.filter (fun opt -> opt.EndsWith ".fs" || opt.EndsWith ".fsx" || opt.EndsWith ".fsi") 
    let referencedProjects = Array.toList projectOptions.ReferencedProjects
    let referencedProjectFileNames = referencedProjects |> List.map fst
    let referencedProjects : IProjectProvider list = 
        referencedProjectFileNames 
        |> List.map (fun projectFile -> ExternalProjectProvider projectFile :> IProjectProvider)

    let project =
        { IsForStandaloneScript = false
          ProjectFile           = projectFileName
          TargetFramework       = FSharpTargetFramework.NET_4_5
          CompilerVersion       = None
          CompilerOptions       = projectOptions.OtherOptions
          SourceFiles           = sourceFiles
          FullOutputFilePath    = None
        }

    member __.ReferencedProjects = referencedProjects

    interface IProjectProvider with
        member __.Project = project
        member __.GetReferencedProjects() = referencedProjects
        member __.GetAllReferencedProjectFileNames() = referencedProjectFileNames
        member __.GetProjectCheckerOptions _ = async.Return projectOptions