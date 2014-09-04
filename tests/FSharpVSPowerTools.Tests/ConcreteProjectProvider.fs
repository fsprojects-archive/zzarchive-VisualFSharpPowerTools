namespace FSharpVSPowerTools.Tests

open System.IO
open FSharpVSPowerTools.ProjectSystem

type ConcreteProjectProvider(projectFileName) =    
    let projectResolver = ProjectParser.load(projectFileName) |> Option.get
    let fullProjectFileName = ProjectParser.getFileName projectResolver
    let compilerOptions = ProjectParser.getOptions projectResolver
    let sourceFiles = ProjectParser.getFiles projectResolver
    let referencedProjects: IProjectProvider list = []
    interface IProjectProvider with
        member __.IsForStandaloneScript = false
        member __.ProjectFileName = fullProjectFileName
        member __.TargetFramework = ProjectParser.getFrameworkVersion projectResolver
        member __.CompilerOptions = compilerOptions
        member __.SourceFiles = sourceFiles
        member __.FullOutputFilePath = Path.ChangeExtension(projectFileName, ".dll")
        member __.GetReferencedProjects() = referencedProjects
        member __.GetAllReferencedProjectFileNames() = []
        member __.GetProjectCheckerOptions languageService =
            async {
                let referencedProjectOptions = 
                    referencedProjects 
                    |> List.map (fun p -> p.FullOutputFilePath, p.GetProjectCheckerOptions languageService |> Async.RunSynchronously)
                    |> List.toArray
                return languageService.GetProjectCheckerOptions (fullProjectFileName, sourceFiles, compilerOptions, referencedProjectOptions)
            }

