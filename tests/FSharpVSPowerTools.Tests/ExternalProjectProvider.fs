namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools.ProjectSystem

type ExternalProjectProvider(projectFileName) =    
    let projectResolver = ProjectParser.load(projectFileName) |> Option.get
    let fullProjectFileName = ProjectParser.getFileName projectResolver
    let frameworkVersion = ProjectParser.getFrameworkVersion projectResolver
    let compilerOptions = ProjectParser.getOptions projectResolver
    let sourceFiles = ProjectParser.getFiles projectResolver
    let outputPath = ProjectParser.getOutputPath projectResolver
    let referencedProjectFileNames = ProjectParser.getProjectReferences projectResolver |> Array.toList
    let referencedProjects: IProjectProvider list = 
        referencedProjectFileNames
        |> List.map (fun proj -> ExternalProjectProvider(proj) :> _)
    interface IProjectProvider with
        member __.IsForStandaloneScript = false
        member __.ProjectFileName = fullProjectFileName
        member __.TargetFramework = frameworkVersion
        member __.CompilerOptions = compilerOptions
        member __.SourceFiles = sourceFiles
        member __.FullOutputFilePath = Some outputPath
        member __.GetReferencedProjects() = referencedProjects
        member __.GetAllReferencedProjectFileNames() = referencedProjectFileNames
        member __.GetProjectCheckerOptions languageService =
            async {
                let referencedProjectOptions = 
                    referencedProjects 
                    |> List.choose (fun p -> 
                        p.FullOutputFilePath |> Option.map (fun x -> x, p.GetProjectCheckerOptions languageService |> Async.RunSynchronously))
                    |> List.toArray
                return languageService.GetProjectCheckerOptions (fullProjectFileName, sourceFiles, compilerOptions, referencedProjectOptions)
            }

