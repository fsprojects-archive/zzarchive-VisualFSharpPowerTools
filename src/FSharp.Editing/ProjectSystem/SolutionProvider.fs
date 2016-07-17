namespace FSharp.Editing.Infrastructure

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing

type ProjectDescriptor = 
  { IsForStandaloneScript : bool  
    ProjectFile           : FileName
    TargetFramework       : FSharpTargetFramework
    CompilerVersion       : FSharpCompilerVersion option
    CompilerOptions       : string array
    SourceFiles           : FileName array
    FullOutputFilePath    : FileName option } 

type IProjectProvider =
    abstract Project: ProjectDescriptor
    abstract GetReferencedProjects: unit -> IProjectProvider list
    abstract GetAllReferencedProjectFileNames: unit -> string list 
    abstract GetProjectCheckerOptions: LanguageService -> Async<FSharpProjectOptions>

[<AutoOpen>]
module TypeExtensions =
  // this is to avoid unneeded code churn while we figure out further changes to IProjectProvider
  type IProjectProvider
    with
      member x.IsForStandaloneScript = x.Project.IsForStandaloneScript
      member x.ProjectFileName       = x.Project.ProjectFile
      member x.TargetFramework       = x.Project.TargetFramework
      member x.CompilerVersion       = x.Project.CompilerVersion
      member x.CompilerOptions       = x.Project.CompilerOptions
      member x.SourceFiles           = x.Project.SourceFiles
      member x.FullOutputFilePath    = x.Project.FullOutputFilePath