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
  type IProjectProvider with
      member inline x.IsForStandaloneScript = x.Project.IsForStandaloneScript
      member inline x.ProjectFileName       = x.Project.ProjectFile
      member inline x.TargetFramework       = x.Project.TargetFramework
      member inline x.CompilerVersion       = x.Project.CompilerVersion
      member inline x.CompilerOptions       = x.Project.CompilerOptions
      member inline x.SourceFiles           = x.Project.SourceFiles
      member inline x.FullOutputFilePath    = x.Project.FullOutputFilePath