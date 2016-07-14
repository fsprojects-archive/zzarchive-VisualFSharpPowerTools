namespace FSharp.Editing.Infrastructure

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing

type IProjectProvider =
    abstract IsForStandaloneScript: bool
    abstract ProjectFileName: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerVersion: FSharpCompilerVersion option
    abstract CompilerOptions: string []
    abstract SourceFiles: string []
    abstract FullOutputFilePath: string option
    abstract GetReferencedProjects: unit -> IProjectProvider list
    abstract GetAllReferencedProjectFileNames: unit -> string list 
    abstract GetProjectCheckerOptions: LanguageService -> Async<FSharpProjectOptions>
