namespace FSharpPowerTools.Core.Infrastructure

open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices

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
