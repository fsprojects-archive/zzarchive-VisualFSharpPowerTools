namespace FSharp.Editing.ProjectSystem

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open System.IO


type ProjectSettings = 
    {   IsForStandaloneScript : bool  
        ProjectFile           : FileName
        TargetFramework       : FSharpTargetFramework
        CompilerVersion       : FSharpCompilerVersion option
        CompilerOptions       : string array
        SourceFiles           : FileName array
        FullOutputFilePath    : FileName option 
        References            : FileName array 
        ProjectReferences     : FileName array
    } 

type ProjectConfig =
    | FsProject  of ProjectSettings
    | FsxProject of ProjectSettings
    | SigProject of ProjectSettings


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProjectConfig = 

    /// Checks a file path to see if the extension matches `.fsproj`
    let isFSharpProject projectPath =
        String.equalsIgnoreCase (Path.GetExtension projectPath) fsprojext 
        
    /// Creates a ProjectConfig for a normal F# Project
    let fsProjectConfig 
        (   projectPath, targetFramework, fscVersion, fscOptions, 
            srcFiles, outputPath, references, projectReferences ) =
        {   IsForStandaloneScript = false
            ProjectFile           = projectPath
            TargetFramework       = targetFramework
            CompilerVersion       = fscVersion
            CompilerOptions       = fscOptions
            SourceFiles           = srcFiles
            FullOutputFilePath    = outputPath
            References            = references
            ProjectReferences     = projectReferences
        } |> FsProject


    /// Creates an ad-hoc ProjectConfig to integrate generated signatures into the project system
    let signatureProjectConfig (sigpath:FileName) (project:ProjectSettings) =
        let sigProjectName = sigpath + fsprojext
        let sourceFiles    = [| sigpath |]
        let flags          = [| noframeworkFlag; debugFlag; optimizeFlag; tailcallsFlag |]

        {   IsForStandaloneScript = true
            ProjectFile           = sigProjectName
            TargetFramework       = project.TargetFramework
            CompilerVersion       = project.CompilerVersion
            CompilerOptions       = flags
            SourceFiles           = sourceFiles
            FullOutputFilePath    = Some (Path.ChangeExtension(sigProjectName, ".dll"))
            References            = [||]
            ProjectReferences     = [||]
        } |> SigProject


    /// Creates a standalone ProjectConfig for an fsx script file
    let fsxProjectConfig (fsxPath:FileName)  (compilerVersion) =
        let fsxProjectName = fsxPath + fsprojext
        let flags          = [| noframeworkFlag; debugFlag; optimizeFlag; tailcallsFlag |]

        {   IsForStandaloneScript = true
            ProjectFile           = fsxProjectName
            TargetFramework       = FSharpTargetFramework.NET_4_5
            CompilerVersion       = Some compilerVersion
            CompilerOptions       = flags
            SourceFiles           = [| fsxPath |]
            FullOutputFilePath    = Some (Path.ChangeExtension(fsxProjectName, ".dll"))
            References            = [||]
            ProjectReferences     = [||]
        } |> FsxProject


