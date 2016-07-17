namespace FSharp.Editing.Server

open FSharp.Editing
open Microsoft.FSharp.Compiler.SourceCodeServices

[<NoComparison>]
type Project =
    { FilePath: FilePath
      CompilerOptions: FSharpProjectOptions }

[<NoComparison>]
type Solution =
    { FileName: FileName option
      Projects: Map<FilePath, Project> }

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Solution =
    //let addOrUpdateProject project solution =
        //{ solution with Projects = solution.Projects |> Map.add project }