namespace FSharp.Editing.Server

open FSharp.Editing

type Project =
    { FilePath: FilePath }

type Solution =
    { Projects: Map<FilePath, Project> } 