namespace FSharpVSPowerTools.Helpers

open EnvDTE
open EnvDTE80
open System
open FSharp

module SolutionExplorerHelper =

    [<Literal>]
    let public FSharpProjectKindGuid = "{f2a71f9b-5d33-465a-a702-920d77279786}"

    let private getSelected<'T> (dte:DTE2) =
        let items = dte.ToolWindows.SolutionExplorer.SelectedItems :?> UIHierarchyItem[]
        items
        |> Seq.map (fun i -> i.Object)
        |> Seq.map (fun o -> match o with | :? 'T as p -> Some(p) | _ -> None)
        |> Seq.filter (fun o -> match o with | Some _ -> true | None -> false)
        |> Seq.map (fun o -> o.Value)

    let getSelectedItems dte =
        getSelected<ProjectItem> dte

    let getSelectedProjects dte =
        getSelected<Project> dte

    let isFSharpProject kind = (kind = FSharpProjectKindGuid)
    let isPhysicalFolder kind = (kind = Constants.vsProjectItemKindPhysicalFolder)
