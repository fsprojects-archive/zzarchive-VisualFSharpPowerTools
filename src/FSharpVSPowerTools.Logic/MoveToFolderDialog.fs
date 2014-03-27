namespace FSharpVSPowerTools.Folders

open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding

type MoveToFolderDialog = FsXaml.XAML<"MoveToFolderDialog.xaml">

[<NoEquality; NoComparison>]
type folder = { Name: string; SubFolders: folder list }

[<NoEquality; NoComparison>]
type MoveToFolderDialogResources =
    { NbOfItems: int
      Root : folder list }

type MoveToFolderDialogModel(resources :MoveToFolderDialogResources) =
    let mutable name = ""

    let validate (name :string) =
        let name = name.Trim()
        match name with
        | "" -> Choice2Of2 Resource.validatingEmptyName
        | _ -> Choice1Of2()

    let mutable validationResult = validate name

    member x.Result = validationResult

    member x.WindowTitle =
        Resource.vsPackageTitle + (sprintf " - Move %d item%s" resources.NbOfItems (match resources.NbOfItems with | 1 -> "" | _ -> "s"))

    member x.Root = resources.Root

    member x.Name
        with get() = name
        and set (v :string) =
            name <- v
            validationResult <- validate name
