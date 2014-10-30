namespace FSharpVSPowerTools.Folders

open System
open System.ComponentModel
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
module FolderMenuUI =
    let loadNewFolderDialog (viewModel: NewFolderNameDialogModel) =
        let window = NewFolderNameDialog()
        window.Root.DataContext <- viewModel
        window.Root
        
    let loadMoveToFolderDialog (viewModel: MoveToFolderDialogModel) =
        let window = MoveToFolderDialog()

        window.FoldersTree.SelectedItemChanged.Add(fun _ -> 
            viewModel.SelectedFolder <- Some (window.FoldersTree.SelectedItem :?> Folder)
        )

        window.Root.DataContext <- viewModel
        window.Root