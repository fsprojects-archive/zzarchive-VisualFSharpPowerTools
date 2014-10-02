namespace FSharpVSPowerTools.Folders

open System
open System.ComponentModel
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
module FolderMenuUI =
    let loadNewFolderDialog (viewModel: NewFolderNameDialogModel) =
        let window = NewFolderNameDialog().CreateRoot()
        window.DataContext <- viewModel
        window
        
    let loadMoveToFolderDialog (viewModel: MoveToFolderDialogModel) =
        let window = MoveToFolderDialog().CreateRoot()

        // Provides access to "code behind" style work
        let accessor = MoveToFolderDialog.Accessor(window)

        accessor.FoldersTree.SelectedItemChanged.Add(fun _ -> 
            viewModel.SelectedFolder <- Some (accessor.FoldersTree.SelectedItem :?> Folder)
        )

        window.DataContext <- viewModel
        window