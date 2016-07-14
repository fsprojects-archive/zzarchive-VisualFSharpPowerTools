namespace FSharp.Editing.VisualStudio.Folders

[<RequireQualifiedAccess>]
module FolderMenuUI =
    let loadNewFolderDialog (viewModel: NewFolderNameDialogModel) =
        let window = NewFolderNameDialog()
        window.DataContext <- viewModel
        window
        
    let loadMoveToFolderDialog (viewModel: MoveToFolderDialogModel) =
        let window = MoveToFolderDialog()

        window.FoldersTree.SelectedItemChanged.Add(fun _ -> 
            viewModel.SelectedFolder <- Some (window.FoldersTree.SelectedItem :?> Folder)
        )

        window.DataContext <- viewModel
        window
