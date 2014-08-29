namespace FSharpVSPowerTools.Folders

open System
open System.ComponentModel
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
module FolderMenuUI =
    let loadNewFolderDialog (viewModel: NewFolderNameDialogModel) =
        let window = NewFolderNameDialog().CreateRoot()

        // Provides access to "code behind" style work
        let accessor = NewFolderNameDialog.Accessor(window)

        // Use this until we are able to do validation directly
        accessor.txtName.TextChanged.Add(fun _ ->
            accessor.btnOk.IsEnabled <- not (viewModel :> INotifyDataErrorInfo).HasErrors
             )
        accessor.btnOk.Click.Add(fun _ -> 
            match viewModel.Result with
            | Choice1Of2 _ ->
                window.DialogResult <- Nullable true
                window.Close()
            | Choice2Of2 errorMsg ->
                window.DialogResult <- Nullable false
                Logging.messageBoxError errorMsg)
        accessor.btnCancel.Click.Add(fun _ -> 
            window.DialogResult <- Nullable false
            window.Close())
        window.DataContext <- viewModel
        window.Loaded.Add (fun _ -> accessor.txtName.SelectAll())
        window
        
    let loadMoveToFolderDialog (viewModel: MoveToFolderDialogModel) =
        let window = MoveToFolderDialog().CreateRoot()

        // Provides access to "code behind" style work
        let accessor = MoveToFolderDialog.Accessor(window)

        // Use this until we are able to do validation directly
        accessor.FoldersTree.SelectedItemChanged.Add(fun _ -> 
            viewModel.SelectedFolder <- Some (accessor.FoldersTree.SelectedItem :?> Folder)
            accessor.btnOk.IsEnabled <- viewModel.Result = Choice1Of2()
        )
        accessor.btnOk.Click.Add(fun _ -> 
            match viewModel.Result with
            | Choice1Of2 _ ->
                window.DialogResult <- Nullable true
                window.Close()
            | Choice2Of2 errorMsg ->
                window.DialogResult <- Nullable false
                Logging.messageBoxError errorMsg)
        accessor.btnCancel.Click.Add(fun _ -> 
            window.DialogResult <- Nullable false
            window.Close())
        window.DataContext <- viewModel
        window