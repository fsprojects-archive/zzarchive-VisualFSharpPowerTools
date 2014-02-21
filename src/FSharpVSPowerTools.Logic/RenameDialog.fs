namespace FSharpVSPowerTools.Refactoring
 
open System
open System.Windows
open System.Windows.Input
open System.ComponentModel

type RenameDialog = FSharpx.XAML<"RenameDialog.xaml">

[<RequireQualifiedAccess>]
module UI =
    let loadRenameDialog(viewModel) =
        let window = RenameDialog()
        window.btnOk.Click.Add(fun _ -> 
            window.Root.DialogResult <- Nullable true
            window.Root.Close())
        window.btnCancel.Click.Add(fun _ -> 
            window.Root.DialogResult <- Nullable false
            window.Root.Close())
        window.Root.DataContext <- viewModel
        window.Root
 