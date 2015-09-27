namespace FSharpVSPowerTools.Linting

open FSharpVSPowerTools
open Microsoft.VisualStudio.Shell
open System.Diagnostics
open System.Runtime.InteropServices
open System.Windows
open FSharpLint.Framework.Configuration
open Management
open LintUtils

[<Guid("f0bb4785-e75a-485f-86e8-e382dd5934a4")>]
type LintOptionsPage private (dte:EnvDTE.DTE option) =
    inherit UIElementDialogPage()

    let mutable loadedConfigs = LoadedConfigs.Empty

    let lintOptionsPageControl = lazy LintOptionsControlProvider()

    let saveViewModel (viewModel:LintViewModel) =
        match viewModel.ViewModel with
        | Some(optionsViewModel) -> 
            loadedConfigs <- saveViewModelToLoadedConfigs loadedConfigs optionsViewModel
            saveViewModel loadedConfigs optionsViewModel
        | None -> ()

    new (dte) = new LintOptionsPage(Some dte)

    new () = new LintOptionsPage(None)

    interface ILintOptions with
        member this.UpdateDirectories() =
            loadedConfigs <- updateLoadedConfigs (this.GetDte()) loadedConfigs

        member __.GetConfigurationForDirectory(dir) =
            getConfigForDirectory loadedConfigs dir

    member private this.GetDte() =
        match dte with
        | Some(dte) -> dte
        | None -> this.GetService(typeof<EnvDTE.DTE>) :?> EnvDTE.DTE
            
    override __.OnApply(_) = 
        match lintOptionsPageControl.Value.DataContext with
        | :? LintViewModel as viewModel -> saveViewModel viewModel
        | _ -> ()

    override this.OnActivate(_) = 
        let dte = this.GetDte()

        loadedConfigs <- updateLoadedConfigs dte loadedConfigs
        loadedConfigs <- refresh tryLoadConfig loadedConfigs

        let lintOptions =
            match getInitialPath dte loadedConfigs with
            | Some(path) ->
                let files = getFileHierarchy loadedConfigs

                let rec getFileViewModel (files:FileViewModel seq) =
                    let getFile (file:FileViewModel) =
                        if file.Path = path then Some(file) 
                        else getFileViewModel file.Files

                    Seq.tryPick getFile files

                match getFileViewModel files with
                | Some(file) ->
                    OptionsViewModel(
                        (fun dir -> getConfigForDirectory loadedConfigs dir),
                        files,
                        file) |> Some
                | None -> 
                    Debug.Assert(false, "No file view model for the initial file found.")
                    None
            | None -> 
                None

        lintOptionsPageControl.Value.DataContext <- LintViewModel(lintOptions, saveViewModel)
            
    override __.Child = 
        let control = lintOptionsPageControl.Value
        control :> UIElement