namespace FSharp.Editing.VisualStudio.Linting

open System
open FSharpVSPowerTools
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open System.Diagnostics
open System.Runtime.InteropServices
open System.Windows
open FSharpLint.Framework.Configuration
open Management
open LintUtils
open FSharpVSPowerTools.ProjectSystem

[<ClassInterface(ClassInterfaceType.AutoDual)>]
[<Guid("f0bb4785-e75a-485f-86e8-e382dd5934a4")>]
type LintOptionsPage private (dte:EnvDTE.DTE option) =
    inherit UIElementDialogPage()

    [<Literal>]
    let MessageBoxRetryButtonClicked = 4

    let mutable loadedConfigs = LoadedConfigs.Empty

    let lintOptionsPageControl = lazy LintOptionsControlProvider()

    let saveViewModel promptRetryDialog (viewModel:LintViewModel) =
        match viewModel.ViewModel with
        | Some(optionsViewModel) -> 
            loadedConfigs <- saveViewModelToLoadedConfigs loadedConfigs optionsViewModel

            let rec trySave () =
                match saveViewModel loadedConfigs optionsViewModel with
                | Success -> ()
                | Failure reason -> 
                    match promptRetryDialog reason with
                    | MessageBoxRetryButtonClicked -> trySave ()
                    | _ -> ()
            trySave ()
        | None -> ()

    new (dte) = new LintOptionsPage(Some dte)

    new () = new LintOptionsPage(None)

    interface ILintOptions with
        member this.UpdateDirectories() =
            loadedConfigs <- updateLoadedConfigs this.Dte loadedConfigs

        member __.GetConfigurationForDirectory(dir) =
            getConfigForDirectory loadedConfigs dir

    member private this.Dte =
        match dte with
        | Some dte -> dte
        | None -> this.GetService(typeof<EnvDTE.DTE>) :?> EnvDTE.DTE

    member private this.VsUiShell =
        this.GetService(typeof<SVsUIShell>) :?> IVsUIShell

    member private this.RetrySaveDialog(failedToSaveReason) =
        match this.VsUiShell.ShowMessageBox
               (0u,
                ref Guid.Empty,
                "Unable To Apply Changes",
                sprintf "Unable to save config changes: %s." failedToSaveReason,
                "",
                0u,
                OLEMSGBUTTON.OLEMSGBUTTON_RETRYCANCEL,
                OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST,
                OLEMSGICON.OLEMSGICON_INFO,
                0) with
        | VSConstants.S_OK, result -> result
        | _ -> 0
            
    override this.OnApply e = 
        protect (fun _ ->
            match lintOptionsPageControl.Value.DataContext with
            | :? LintViewModel as viewModel -> 
                saveViewModel this.RetrySaveDialog viewModel
            | _ -> ())
        base.OnApply e

    override this.OnActivate e = 
        protect (fun _ ->
            let dte = this.Dte

            loadedConfigs <- updateLoadedConfigs dte loadedConfigs
            loadedConfigs <- refresh tryLoadConfig loadedConfigs

            let lintOptions =
                match getInitialPath dte loadedConfigs with
                | Some(path) ->
                    let files = getFileHierarchy loadedConfigs

                    let rec getFileViewModel files =
                        let getFile (file: FileViewModel) =
                            if file.Path = path then Some file 
                            else getFileViewModel file.Files

                        Seq.tryPick getFile files

                    match getFileViewModel files with
                    | Some file ->
                        OptionsViewModel(
                            (getConfigForDirectory loadedConfigs),
                            files,
                            file) |> Some
                    | None -> 
                        Debug.Assert(false, "No file view model for the initial file found.")
                        None
                | None -> 
                    None

            lintOptionsPageControl.Value.DataContext <- 
                LintViewModel(lintOptions, saveViewModel this.RetrySaveDialog))

        base.OnActivate e
            
    override __.Child = 
        protectOrDefault (fun _ ->
            let control = lintOptionsPageControl.Value
            control :> UIElement)
            null