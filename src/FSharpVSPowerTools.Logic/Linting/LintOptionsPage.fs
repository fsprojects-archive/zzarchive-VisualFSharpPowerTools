namespace FSharpVSPowerTools.Linting

open FSharpVSPowerTools
open Microsoft.VisualStudio.Shell
open System.ComponentModel
open System.IO
open System.Runtime.InteropServices
open System.Windows
open System.ComponentModel.Composition
open Microsoft.VisualStudio.OLE.Interop
open System.Collections.Generic
open FSharpLint.Framework.Configuration
open Management
open LintUtils

[<Guid("f0bb4785-e75a-485f-86e8-e382dd5934a4")>]
type LintOptionsPage() =
    inherit UIElementDialogPage()

    let mutable loadedConfigs = LoadedConfigs.Empty

    let config = defaultConfiguration

    let lintOptionsPageControl = lazy LintOptionsControlProvider()

    interface ILintOptions with
        member this.UpdateDirectories(_) =
            ()

        member this.GetConfigurationForDirectory(_) =
            config

    override this.OnApply(_) = 
        // TODO: Save updates to configuration
        ()

    override this.OnActivate(_) = 
        loadedConfigs <- updateLoadedConfigs loadedConfigs

        let initiallySelectedConfig = 
            match getInitialPath loadedConfigs with
            | Some(x) -> sprintf "%s%c%s" x Path.DirectorySeparatorChar SettingsFileName
            | None -> "No Projects Open"

        lintOptionsPageControl.Value.DataContext <- 
            OptionsViewModel(
                config, 
                getFileHierarchy loadedConfigs, 
                [], 
                initiallySelectedConfig)

    override this.Child = 
        let control = lintOptionsPageControl.Value
        control :> UIElement