namespace FSharpVSPowerTools.Linting

open FSharpVSPowerTools
open Microsoft.VisualStudio.Shell
open System.ComponentModel
open System.Runtime.InteropServices
open System.Windows
open System.ComponentModel.Composition
open Microsoft.VisualStudio.OLE.Interop
open System.Collections.Generic

[<Guid("f0bb4785-e75a-485f-86e8-e382dd5934a4")>]
type LintOptionsPage() =
    inherit UIElementDialogPage()

    let getProjectPaths () =
        let dte = Package.GetGlobalService(typeof<EnvDTE.DTE>) :?> EnvDTE.DTE

        [ for x in dte.Solution.Projects do 
            if (System.String.IsNullOrEmpty >> not) x.FullName then yield x.FullName ]

    let lintOptionsPageControl = lazy (LintOptionsControlProvider() :> UIElement)

    interface ILintOptions with
        member this.UpdateDirectories(_) =
            ()

        member this.GetConfigurationForDirectory(_) =
            FSharpLint.Framework.Configuration.defaultConfiguration

    override this.OnApply(_) = 
        // TODO: Save updates to configuration
        ()

    override this.OnActivate(_) = 
        // TODO: Load configuration
        ()

    override this.Child = lintOptionsPageControl.Value