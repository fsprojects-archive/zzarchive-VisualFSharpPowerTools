namespace FSharpVSPowerTools

open System.ComponentModel
open Microsoft.VisualStudio.Shell
open System.Runtime.InteropServices

    [<Guid("CE38C84E-BE03-472C-8741-952DAE4EDA2B")>]
    type GlobalOptionsPage () =
        inherit  DialogPage() 

        let mutable diagnosticMode = false
        let mutable backgroundCompilation = true
        let mutable projectCacheSize = 50


        interface IGlobalOptions with

            [<Category ("Debugging")>]
            [<DisplayName ("Diagnostic Mode")>]
            [<Description ("Print out stacktraces and log information to Visual Studio Output panel.")>]
            member __.DiagnosticMode with get() = diagnosticMode and set v = diagnosticMode <- v

            [<Category("Performance")>]
            [<DisplayName("Background Compilation")>]
            [<Description("Compile current project in background. Enabling the option may cause high CPU load on large projects.")>]
            member __.BackgroundCompilation with get () = backgroundCompilation and set v = backgroundCompilation <- v

            [<Category("Performance")>]
            [<DisplayName("Project Cache Size")>]
            [<Description("The number of projects where their parse and check results are cached. A large value may cause high memory load, " +
                         "which will make Visual Studio sluggish.")>]
            member __.ProjectCacheSize with get () = projectCacheSize and set v = projectCacheSize <- v
