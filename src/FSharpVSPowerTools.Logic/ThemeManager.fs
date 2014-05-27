namespace FSharpVSPowerTools

open System
open System.Diagnostics
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open Microsoft.Win32
open System.Windows.Media

type VisualStudioTheme =
    | Unknown = 0
    | Light = 1
    | Blue = 2
    | Dark = 3

[<Export>]
type ThemeManager [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =
    static let themes = 
        dict [ (Guid("de3dbbcd-f642-433c-8353-8f1df4370aba"), VisualStudioTheme.Light); 
               (Guid("a4d6a176-b948-4b29-8c66-53c97a1ed7d0"), VisualStudioTheme.Blue);
               (Guid("1ded0138-47ce-435e-84ef-9ec1f439b749"), VisualStudioTheme.Dark) ]

    let getThemeId() =
        let categoryName = "General"
        let themePropertyName = "CurrentTheme"
        let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
        let keyName = 
            match VisualStudioVersion.fromDTEVersion dte.Version with
            | VisualStudioVersion.VS2012 ->
                String.Format(@"Software\Microsoft\VisualStudio\11.0\{0}", categoryName)
            | VisualStudioVersion.VS2013 ->
                String.Format(@"Software\Microsoft\VisualStudio\12.0\{0}", categoryName)
            | v ->
                debug "Unknown Visual Studio version detected while updating theme colors: %O" v
                null
        use key = Registry.CurrentUser.OpenSubKey(keyName)
        match key with
        | null -> None
        | _ -> Some (string <| key.GetValue(themePropertyName, String.Empty))

    member x.GetCurrentTheme() =
        match getThemeId() with
        | Some themeId ->
            match Guid.TryParse(themeId) with
            | true, themeGuid ->
                match themes.TryGetValue(themeGuid) with
                | true, t -> t
                | _ -> VisualStudioTheme.Unknown
            | _ ->
                VisualStudioTheme.Unknown
        | None -> 
            VisualStudioTheme.Unknown
