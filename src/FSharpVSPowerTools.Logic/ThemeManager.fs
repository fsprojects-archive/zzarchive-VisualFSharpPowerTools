namespace FSharpVSPowerTools

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools.ProjectSystem
open Microsoft.Win32
open EnvDTE

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
        let dte = serviceProvider.GetService<DTE, SDTE>()
        let keyName = 
            match VisualStudioVersion.fromDTEVersion dte.Version with
            | VisualStudioVersion.VS2012
            | VisualStudioVersion.VS2013
            | VisualStudioVersion.VS2015 as version ->
                Some (String.Format(@"Software\Microsoft\VisualStudio\{0}.0\{1}", VisualStudioVersion.toString version, categoryName))
            | v ->
                Logging.logWarning "Unknown Visual Studio version detected while checking theme colors: %O" v
                None
        keyName 
        |> Option.bind (fun keyName ->
            use key = Registry.CurrentUser.OpenSubKey(keyName)
            key.GetValue(themePropertyName, null)
            |> Option.ofNull
            |> Option.map string)

    member __.GetCurrentTheme() =
        getThemeId()
        |> Option.bind (fun themeId ->
            match Guid.TryParse(themeId) with
            | true, themeGuid ->
                match themes.TryGetValue(themeGuid) with
                | true, t -> Some t
                | _ -> None
            | _ -> None)
        |> Option.getOrTry (fun _ ->
            try 
                Logging.logWarning "Can't read Visual Studio themes from registry keys."
                let dte = serviceProvider.GetService<DTE, SDTE>()
                let fontsAndColors = 
                    dte.Properties("FontsAndColors", "TextEditor").Item("FontsAndColorsItems").Object :?> FontsAndColorsItems
                let background = System.Drawing.ColorTranslator.FromOle(int (fontsAndColors.Item("Plain Text").Background))
                match background.R, background.G, background.B with
                | 30uy, 30uy, 30uy -> VisualStudioTheme.Dark
                | _ -> VisualStudioTheme.Light
            with _ -> 
                Logging.logError "Can't read Visual Studio themes from Fonts and Colors Items."
                VisualStudioTheme.Unknown)
