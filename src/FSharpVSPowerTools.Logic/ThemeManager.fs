namespace FSharpVSPowerTools

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open System.Drawing
open Microsoft.Win32
open EnvDTE
open Microsoft.VisualStudio.PlatformUI
open System.Runtime.InteropServices
open Reflection

type VisualStudioTheme =
    | Unknown = 0
    | Light = 1
    | Blue = 2
    | Dark = 3

[<Guid("0d915b59-2ed7-472a-9de8-9161737ea1c5")>]
type SVsColorThemeService = interface end

[<Export>]
type ThemeManager [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =
    static let themes = 
        dict [ (Guid("de3dbbcd-f642-433c-8353-8f1df4370aba"), VisualStudioTheme.Light); 
               (Guid("a4d6a176-b948-4b29-8c66-53c97a1ed7d0"), VisualStudioTheme.Blue);
               (Guid("1ded0138-47ce-435e-84ef-9ec1f439b749"), VisualStudioTheme.Dark) ]

    let getThemeId() =
        let themeService = serviceProvider.GetService(typeof<SVsColorThemeService>)
        themeService?CurrentTheme?ThemeId: Guid

    member __.GetCurrentTheme() =
        let themeGuid = getThemeId()
        themes |> Dict.tryFind themeGuid
        |> Option.getOrTry (fun _ ->
            try 
                let color = VSColorTheme.GetThemedColor EnvironmentColors.ToolWindowTextColorKey
                if color.GetBrightness() > 0.5f then
                    VisualStudioTheme.Dark
                else
                    VisualStudioTheme.Light
            with _ -> 
                Logging.logError (fun _ -> "Can't read Visual Studio themes from environment colors.")
                VisualStudioTheme.Unknown)

    member __.GetEditorTextColors (item: string) =
        let dte = serviceProvider.GetDte()
        let fontsAndColors = dte.Properties("FontsAndColors", "TextEditor")
        let fontsAndColorsItems = fontsAndColors.Item("FontsAndColorsItems").Object :?> EnvDTE.FontsAndColorsItems
        let selectedItem = fontsAndColorsItems.Item(item)
        let foreColor = int selectedItem.Foreground |> System.Drawing.ColorTranslator.FromOle
        let backColor = int selectedItem.Background |> System.Drawing.ColorTranslator.FromOle
        (foreColor, backColor)

