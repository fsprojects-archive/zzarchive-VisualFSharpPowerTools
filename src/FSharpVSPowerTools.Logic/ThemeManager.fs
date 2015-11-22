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
        let dte = serviceProvider.GetService<DTE, SDTE>()
        let version = VisualStudioVersion.fromDTEVersion dte.Version
        match version with
        | VisualStudioVersion.VS2012 
        | VisualStudioVersion.VS2013 ->
            let keyName = sprintf @"Software\Microsoft\VisualStudio\%s.0\General" (VisualStudioVersion.toString version)
            use key = Registry.CurrentUser.OpenSubKey(keyName)
            key.GetValue("CurrentTheme", null)
            |> Option.ofNull
            |> Option.map string
        | VisualStudioVersion.VS2015 ->
            let keyName = sprintf @"Software\Microsoft\VisualStudio\%s.0\ApplicationPrivateSettings\Microsoft\VisualStudio" 
                            (VisualStudioVersion.toString version)
            use key = Registry.CurrentUser.OpenSubKey(keyName)
            key.GetValue("ColorTheme", null)
            |> Option.ofNull
            |> Option.map (string >> String.split StringSplitOptions.None [|"*"|])
            |> Option.bind (function 
                            | [|_; _; themeId|] -> Some themeId 
                            | arr -> 
                                Logging.logWarning (fun _ -> sprintf "Parsed Visual Studio theme settings are not well-formed %A." arr)
                                None)
        | _ ->
            Logging.logWarning (fun _ -> sprintf "Can't recognize Visual Studio version %A." version)
            None

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
                let color = VSColorTheme.GetThemedColor EnvironmentColors.ToolWindowTextColorKey
                if color.GetBrightness() > 0.5f then
                    VisualStudioTheme.Dark
                else
                    VisualStudioTheme.Light
            with _ -> 
                Logging.logError (fun _ -> "Can't read Visual Studio themes from environment colors.")
                VisualStudioTheme.Unknown)
