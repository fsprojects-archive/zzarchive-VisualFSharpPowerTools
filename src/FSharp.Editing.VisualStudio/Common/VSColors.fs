namespace FSharp.Editing.VisualStudio

open System.Reflection
open Microsoft.VisualStudio.Shell
open FSharpVSPowerTools
open System.Windows.Controls

// A port of https://github.com/tomasr/viasfora/blob/master/Viasfora/Util/VsColors.cs

type VSColors() =
    static let colorTypeOpt =
        try
            let vsShellAssembly = Assembly.Load("Microsoft.VisualStudio.Shell.11.0, Version=11.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
            vsShellAssembly
            |> Option.ofNull
            |> Option.map (fun assembly ->
                assembly.GetType("Microsoft.VisualStudio.PlatformUI.EnvironmentColors"))
        with _ ->
            None

    static let tryGetOrElse (key: string) (alternate: obj) = 
        colorTypeOpt
        |> Option.map (fun colorType ->
            let prop = colorType.GetProperty(key)
            prop.GetValue(null, null))
        |> Option.getOrElse alternate

    static member CommandShelfBackgroundGradientBrushKey = tryGetOrElse "CommandShelfBackgroundGradientBrushKey" VsBrushes.CommandBarGradientBeginKey
    static member CommandBarTextActiveBrushKey = tryGetOrElse "CommandBarTextActiveBrushKey" VsBrushes.CommandBarTextActiveKey
    static member ToolTipBrushKey = tryGetOrElse "ToolTipBrushKey" TextBlock.BackgroundProperty
    static member ToolTipTextBrushKey = tryGetOrElse "ToolTipTextBrushKey" TextBlock.ForegroundProperty