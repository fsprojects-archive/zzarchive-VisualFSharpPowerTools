namespace FSharpVSPowerTools


open System.Windows
open System.Windows.Controls
open FsXaml


type GeneralOptionsControl = XAML< @"GUI/GeneralOptionsControl.xaml",true>

type GeneralOptionsController () =
    inherit UserControlViewController<GeneralOptionsControl>()

