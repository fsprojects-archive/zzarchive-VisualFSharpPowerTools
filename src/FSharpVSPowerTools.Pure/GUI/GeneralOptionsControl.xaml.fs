namespace FSharpVSPowerTools


open System.Windows
open System.Windows.Controls
open FsXaml


type GeneralOptionsControlView = XAML< @"GUI/GeneralOptionsControl.xaml",true>

type GeneralOptionsController () =
    inherit UserControlViewController<GeneralOptionsControlView>()

