namespace FSharpVSPowerTools


open System.Windows
open System.Windows.Controls
open FsXaml


type GeneralOptionsView = XAML< @"GUI/GeneralOptions.xaml",true>

type GeneralOptionsControl () = //as self =
    inherit UserControlViewController<GeneralOptionsView>()

    //do base.



    

