namespace FSharpVSPowerTools.Linting

open FSharp.ViewModule

type RuleViewModel(name:string, rules:RuleViewModel seq, settings, isChecked:bool) as this =
    inherit ViewModelBase()

    let isChecked = this.Factory.Backing(<@ this.IsChecked @>, isChecked)
            
    member __.Name = name

    member __.IsChecked
        with get () = isChecked.Value
        and set (value) = isChecked.Value <- value
    
    member __.Rules = rules

    member __.HasAnySettings = (Seq.isEmpty >> not) settings
    
    member __.Settings = settings