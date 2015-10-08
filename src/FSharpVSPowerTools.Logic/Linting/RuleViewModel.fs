namespace FSharpVSPowerTools.Linting

open System.ComponentModel
open FSharp.ViewModule

type RuleViewModel(name:string, rules:RuleViewModel seq, settings, isChecked:bool) as this =
    inherit ViewModelBase()

    let isChecked = this.Factory.Backing(<@ this.IsChecked @>, isChecked)

    let childCheckboxChanged _ = 
        let ruleIsChecked (x:RuleViewModel) = x.IsChecked

        let allChildrenChecked = Seq.forall ruleIsChecked
        let noChildrenChecked = Seq.forall (ruleIsChecked >> not)

        if allChildrenChecked rules then
            this.IsChecked <- true
        else if noChildrenChecked rules then
            this.IsChecked <- false
        else
            // TODO: Third state.
            ()

    do
        for rule in rules do
            let notifyRuleChanged = rule :> INotifyPropertyChanged

            notifyRuleChanged.PropertyChanged.Subscribe childCheckboxChanged
            |> ignore

        (this :> INotifyPropertyChanged).PropertyChanged.Subscribe
            (fun _ -> for rule in rules do rule.IsChecked <- this.IsChecked)
        |> ignore
            
    member __.Name = name

    member __.IsChecked
        with get () = isChecked.Value
        and set (value) = isChecked.Value <- value
    
    member __.Rules = rules

    member __.HasAnySettings = (Seq.isEmpty >> not) settings
    
    member __.Settings = settings