namespace FSharp.Editing.VisualStudio.Linting

open System
open System.ComponentModel
open ViewModule
open ViewModule.FSharp

type RuleViewModel(name:string, rules:RuleViewModel seq, settings, isChecked:bool) as this =
    inherit ViewModelBase()

    let isChecked = this.Factory.Backing(<@ this.IsChecked @>, isChecked)

    let mutable suppressListeners = false

    /// Sets a checkbox without any of the observers in this model being notified.
    /// We only want them to be notified when the checkbox is updated from the UI.
    let setChecked isChecked (rule:RuleViewModel) =
        suppressListeners <- true
        rule.IsChecked <- isChecked
        suppressListeners <- false

    let ruleCheckboxChanged _ = 
        let ruleIsChecked (x:RuleViewModel) = x.IsChecked

        let anyRuleChecked = Seq.exists ruleIsChecked
        let noRuleChecked = Seq.forall (ruleIsChecked >> not)

        if anyRuleChecked rules then
            setChecked true this
        else if noRuleChecked rules then
            setChecked false this

    do
        for rule in rules do
            (rule :> INotifyPropertyChanged).PropertyChanged
            |> Observable.filter (fun _ -> not suppressListeners)
            |> Observable.subscribe ruleCheckboxChanged
            |> ignore

        (this :> INotifyPropertyChanged).PropertyChanged
        |> Observable.filter (fun _ -> not suppressListeners)
        |> Observable.subscribe (fun _ -> rules |> Seq.iter (setChecked this.IsChecked))
        |> ignore
            
    member __.Name = name

    member __.IsChecked
        with get () = isChecked.Value
        and set (value) = isChecked.Value <- value
    
    member __.Rules = rules

    member __.HasAnySettings = (Seq.isEmpty >> not) settings
    
    member __.Settings = settings