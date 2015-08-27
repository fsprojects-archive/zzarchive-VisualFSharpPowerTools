namespace FSharpVSPowerTools.Linting

open System.ComponentModel
open FSharp.ViewModule

type RuleViewModel(name:string, rules:RuleViewModel seq, settings:obj seq) as this =
    inherit ViewModelBase()

    let isChecked = this.Factory.Backing(<@ this.IsChecked @>, false)
            
    member this.Name
        with get() = name

    member this.IsChecked
        with get () = isChecked.Value
        and set (value) = 
            isChecked.Value <- value

            match this.ParentRule with
            | Some(parentRule:RuleViewModel) ->
                let allSiblingsNotChecked = 
                    parentRule.Rules |> Seq.forall (fun (x:RuleViewModel) -> not x.IsChecked)

                let anySiblingsChecked = not allSiblingsNotChecked

                if allSiblingsNotChecked && parentRule.IsChecked then
                    parentRule.IsChecked <- false
                else if anySiblingsChecked && not parentRule.IsChecked then
                    parentRule.IsChecked <- true

                let isParentAndUnchecked = (Seq.isEmpty >> not) rules && not isChecked.Value

                if isParentAndUnchecked then
                    for rule in rules |> Seq.filter (fun x -> x.IsChecked) do
                        rule.IsChecked <- false
            | None -> ()
    
    member val ParentRule:RuleViewModel option = None with get, set
    
    member this.Rules
        with get() = rules

    member this.HasAnySettings
        with get() = Seq.isEmpty settings
    
    member this.Settings
        with get() = settings