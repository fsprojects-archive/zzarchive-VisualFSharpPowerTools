namespace FSharpVSPowerTools.Linting

open System.ComponentModel

[<AllowNullLiteral>]
type RuleViewModel() =
    let mutable isChecked = false

    let propertyChanged = new Event<_, _>()
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member this.OnPropertyChanged(e:PropertyChangedEventArgs) =
        propertyChanged.Trigger(this, e)

    member this.OnPropertyChanged(propertyName:string) =
        this.OnPropertyChanged(PropertyChangedEventArgs(propertyName))
            
    member val Name:string = null with get, set
    
    member val Description:string = null with get, set

    member this.IsChecked
        with get () = isChecked
        and set (value) = 
            isChecked <- value;
            this.OnPropertyChanged("IsChecked")

            let isChild = this.ParentRule <> null

            let allSiblingsNotChecked = isChild && 
                                        this.ParentRule.Rules |> Seq.forall (fun x -> not x.IsChecked)

            let anySiblingsChecked = isChild && not allSiblingsNotChecked

            if allSiblingsNotChecked && this.ParentRule.IsChecked then
                this.ParentRule.IsChecked <- false
            else if anySiblingsChecked && not this.ParentRule.IsChecked then
                this.ParentRule.IsChecked <- true

            let isParentAndUnchecked = this.Rules <> null && not isChecked

            if isParentAndUnchecked then
                for rule in this.Rules |> Seq.filter (fun x -> x.IsChecked) do
                    rule.IsChecked <- false
    
    member val ParentRule:RuleViewModel = null with get, set
    
    member val Rules:RuleViewModel seq = null with get, set

    member this.HasAnySettings
        with get () = this.Settings = null || Seq.isEmpty this.Settings
    
    member val Settings:obj seq = null with get, set