namespace FSharpVSPowerTools.Linting

open System.ComponentModel

type BoolViewModel() =
    member val Name:string = null with get, set

type IntViewModel() =
    member val Name:string = null with get, set

type OptionsViewModel() =
    let mutable selectedRule:RuleViewModel = null

    let propertyChanged = new Event<_, _>()

    let mutable (files:FileViewModel seq) = Seq.empty

    let mutable (rules:RuleViewModel seq) = Seq.empty

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member this.OnPropertyChanged(e:PropertyChangedEventArgs) =
        propertyChanged.Trigger(this, e)

    member this.OnPropertyChanged(propertyName:string) =
        this.OnPropertyChanged(PropertyChangedEventArgs(propertyName))

    member this.SelectedRule 
        with get() = selectedRule
        and set (value) = 
            selectedRule <- value
            this.OnPropertyChanged("SelectedRule")
    
    member val SelectedFileName:int = 0 with get, set
    
    member val FileNames:string seq = null with get, set
    
    member this.Files
        with get() = files
        and set(value) = files <- value
    
    member this.Rules
        with get() = rules
        and set(value) = rules <- value