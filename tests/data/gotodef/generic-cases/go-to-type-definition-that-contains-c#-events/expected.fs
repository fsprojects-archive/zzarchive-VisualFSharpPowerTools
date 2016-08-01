namespace System.ComponentModel

open System.ComponentModel

/// Notifies clients that a property value has changed.
[<Interface>]
type INotifyPropertyChanged =
    abstract member add_PropertyChanged : value:PropertyChangedEventHandler -> unit
    abstract member remove_PropertyChanged : value:PropertyChangedEventHandler -> unit
