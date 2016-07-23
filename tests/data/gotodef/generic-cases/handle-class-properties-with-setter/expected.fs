module File

type MyClass =
    new : unit -> MyClass
    member GetterAndSetter : int with get, set
    member SetterOnly : int with set
    static member StaticGetterAndSetter : int with get, set
    static member StaticSetterOnly : int with set
