module File

type MyClass =
    new : unit -> MyClass
    member inline Method : t: ^T -> unit when ^T : (member ConstraintMethod : unit -> unit)
    static member inline StaticMethod : unit ->  ^T when ^T : (static member Create : unit ->  ^T)
