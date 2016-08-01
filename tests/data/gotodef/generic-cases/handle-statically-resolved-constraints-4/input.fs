
type MyClass() =
    member inline __.Method< ^T when ^T : (member ConstraintMethod : unit -> unit)>(t : ^T) = ()
    static member inline StaticMethod< ^T when ^T : (static member Create : unit -> ^T)>() =
        (^T : (static member Create : unit -> ^T) ())
