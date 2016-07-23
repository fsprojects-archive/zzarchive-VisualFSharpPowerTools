module File

type MyClass =
    new : unit -> MyClass
    [<System.Obsolete ("Method is obsolete")>]
    member Method : unit -> unit
    [<System.Obsolete ("Prop is obsolete")>]
    member Prop : int
