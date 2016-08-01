module File

type MyClass<'T when 'T : struct> =
    new : unit -> MyClass<'T>
    member Method : x:'X -> int when 'X : null
    member NormalMethod : unit -> unit
    static member StaticMethod : x:('X * 'X) -> bool when 'X : equality
