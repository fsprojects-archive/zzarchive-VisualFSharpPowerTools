module File

type MyClass<'T, 'U when 'T : null and 'T : (new : unit -> 'T) and 'U : struct> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
