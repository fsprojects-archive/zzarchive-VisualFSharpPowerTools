module File

type MyClass<'T when 'T : delegate<obj * int, unit>> =
    new : unit -> MyClass<'T>
    member Method : unit -> unit
