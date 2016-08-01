module File

type MyClass<'T, 'U when 'T : comparison and 'U : equality> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
