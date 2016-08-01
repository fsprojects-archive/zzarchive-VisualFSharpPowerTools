module File

type MyClass<'T, 'U when 'T : unmanaged and 'U : enum<uint32>> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
