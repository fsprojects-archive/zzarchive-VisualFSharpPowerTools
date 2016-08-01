open System
type MyClass<'T, 'U when 'T : null and 'T : (new : unit -> 'T) and 'U : struct>() =
    member x.Method() = ()
