open System
type MyClass<'T when 'T : delegate<obj * int, unit>>() =
    member x.Method() = ()
