open System
type MyClass<'T, 'U when 'T : comparison and 'U : equality>() =
    member x.Method() = ()
