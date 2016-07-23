open System
type MyClass<'T, 'U when 'T : unmanaged and 'U : enum<uint32>>() =
    member x.Method() = ()
