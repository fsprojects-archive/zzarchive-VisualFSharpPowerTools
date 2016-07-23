open System
type MyClass<'T, 'U when 'T :> IComparable and 'U : not struct>() =
    member x.Method() = ()

