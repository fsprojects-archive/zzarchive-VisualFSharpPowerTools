
type MyClass() =
    [<System.Obsolete("Prop is obsolete")>]
    member __.Prop = 0
    [<System.Obsolete("Method is obsolete")>]
    member __.Method() = ()
