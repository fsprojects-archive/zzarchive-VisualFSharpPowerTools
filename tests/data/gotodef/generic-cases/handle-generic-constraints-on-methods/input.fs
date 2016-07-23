
type MyClass<'T when 'T : struct>() =
    member __.NormalMethod() = ()
    member __.Method<'X when 'X : null>(x: 'X) = 0
    static member StaticMethod<'X when 'X : equality>(x: 'X * 'X) =
        x = x
