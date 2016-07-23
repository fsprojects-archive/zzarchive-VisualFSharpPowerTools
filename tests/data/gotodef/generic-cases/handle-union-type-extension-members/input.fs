
type MyUnion = A of int | B of float
with
    member __.Method1() = ()

type MyUnion with
    member __.Method2() = "allo!"
