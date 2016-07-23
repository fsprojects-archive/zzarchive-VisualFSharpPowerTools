module File

type MyUnion =
    | A of int
    | B of float
    member Method1 : unit -> unit
    member Method2 : unit -> string
