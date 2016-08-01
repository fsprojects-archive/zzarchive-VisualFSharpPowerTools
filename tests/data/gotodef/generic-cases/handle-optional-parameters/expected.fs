module File

type T =
    new : unit -> T
    static member Method : ?x:int -> int
    static member Method2 : ?x:int -> int
