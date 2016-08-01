module File

type T =
    new : unit -> T
    member Test : x:(int * int) * y:int -> int
