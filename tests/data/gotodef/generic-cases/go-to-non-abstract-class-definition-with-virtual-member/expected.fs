module File

type MyBaseClass =
    new : unit -> MyBaseClass
    abstract member Method : int -> unit
    override Method : x:int -> unit
