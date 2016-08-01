module File

[<AbstractClass>]
type MyAbstractClass =
    abstract member Method : int -> unit
    override Method : x:int -> unit
