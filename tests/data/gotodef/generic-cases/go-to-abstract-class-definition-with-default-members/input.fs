
[<AbstractClass>]
type MyAbstractClass =
    abstract member Method: int -> unit
    default this.Method(x) = ()