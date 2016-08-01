
type MyBaseClass() =
    abstract member Method: int -> unit
    default this.Method(x) = ()