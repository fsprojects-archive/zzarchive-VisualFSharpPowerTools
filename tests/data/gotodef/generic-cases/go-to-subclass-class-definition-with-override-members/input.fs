
type MyBaseClass() =
    abstract member Method: int -> unit
    default this.Method(x) = ()

type MyClass() =
    inherit MyBaseClass()
    override this.Method(x) = ()