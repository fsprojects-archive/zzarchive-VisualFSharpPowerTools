
[<AbstractClass>]
type Abstract =
    member this.P = 3
    abstract member AP: float
    static member SP = 3
    static member SM() = ()
    member this.M1() = ()
    abstract member M: unit -> int

let x: Abstract = Unchecked.defaultof<_>