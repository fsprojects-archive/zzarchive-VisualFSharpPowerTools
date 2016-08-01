module File

[<AbstractClass>]
type Abstract =
    abstract member AP : float
    abstract member M : unit -> int
    member M1 : unit -> unit
    member P : int
    static member SM : unit -> unit
    static member SP : int
