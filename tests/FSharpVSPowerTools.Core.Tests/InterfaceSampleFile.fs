type IPrintable =
   abstract member Print : unit -> unit

type SomeClass1(x: int, y: float) =
   interface IPrintable with 
      member this.Print() = printfn "%d %f" x y

let makePrintable(x: int, y: float) =
    { new IPrintable with 
          member this.Print() = printfn "%d %f" x y }

let x2 = makePrintable(1, 2.0) 
x2.Print()

let x3 =
    { new System.IDisposable with
        member x.Dispose() = raise (System.NotImplementedException()) }

type Interface1 =
    abstract member Method1 : int -> int

type Interface2 =
    abstract member Method2 : int -> int

type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int

type MyClass() =
    interface Interface3 with 
        member this.Method1(n) = 2 * n
        member this.Method2(n) = n + 100
        member this.Method3(n) = n / 10

//let _ =
//    { new System.Collections.Generic.IList<'a> with }
