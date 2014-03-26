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

open System.Text
open System.Collections.Generic

type IDocument = interface end

type Infrastructure =
    abstract Serialize: StringBuilder -> StringBuilder
    abstract ToXml: unit -> obj

type Text(text : string) = 
    interface IDocument
    interface Infrastructure with
        member this.Serialize(sb: StringBuilder) = sb.AppendFormat("\"{0}\"", this)
        member this.ToXml() = this :> obj

let obj1 = 
    { new System.Object() with
          member x.ToString() = "F#" }

type IFirst =
    abstract F: unit -> unit
    abstract G: unit -> unit

type ISecond =
    abstract H: unit -> unit
    abstract J: unit -> unit

let implementer() = 
    { new ISecond with
          member this.H() = ()
          member this.J() = ()
      interface IFirst with
          member this.F() = ()
          member this.G() = () }

type INotifyEnumerableInternal<'T> = interface end

let f () =       
    { new obj() with
        override x.ToString() = "INotifyEnumerableInternal"
      interface INotifyEnumerableInternal<'T> }

type IA<'a> = 
    abstract M: 'a

let _ =
    { new IA<'T> with
        member x.M = Unchecked.defaultof<'T> }

let _ =
    { new System.Collections.Generic.IList<'a> }
