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
    abstract N: 'a

let _ =
    { new IA<int> with
        member x.M = Unchecked.defaultof<int>
        member x.N = Unchecked.defaultof<int> }

type Indexer1 =
    abstract Item: int

type Indexer2 =
    abstract Item: float with set

type Indexer3 =
    inherit Indexer1
    inherit Indexer2
    abstract Item: string with get, set

let _ =
    { new Indexer3 with
        member x.Item
            with get (): string = 
                raise (System.NotImplementedException())
        
        member x.Item
            with set (v: string): unit = 
                raise (System.NotImplementedException())
        
        member x.Item
            with set (v: float): unit = 
                raise (System.NotImplementedException())
        
        member x.Item
            with get (): int = 
                raise (System.NotImplementedException())
 }

open System.Collections.Generic

let _ =
    { new System.Collections.Generic.IList<'a> with
        member x.get_Item(index: int): 'a = 
            raise (System.NotImplementedException())
        
        member x.set_Item(index: int, value: 'a): unit = 
            raise (System.NotImplementedException())
        
        member x.IndexOf(item: 'a): int = 
            raise (System.NotImplementedException())
        
        member x.Insert(index: int, item: 'a): unit = 
            raise (System.NotImplementedException())
        
        member x.RemoveAt(index: int): unit = 
            raise (System.NotImplementedException())
        
        member x.get_Count(): int = 
            raise (System.NotImplementedException())
        
        member x.get_IsReadOnly(): bool = 
            raise (System.NotImplementedException())
        
        member x.Add(item: 'a): unit = 
            raise (System.NotImplementedException())
        
        member x.Clear(): unit = 
            raise (System.NotImplementedException())
        
        member x.Contains(item: 'a): bool = 
            raise (System.NotImplementedException())
        
        member x.CopyTo(array: 'a [], arrayIndex: int): unit = 
            raise (System.NotImplementedException())
        
        member x.Remove(item: 'a): bool = 
            raise (System.NotImplementedException())
        
        member x.GetEnumerator(): IEnumerator<'a> = 
            raise (System.NotImplementedException())
        
        member x.GetEnumerator(): System.Collections.IEnumerator = 
            raise (System.NotImplementedException())
 }

let xx =
    { new System.Collections.ICollection with
        member x.CopyTo(array: System.Array, index: int): unit = 
            raise (System.NotImplementedException())
        
        member x.get_Count(): int = 
            raise (System.NotImplementedException())
        
        member x.get_SyncRoot(): obj = 
            raise (System.NotImplementedException())
        
        member x.get_IsSynchronized(): bool = 
            raise (System.NotImplementedException())
        
        member x.GetEnumerator(): System.Collections.IEnumerator = 
            raise (System.NotImplementedException())
 }

let yy = { new IA<_> with
    member x.M
        with get (): 'a = 
            raise (System.NotImplementedException())
    
    member x.N
        with get (): 'a = 
            raise (System.NotImplementedException())
}


