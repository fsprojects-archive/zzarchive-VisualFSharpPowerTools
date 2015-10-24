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

let _ =
    { new IComparer<'a> with
          member x.Compare(x1: 'a, y: 'a): int = 
              raise (System.NotImplementedException())
 }

type IKeyword =
    abstract Method: ``member``: int -> Member: int -> member1: int -> unit

type OKeyword =
    interface IKeyword with
        member x.Method(``member``: int) (member1: int) (member2: int): unit = 
            raise (System.NotImplementedException())                     

let xx =
    { new System.Collections.ICollection with
        member x.get_SyncRoot(): obj = 
            raise (System.NotImplementedException())
        
        member x.get_IsSynchronized(): bool = 
            raise (System.NotImplementedException())
        
        member x.GetEnumerator(): System.Collections.IEnumerator = 
            raise (System.NotImplementedException())
        
        member x.CopyTo(array: System.Array, index: int): unit = 
            raise (System.NotImplementedException())
        
        member x.get_Count(): int = 
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

let _ =
    seq { for _ in 1..10 -> { new IList<int> with
                                  member x.get_Item(index: int): int = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.set_Item(index: int, value: int): unit = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.IndexOf(item: int): int = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.Insert(index: int, item: int): unit = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.RemoveAt(index: int): unit = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.get_Count(): int = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.get_IsReadOnly(): bool = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.Add(item: int): unit = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.Clear(): unit = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.Contains(item: int): bool = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.CopyTo(array: int [], arrayIndex: int): unit = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.Remove(item: int): bool = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.GetEnumerator(): IEnumerator<int> = 
                                      raise (System.NotImplementedException())
                                  
                                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                                      raise (System.NotImplementedException())
                                  
  }
}

module M =
    type D = System.IDisposable

    let _ = { new D with
                  member x.Dispose(): unit = 
                      raise (System.NotImplementedException())
                   }

type IWithUpperCaseArgs =
    abstract Method: Arg1: int -> ARg2: int -> aRg3: int -> arG4: int -> arg5: int -> arg2: int -> unit

let _ = { new IWithUpperCaseArgs with
              member x.Method (arg1: int) (aRg2: int) (aRg3: int) (arG4: int) (arg5: int) (arg11: int): unit =
                raise (System.NotImplementedException()) }

//type Dict<'T> = IDictionary<int, 'T>
//
//let _ =
//     { new Dict<string> }
//
//type IB = IList<int>
//
//type IC = IB
//
//let _ =
//     { new IC }

type IWithProperties =
    abstract Item: v: int -> int with set

let _ = { new IWithProperties with
              member x.Item
                  with set (v: int) (v1: int): unit = 
                      raise (System.NotImplementedException())
                }

let _ = { new IDictionary<string, int> with
              member x.GetEnumerator(): System.Collections.IEnumerator = 
                  raise (System.NotImplementedException())
              
              member x.get_Item(key: string): int = 
                  raise (System.NotImplementedException())
              
              member x.set_Item(key: string, value: int): unit = 
                  raise (System.NotImplementedException())
              
              member x.get_Keys(): ICollection<string> = 
                  raise (System.NotImplementedException())
              
              member x.get_Values(): ICollection<int> = 
                  raise (System.NotImplementedException())
              
              member x.ContainsKey(key: string): bool = 
                  raise (System.NotImplementedException())
              
              member x.Add(key: string, value: int): unit = 
                  raise (System.NotImplementedException())
              
              member x.Remove(key: string): bool = 
                  raise (System.NotImplementedException())
              
              member x.TryGetValue(key: string, value: byref<int>): bool = 
                  raise (System.NotImplementedException())
              
              member x.get_Count(): int = 
                  raise (System.NotImplementedException())
              
              member x.get_IsReadOnly(): bool = 
                  raise (System.NotImplementedException())
              
              member x.Add(item: KeyValuePair<string,int>): unit = 
                  raise (System.NotImplementedException())
              
              member x.Clear(): unit = 
                  raise (System.NotImplementedException())
              
              member x.Contains(item: KeyValuePair<string,int>): bool = 
                  raise (System.NotImplementedException())
              
              member x.CopyTo(array: KeyValuePair<string,int> [], arrayIndex: int): unit = 
                  raise (System.NotImplementedException())
              
              member x.Remove(item: KeyValuePair<string,int>): bool = 
                  raise (System.NotImplementedException())
              
              member x.GetEnumerator(): IEnumerator<KeyValuePair<string,int>> = 
                  raise (System.NotImplementedException())
              
               }

type IMy<'a> = abstract Method: 'a -> unit

let _ = { new IMy<int option> with
              member x.Method(arg1: int option): unit = 
                  raise (System.NotImplementedException())
               
               }

let _ = { new IMy<Choice<int, string>> with
              member x.Method(arg1: Choice<int, string>): unit = 
                  raise (System.NotImplementedException()) }

let _ = { new IMy<int * int> with
              member x.Method(arg1: int * int): unit = 
                  raise (System.NotImplementedException()) }
              
type IMyEvent<'a> = 
    [<CLIEvent>]
    abstract M: IEvent<'a>     
    
type T1() =
    interface IMyEvent<int> with
        [<CLIEvent>]
        member x.M: IEvent<int> = 
            raise (System.NotImplementedException())

type NewInfrastructure<'T> =
    abstract ReadonlyProp: int
    abstract ReadWriteProp: int with get, set

let _ = { new NewInfrastructure<string> with
              member x.ReadWriteProp
                  with get (): int = 
                      raise (System.NotImplementedException())
                  and set (v: int): unit = 
                      raise (System.NotImplementedException())
              
              member x.ReadonlyProp: int = 
                  raise (System.NotImplementedException()) }

type NewInfrastructure2<'T> =
    [<CLIEvent>]
    abstract CLIEvent: IEvent<int>
    abstract ReadonlyProp: int

let _ = { new NewInfrastructure2<string> with
              [<CLIEvent>]
              member x.CLIEvent: IEvent<int> = 
                  raise (System.NotImplementedException())
              
              member x.ReadonlyProp: int = 
                  raise (System.NotImplementedException()) }

let _ = { new System.ComponentModel.INotifyPropertyChanged with
              [<CLIEvent>]
              member x.PropertyChanged: IEvent<System.ComponentModel.PropertyChangedEventHandler, _> = 
                  raise (System.NotImplementedException())
               }

type Base(x: System.IDisposable) = class end
               
type Derived() = 
    inherit Base ({new System.IDisposable with 
                       member x.Dispose(): unit =
                           raise (System.NotImplementedException())})

type IOverloaded =
    abstract member Foo: num:int -> int
    abstract member Foo: num:bool -> bool
    abstract member Bar: thing:string -> string

type Overloaded =
    interface IOverloaded with
        member x.Foo(num:int): int =
            raise (System.NotImplementedException())
        member x.Foo(num:bool): bool =
            raise (System.NotImplementedException())
        member x.Bar(thing) = raise (System.NotImplementedException())

type LightweightInfrastructure() =
    interface Infrastructure with
        member x.Serialize(sb) = raise (System.NotImplementedException())
        member x.ToXml() = raise (System.NotImplementedException())
