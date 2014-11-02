namespace FSharp.Control

open System
open System.Threading

type private TestObserver<'a>() =
    let mutable stopped = false
    let elements = new System.Collections.Generic.List<'a>()
    interface IObserver<'a> with
        member x.OnNext value = elements.Add(value)
        member x.OnError e = raise e
        member x.OnCompleted () = stopped <- true

    member x.Elements = elements
    member x.Stopped = stopped

module Observable =
    let ofSeq<'TItem>(items:'TItem seq) =
        { new IObservable<_> with
            member __.Subscribe(observer:IObserver<_>) =
                for item in items do observer.OnNext item      
                observer.OnCompleted()     
                { new IDisposable with member __.Dispose() = () }
        }