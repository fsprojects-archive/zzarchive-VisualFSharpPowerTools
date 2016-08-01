namespace Microsoft.FSharp.Control

open System

/// This static class holds members for creating and manipulating asynchronous computations.
[<Sealed>]
[<CompiledName("FSharpAsync")>]
[<Class>]
type Async =
    /// Creates three functions that can be used to implement the .NET Asynchronous 
    /// Programming Model (APM) for a given asynchronous computation.
    static member AsBeginEnd : computation:('Arg -> Async<'T>) -> ('Arg * AsyncCallback * obj -> IAsyncResult) * (IAsyncResult -> 'T) * (IAsyncResult -> unit)
