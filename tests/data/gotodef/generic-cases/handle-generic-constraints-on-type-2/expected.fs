module File

open System

type MyClass<'T, 'U when 'T :> IComparable and 'U : not struct> =
    new : unit -> MyClass<'T, 'U>
    member Method : unit -> unit
