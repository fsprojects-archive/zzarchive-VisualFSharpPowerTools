module File

open File

type MyClass =
    inherit MyBaseClass
    new : unit -> MyClass
    override Method : x:int -> unit
