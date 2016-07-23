module File

[<Struct>]
type MyStruct =
    new : x:int * y:string -> MyStruct
    val Field1 : int
    val Field2 : string
