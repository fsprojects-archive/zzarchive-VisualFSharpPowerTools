
[<Struct>]
type MyStruct =
    val Field1 : int
    val Field2 : string
    new(x, y) = { Field1 = x; Field2 = y }

let x = new MyStruct()