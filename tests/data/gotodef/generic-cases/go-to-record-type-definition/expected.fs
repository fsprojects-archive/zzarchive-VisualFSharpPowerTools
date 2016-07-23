module File

[<CustomComparison>]
[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string -> unit
    }
    interface System.ICloneable
