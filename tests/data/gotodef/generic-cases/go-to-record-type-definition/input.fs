
[<CustomComparison>]
[<CustomEquality>]
type MyRecord =
    {
        Field1: int
        Field2: string -> unit
    }
    interface System.ICloneable with
        member x.Clone(): obj = null

let r: MyRecord = Unchecked.defaultof<_>