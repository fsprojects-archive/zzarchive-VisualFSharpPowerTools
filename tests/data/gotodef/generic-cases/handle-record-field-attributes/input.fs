open System
open System.Runtime.Serialization
type Record = {
    [<DefaultValue>]
    [<Obsolete("Reason1")>]
    Field1: int

    [<Obsolete("Reason2")>]
    Field2: float
}