
open System.Runtime.InteropServices 
type T() =
    static member Method([<Optional; DefaultParameterValue(0)>]?x: int) = (defaultArg x 0) * 2
    static member Method2(?x: int) = (defaultArg x 0) * 2
    