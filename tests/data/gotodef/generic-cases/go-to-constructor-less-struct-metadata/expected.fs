namespace System

/// Represents a Boolean value.
[<System.Runtime.InteropServices.ComVisible(true)>]
[<Struct>]
type Boolean =
    interface System.IConvertible
    /// Compares this instance to a specified object and returns an integer that indicates their relationship to one another.
    member CompareTo : obj:obj -> int
    /// Compares this instance to a specified  object and returns an integer that indicates their relationship to one another.
    member CompareTo : value:bool -> int
