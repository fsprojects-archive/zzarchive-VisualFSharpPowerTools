namespace System

/// Provides the base class for value types.
[<System.Runtime.InteropServices.ComVisible(true)>]
[<AbstractClass>]
type ValueType =
    /// Initializes a new instance of the  class. 
    new : unit -> ValueType
    /// Indicates whether this instance and a specified object are equal.
    [<System.Security.SecuritySafeCritical>]
    member Equals : obj:obj -> bool
    /// Returns the hash code for this instance.
    [<System.Security.SecuritySafeCritical>]
    member GetHashCode : unit -> int
    /// Returns the fully qualified type name of this instance.
    member ToString : unit -> string
