namespace System

/// Represents a 2-tuple, or pair. 
type Tuple<'T1, 'T2> =
    /// Initializes a new instance of the  class.
    new : item1:'T1 * item2:'T2 -> Tuple<'T1, 'T2>
    /// Returns a value that indicates whether the current  object is equal to a specified object.
    member Equals : obj:obj -> bool
    /// Returns the hash code for the current  object.
    member GetHashCode : unit -> int
    /// Gets the value of the current  object's first component.
    member Item1 : 'T1
    /// Gets the value of the current  object's second component.
    member Item2 : 'T2
    /// Returns a string that represents the value of this  instance.
    member ToString : unit -> string
