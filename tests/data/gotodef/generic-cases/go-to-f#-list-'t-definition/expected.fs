namespace Microsoft.FSharp.Collections

/// The type of immutable singly-linked lists.
[<DefaultAugmentation(false)>]
[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpList`1")>]
type List<'T> =
    | ( [] )
    | ( :: ) of Head: 'T * Tail: 'T list
    interface System.Collections.IEnumerable
    interface System.Collections.Generic.IEnumerable<'T>
    /// Gets a slice of the list, the elements of the list from the given start index to the given end index.
    member GetSlice : startIndex:int option * endIndex:int option -> 'T list
    /// Gets the first element of the list
    member Head : 'T
    /// Gets a value indicating if the list contains no entries
    member IsEmpty : bool
    /// Gets the element of the list at the given position.
    member Item : 'T
    /// Gets the number of items contained in the list
    member Length : int
    /// Gets the tail of the list, which is a list containing all the elements of the list, excluding the first element 
    member Tail : 'T list
    /// Returns a list with head as its first element and tail as its subsequent elements
    static member Cons : head:'T * tail:'T list -> 'T list
    /// Returns an empty list of a particular type
    static member Empty : 'T list
