namespace System.Runtime.InteropServices

open System

/// Controls accessibility of an individual managed type or member, or of all types within an assembly, to COM.
[<Runtime.InteropServices.ComVisible(true)>]
type ComVisibleAttribute =
    inherit System.Attribute
    /// Initializes a new instance of the ComVisibleAttribute class.
    new : visibility:bool -> ComVisibleAttribute
    /// Gets a value that indicates whether the COM type is visible.
    member Value : bool
