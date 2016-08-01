namespace Microsoft.FSharp.Core

/// Adding this attribute to a value or function definition in an F# module changes the name used
/// for the value in compiled CLI code.
[<Sealed>]
type CompiledNameAttribute =
    inherit System.Attribute
    /// Creates an instance of the attribute
    new : compiledName:string -> CompiledNameAttribute
    /// The name of the value as it appears in compiled code
    member CompiledName : string
