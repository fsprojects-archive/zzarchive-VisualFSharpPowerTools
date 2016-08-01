namespace Microsoft.FSharp.Core

/// Helper types for active patterns with 2 choices.
[<StructuralEquality>]
[<StructuralComparison>]
[<CompiledName("FSharpChoice`2")>]
type Choice<'T1, 'T2> =
    /// Choice 1 of 2 choices
    | Choice1Of2 of 'T1
    /// Choice 2 of 2 choices
    | Choice2Of2 of 'T2
