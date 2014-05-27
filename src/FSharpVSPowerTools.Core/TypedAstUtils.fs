[<AutoOpen>]
module FSharpVSPowerTools.TypedAstUtils

open Microsoft.FSharp.Compiler.SourceCodeServices

let hasAttribute<'T> (attrs: seq<FSharpAttribute>) =
    attrs |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'T>.Name)

