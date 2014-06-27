[<AutoOpen>]
module FSharpVSPowerTools.TypedAstUtils

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

let inline hasAttribute< ^entity, 'attribute when ^entity: (member Attributes: IList<FSharpAttribute>)> entity =
    let attrs = (^entity: (member Attributes: IList<FSharpAttribute>) entity)
    attrs |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'attribute>.Name)


let (|AbbreviatedType|_|) (entity: FSharpEntity) =
    if entity.IsFSharpAbbreviation then Some entity.AbbreviatedType
    else None

let (|TypeWithDefinition|_|) (ty: FSharpType) =
    if ty.HasTypeDefinition then Some ty.TypeDefinition
    else None