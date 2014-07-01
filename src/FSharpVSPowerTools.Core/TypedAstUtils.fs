[<AutoOpen>]
module FSharpVSPowerTools.TypedAstUtils

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

let hasAttribute<'attribute> (attributes: seq<FSharpAttribute>) =
    attributes |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'attribute>.Name)

let isOperator (name: string) =
    name.StartsWith "( " && name.EndsWith " )" && name.Length > 4
        && name.Substring (2, name.Length - 4) |> String.forall ((<>) ' ')
        
let (|AbbreviatedType|_|) (entity: FSharpEntity) =
    if entity.IsFSharpAbbreviation then Some entity.AbbreviatedType
    else None

let (|TypeWithDefinition|_|) (ty: FSharpType) =
    if ty.HasTypeDefinition then Some ty.TypeDefinition
    else None

let rec getEntityAbbreviatedType (entity: FSharpEntity) =
    if entity.IsFSharpAbbreviation then
        match entity.AbbreviatedType with
        | TypeWithDefinition def -> getEntityAbbreviatedType def
        | abbreviatedType -> entity, Some abbreviatedType
    else entity, None

let rec getAbbreviatedType (fsharpType: FSharpType) =
    if fsharpType.IsAbbreviation then
        getAbbreviatedType fsharpType.AbbreviatedType
    else fsharpType

/// Field (field, fieldAbbreviatedType)
let (|Field|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpField as field -> Some (field, getAbbreviatedType field.FieldType)
    | _ -> None

let (|MutableVar|_|) (symbol: FSharpSymbol) = 
    let isMutable = 
        match symbol with
        | :? FSharpField as field -> field.IsMutable
        | :? FSharpMemberFunctionOrValue as func -> func.IsMutable
        | _ -> false
    if isMutable then Some() else None
    

let (|RefCell|_|) (ty: FSharpType) = 
    match getAbbreviatedType ty with
    | TypeWithDefinition def when 
        def.IsFSharpRecord && def.FullName = "Microsoft.FSharp.Core.FSharpRef`1" -> Some() 
    | _ -> None

let (|Pattern|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpUnionCase
    | :? FSharpActivePatternCase -> Some()
    | _ -> None

/// Entity (originalEntity, abbreviatedEntity, abbreviatedType)
let (|Entity|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity -> 
        let abbreviatedEntity, abbreviatedType = getEntityAbbreviatedType entity
        Some (entity, abbreviatedEntity, abbreviatedType)
    | _ -> None

let (|ValueType|_|) (e: FSharpEntity) =
    if e.IsEnum || e.IsValueType || hasAttribute<MeasureAnnotatedAbbreviationAttribute> e.Attributes then Some()
    else None

let (|Class|_|) (original: FSharpEntity, abbreviated: FSharpEntity, _) = 
    if (abbreviated.IsClass 
        && (not abbreviated.IsStaticInstantiation || original.IsFSharpAbbreviation)) then Some()
    else None 
        
let (|FSharpType|_|) (e: FSharpEntity) = 
    if e.IsDelegate || e.IsFSharpExceptionDeclaration || e.IsFSharpRecord || e.IsFSharpUnion 
        || e.IsInterface || e.IsMeasure 
        || (e.IsFSharp && e.IsOpaque && not e.IsFSharpModule && not e.IsNamespace) then Some() 
    else None

let (|ProvidedType|_|) (e: FSharpEntity) =
    if (e.IsProvided || e.IsProvidedAndErased || e.IsProvidedAndGenerated) && e.CompiledName = e.DisplayName then
        Some()
    else None

let (|ByRef|_|) (e: FSharpEntity) = if e.IsByRef then Some() else None
let (|Array|_|) (e: FSharpEntity) = if e.IsArrayType then Some() else None
let (|Module|_|) (entity: FSharpEntity) = if entity.IsFSharpModule then Some() else None

let (|Tuple|_|) (ty: FSharpType option) = 
    ty |> Option.bind (fun ty -> if ty.IsTupleType then Some() else None)

/// Func (memberFunctionOrValue, fullType)
let (|MemberFunctionOrValue|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpMemberFunctionOrValue as func -> Some (func, func.FullType)
    | _ -> None

/// Constructor (enclosingEntity)
let (|Constructor|_|) (func: FSharpMemberFunctionOrValue) =
    if func.CompiledName = ".ctor" then Some func.EnclosingEntity
    else None

let (|Function|_|) excluded (func: FSharpMemberFunctionOrValue) =
    if func.FullType.IsFunctionType 
       && not func.IsPropertyGetterMethod 
       && not func.IsPropertySetterMethod
       && not excluded
       && not (isOperator func.DisplayName) then Some()
    else None