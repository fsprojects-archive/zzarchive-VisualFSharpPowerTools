[<AutoOpen>]
module FSharpVSPowerTools.TypedAstUtils

open Microsoft.FSharp.Compiler.SourceCodeServices

let hasAttribute<'T> (attrs: seq<FSharpAttribute>) =
    attrs |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'T>.Name)

let isOperator (name: string) =
    if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4 then
        name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
    else false

let rec getEntityAbbreviatedType (entity: FSharpEntity) =
    if entity.IsFSharpAbbreviation then
        let typ = entity.AbbreviatedType
        if typ.HasTypeDefinition then getEntityAbbreviatedType typ.TypeDefinition
        else entity, Some typ
    else entity, None

let rec getAbbreviatedType (fsharpType: FSharpType) =
    if fsharpType.IsAbbreviation then
        let typ = fsharpType.AbbreviatedType
        if typ.HasTypeDefinition then getAbbreviatedType typ
        else fsharpType
    else fsharpType

/// Field (field, fieldAbbreviatedType)
let (|Field|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpField as field -> Some (field, getAbbreviatedType field.FieldType)
    | _ -> None

let inline (|MutableVar|_|) (symbol: ^a when ^a: (member IsMutable: bool)) = 
    if (^a: (member IsMutable: bool) symbol) then Some() else None

let (|RefCell|_|) (ty: FSharpType) = 
    let ty = getAbbreviatedType ty
    if ty.HasTypeDefinition 
        && ty.TypeDefinition.IsFSharpRecord
        && ty.TypeDefinition.FullName = "Microsoft.FSharp.Core.FSharpRef`1" then Some() 
    else None

let (|Pattern|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpGenericParameter
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

let (|Class|_|) (orig: FSharpEntity, abbr: FSharpEntity, _) = 
    if (abbr.IsClass && 
        (not abbr.IsStaticInstantiation 
            // here we must use "entity", not "e" because "entity" could be an alias, but "e" is certainly cannot
            || orig.IsFSharpAbbreviation)) then Some()
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

let (|Function|_|) isFromComputationExpression (func: FSharpMemberFunctionOrValue) =
    if func.FullType.IsFunctionType 
            && not func.IsPropertyGetterMethod 
            && not func.IsPropertySetterMethod
            && not isFromComputationExpression
            && not (isOperator func.DisplayName) then Some()
    else None