[<AutoOpen>]
module internal FSharpVSPowerTools.TypedAstUtils

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices

let isSymbolLocalForProject (symbol: FSharpSymbol) = 
    match symbol with 
    | :? FSharpParameter -> true
    | :? FSharpMemberFunctionOrValue as m -> not m.IsModuleValueOrMember || not m.Accessibility.IsPublic
    | :? FSharpEntity as m -> not m.Accessibility.IsPublic
    | :? FSharpGenericParameter -> true
    | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
    | :? FSharpField as m -> not m.Accessibility.IsPublic
    | _ -> false

let isAttribute<'T> (attribute: FSharpAttribute) =
    attribute.AttributeType.CompiledName = typeof<'T>.Name

let hasAttribute<'T> (attributes: seq<FSharpAttribute>) =
    attributes |> Seq.exists isAttribute<'T>

let tryGetAttribute<'T> (attributes: seq<FSharpAttribute>) =
    attributes |> Seq.tryFind isAttribute<'T>
    
let hasModuleSuffixAttribute (entity: FSharpEntity) = 
     entity.Attributes
     |> tryGetAttribute<CompilationRepresentationAttribute>
     |> Option.bind (fun a -> 
          a.ConstructorArguments 
          |> Seq.tryPick (fun arg ->
               let res =
                   match arg with
                   | :? int32 as arg when arg = int CompilationRepresentationFlags.ModuleSuffix -> 
                       Some() 
                   | :? CompilationRepresentationFlags as arg when arg = CompilationRepresentationFlags.ModuleSuffix -> 
                       Some() 
                   | _ -> 
                       None
               res))
     |> Option.isSome

let isOperator (name: string) =
    name.StartsWith "( " && name.EndsWith " )" && name.Length > 4
        && name.Substring (2, name.Length - 4) |> String.forall ((<>) ' ')

let private UnnamedUnionFieldRegex = Regex("^Item(\d+)?$", RegexOptions.Compiled)
let isUnnamedUnionCaseField (field: FSharpField) = UnnamedUnionFieldRegex.IsMatch(field.Name)
        
type FSharpEntity with
    member x.TryGetFullName() =
        x.TryFullName
        |> Option.orTry (fun _ -> 
            Option.attempt (fun _ -> x.DisplayName)
            |> Option.map (fun displayName -> String.Join(".", x.AccessPath, displayName)))

    member x.TryGetFullDisplayName() =
        let fullName = x.TryGetFullName() |> Option.map (fun fullName -> fullName.Split '.')
        let res = 
            match fullName with
            | Some fullName ->
                match Option.attempt (fun _ -> x.DisplayName) with
                | Some shortDisplayName when not (shortDisplayName.Contains ".") ->
                    Some (fullName |> Array.replace (fullName.Length - 1) shortDisplayName)
                | _ -> Some fullName
            | None -> None 
            |> Option.map (fun fullDisplayName -> String.Join (".", fullDisplayName))
        //debug "GetFullDisplayName: FullName = %A, Result = %A" fullName res
        res

type FSharpMemberFunctionOrValue with
    // FullType may fail with exception (see https://github.com/fsharp/fsharp/issues/307). 
    member x.FullTypeSafe = Option.attempt (fun _ -> x.FullType)
    member x.TryGetFullDisplayName() =
        let fullName = Option.attempt (fun _ -> x.FullName.Split '.')
        match fullName with
        | Some fullName ->
            match Option.attempt (fun _ -> x.DisplayName) with
            | Some shortDisplayName when not (shortDisplayName.Contains ".") ->
                Some (fullName |> Array.replace (fullName.Length - 1) shortDisplayName)
            | _ -> Some fullName
        | None -> None
        |> Option.map (fun fullDisplayName -> String.Join (".", fullDisplayName))

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

let isInterfaceOrAbstractClass (entity: FSharpEntity) =
    entity.IsInterface || hasAttribute<AbstractClassAttribute> entity.Attributes

let (|Attribute|_|) (entity: FSharpEntity) =
    let isAttribute (entity: FSharpEntity) =
        let getBaseType (entity: FSharpEntity) =
            try 
                match entity.BaseType with
                | Some (TypeWithDefinition def) -> Some def
                | _ -> None
            with _ -> None

        let rec isAttributeType (ty: FSharpEntity option) =
            match ty with
            | None -> false
            | Some ty ->
                match ty.TryGetFullName() with
                | None -> false
                | Some fullName ->
                    fullName = "System.Attribute" || isAttributeType (getBaseType ty)
        isAttributeType (Some entity)
    if isAttribute entity then Some() else None

let (|ValueType|_|) (e: FSharpEntity) =
    if e.IsEnum || e.IsValueType || hasAttribute<MeasureAnnotatedAbbreviationAttribute> e.Attributes then Some()
    else None

let (|Class|_|) (original: FSharpEntity, abbreviated: FSharpEntity, _) = 
    if abbreviated.IsClass 
       && (not abbreviated.IsStaticInstantiation || original.IsFSharpAbbreviation) then Some()
    else None 

let (|Record|_|) (e: FSharpEntity) = if e.IsFSharpRecord then Some() else None
let (|UnionType|_|) (e: FSharpEntity) = if e.IsFSharpUnion then Some() else None
let (|Delegate|_|) (e: FSharpEntity) = if e.IsDelegate then Some() else None
let (|FSharpException|_|) (e: FSharpEntity) = if e.IsFSharpExceptionDeclaration then Some() else None

let (|Interface|_|) (e: FSharpEntity) = if e.IsInterface then Some() else None
        
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
let (|FSharpModule|_|) (entity: FSharpEntity) = if entity.IsFSharpModule then Some() else None

let (|Tuple|_|) (ty: FSharpType option) = 
    ty |> Option.bind (fun ty -> if ty.IsTupleType then Some() else None)

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

/// Entity (originalEntity, abbreviatedEntity, abbreviatedType)
let (|Entity|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity -> 
        let abbreviatedEntity, abbreviatedType = getEntityAbbreviatedType entity
        Some (entity, abbreviatedEntity, abbreviatedType)
    | _ -> None

let (|Parameter|_|) (symbol: FSharpSymbol) = 
    match symbol with
    | :? FSharpParameter -> Some()
    | _ -> None

let (|UnionCase|_|) (e: FSharpSymbol) = 
    match e with
    | :? FSharpUnionCase as uc -> Some uc
    | _ -> None

let (|RecordField|_|) (e: FSharpSymbol) =
    match e with
    | :? FSharpField as field ->
        if field.DeclaringEntity.IsFSharpRecord then Some field else None
    | _ -> None

let (|ActivePatternCase|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpActivePatternCase as case -> Some case
    | _ -> None

/// Func (memberFunctionOrValue, fullType)
let (|MemberFunctionOrValue|_|) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpMemberFunctionOrValue as func -> Some func
    | _ -> None

/// Constructor (enclosingEntity)
let (|Constructor|_|) (func: FSharpMemberFunctionOrValue) =
    if func.CompiledName = ".ctor" then Some func.EnclosingEntity
    else None

let (|Function|_|) excluded (func: FSharpMemberFunctionOrValue) =
    match func.FullTypeSafe with
    | Some typ when typ.IsFunctionType
                   && not func.IsPropertyGetterMethod 
                   && not func.IsPropertySetterMethod
                   && not excluded
                   && not (isOperator func.DisplayName) -> Some()
    | _ -> None

let (|ExtensionMember|_|) (func: FSharpMemberFunctionOrValue) =
    if func.IsExtensionMember then Some() else None

let (|Event|_|) (func: FSharpMemberFunctionOrValue) =
    if func.IsEvent then Some () else None


