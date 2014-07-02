namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler.SourceCodeServices

type ShortIdent = string
type Idents = ShortIdent[]

type EntityKind =
    | Attribute
    | Type
    | FunctionOrValue
    override x.ToString() = sprintf "%A" x

type RawEntity = 
    { Idents: Idents
      Namespace: Idents option
      IsPublic: bool
      TopRequireQualifiedAccessParent: Idents option
      AutoOpenParent: Idents option
      Kind: EntityKind }
    override x.ToString() = sprintf "%A" x  

type AssemblyPath = string
type AssemblyContentType = Public | Full

type Perent = 
    { Namespace: Idents option
      RequiresQualifiedAccess: Idents option
      AutoOpen: Idents option
      WithModuleSuffix: Idents option }
    static member Empty = 
        { Namespace = None
          RequiresQualifiedAccess = None
          AutoOpen = None
          WithModuleSuffix = None }
           
module AssemblyContentProvider =
    open System
    open System.IO
    open System.Collections.Generic

    let unrepresentedTypes = ["nativeptr"; "ilsigptr"; "[,]"; "[,,]"; "[,,,]"; "[]"]
        
    let rec getFullName (entity: FSharpEntity) =
        match entity with
        | AbbreviatedType (TypeWithDefinition def) -> getFullName def
        | x when x.IsArrayType || x.IsByRef -> None
        | _ ->
            Option.attempt (fun _ -> entity.DisplayName)
            |> Option.bind (fun displayName ->
                if List.exists ((=) displayName) unrepresentedTypes then None
                else 
                    try Some entity.FullName
                    with e -> 
                        //fail "Should add this type to the black list: %O" e
                        None)
            |> Option.map (fun fullName ->
                // remove number of arguments from generic types
                // e.g. System.Collections.Generic.Dictionary`2 -> System.Collections.Generic.Dictionary
                if Char.IsDigit fullName.[fullName.Length - 1] then
                    match fullName.LastIndexOf '`' with
                    | -1 -> fullName
                    | lastBacktickIndex -> 
                        fullName.Substring(0, lastBacktickIndex)
                else fullName) 
            
    let fixParentModuleSuffix (parent: Idents option) (idents: Idents) =
        match parent with
        | Some p when p.Length <= idents.Length -> 
            for i in 0..p.Length - 1 do
                idents.[i] <- p.[i]
        | _ -> ()
        idents

    let getFullNameAsIdents (parentWithModuleSuffix: Idents option) entity = 
        entity |> getFullName |> Option.map (fun x -> x.Split '.')
        |> Option.map (fixParentModuleSuffix parentWithModuleSuffix)

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
                match getFullName ty with
                | None -> false
                | Some fullName ->
                    fullName = "System.Attribute" || isAttributeType (getBaseType ty)
        isAttributeType (Some entity)

    let createEntity (ns, topRequiresQualifiedAccessParent, autoOpenParent, parentWithModuleSuffix, entity: FSharpEntity) =
        getFullNameAsIdents parentWithModuleSuffix entity
        |> Option.map (fun fullName ->
                { Idents = fullName
                  Namespace = ns
                  IsPublic = entity.Accessibility.IsPublic
                  TopRequireQualifiedAccessParent = 
                      topRequiresQualifiedAccessParent |> Option.bind (getFullNameAsIdents parentWithModuleSuffix)
                  AutoOpenParent = autoOpenParent |> Option.bind (getFullNameAsIdents parentWithModuleSuffix)
                  Kind = if isAttribute entity then EntityKind.Attribute else EntityKind.Type })

    let rec traverseEntity contentType (parentNamespace: Idents option) (requiresQualifiedAccessParent: FSharpEntity option) 
                            (autoOpenParent: FSharpEntity option) (parentWithModuleSuffix: Idents option) (entity: FSharpEntity) = 

        seq { if not entity.IsProvided then
                match contentType, entity.Accessibility.IsPublic with
                | Full, _ | Public, true ->
                    let ns = entity.Namespace |> Option.map (fun x -> x.Split '.') |> Option.orElse parentNamespace
                    match createEntity (ns, requiresQualifiedAccessParent, autoOpenParent, 
                                        parentWithModuleSuffix, entity) with
                    | Some x -> yield x
                    | None -> ()
                                            
                    let requiresQualifiedAccessParent =
                        requiresQualifiedAccessParent
                        |> Option.orElse (
                            if hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes then Some entity
                            else None)

                    let autoOpenParent =
                        let isAutoOpen = entity.IsFSharpModule && hasAttribute<AutoOpenAttribute> entity.Attributes

                        match isAutoOpen, autoOpenParent with
                        | true, Some parent -> Some parent // if parent is also AutoOpen, then keep the parent
                        | true, None -> Some entity // if parent is not AutoOpen, but current entity is, peek the latter as a new AutoOpen module
                        | false, _ -> None // if current entity is not AutoOpen, we discard whatever parent was

                    let currentParentWithModuleSuffix =
                        if entity.IsFSharpModule && hasModuleSuffixAttribute entity then 
                            Some entity
                        else None
                        |> Option.bind (getFullNameAsIdents parentWithModuleSuffix)
                        |> Option.map (fun idents -> 
                             if idents.Length > 0 then
                                let lastIdent = idents.[idents.Length - 1]
                                idents.[idents.Length - 1] <- lastIdent.Substring(0, lastIdent.Length - 6)
                             idents)

                    if entity.IsFSharpModule then
                        for func in entity.MembersFunctionsAndValues do
                            let e =
                                    { Idents = func.FullName.Split '.' |> fixParentModuleSuffix currentParentWithModuleSuffix
                                      Namespace = ns
                                      IsPublic = func.Accessibility.IsPublic
                                      TopRequireQualifiedAccessParent = 
                                          requiresQualifiedAccessParent |> Option.bind (getFullNameAsIdents currentParentWithModuleSuffix)
                                      AutoOpenParent = autoOpenParent |> Option.bind (getFullNameAsIdents currentParentWithModuleSuffix)
                                      Kind = EntityKind.FunctionOrValue }
                            yield e

                    for e in (try entity.NestedEntities :> _ seq with _ -> Seq.empty) do
                        yield! traverseEntity contentType ns requiresQualifiedAccessParent autoOpenParent currentParentWithModuleSuffix e 
                | _ -> () }

    let getAssemblySignatureContent contentType (signature: FSharpAssemblySignature) =
            seq { for e in (try signature.Entities :> _ seq with _ -> Seq.empty) do
                    yield! traverseEntity contentType None None None None e }
            |> Seq.distinct
            |> Seq.toList

    let private entityCache = Dictionary<AssemblyPath, DateTime * AssemblyContentType * RawEntity list>()

    let getAssemblyContent contentType (asm: FSharpAssembly) =
        match asm.FileName with
        | Some fileName ->
            let assemblyWriteTime = FileInfo(fileName).LastWriteTime
            match contentType, entityCache.TryGetValue fileName with
            | _, (true, (cacheWriteTime, Full, entities))
            | Public, (true, (cacheWriteTime, _, entities)) when cacheWriteTime = assemblyWriteTime -> 
                debug "[LanguageService] Return entities from %s from cache." fileName
                entities
            | _ ->
                debug "[LanguageService] Getting entities from %s." fileName
                let entities = getAssemblySignatureContent contentType asm.Contents
                entityCache.[fileName] <- (assemblyWriteTime, contentType, entities)
                entities
        | None -> 
            debug "[LanguageService] Getting entities from an assembly with no FileName: %s." asm.QualifiedName
            getAssemblySignatureContent contentType asm.Contents
        |> List.filter (fun entity -> 
            match contentType, entity.IsPublic with
            | Full, _ | Public, true -> true
            | _ -> false)