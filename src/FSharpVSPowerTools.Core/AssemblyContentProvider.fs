namespace FSharpVSPowerTools

open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal ShortIdent = string
type internal Idents = ShortIdent[]

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

[<AutoOpen>]
module Extensions =
    let private unrepresentedTypes = ["nativeptr"; "ilsigptr"; "[,]"; "[,,]"; "[,,,]"; "[]"]
    
    type FSharpEntity with
        member x.GetFullName() =
            match x with
            | AbbreviatedType (TypeWithDefinition def) -> def.GetFullName()
            | x when x.IsArrayType || x.IsByRef -> None
            | _ ->
                Option.attempt (fun _ -> x.DisplayName)
                |> Option.bind (fun displayName ->
                    if List.exists ((=) displayName) unrepresentedTypes then None
                    else 
                        try Some x.FullName
                        with e -> 
                            //fail "Should add this type to the black list: %O" e
                            None)

type Parent = 
    { Namespace: Idents option
      RequiresQualifiedAccess: Idents option
      AutoOpen: Idents option
      WithModuleSuffix: Idents option }
    static member Empty = 
        { Namespace = None
          RequiresQualifiedAccess = None
          AutoOpen = None
          WithModuleSuffix = None }
    static member RewriteParentIdents (parentIdents: Idents option) (idents: Idents) =
        match parentIdents with
        | Some p when p.Length <= idents.Length -> 
            for i in 0..p.Length - 1 do
                idents.[i] <- p.[i]
        | _ -> ()
        idents
    member private x.FixParentModuleSuffix (idents: Idents) =
        Parent.RewriteParentIdents x.WithModuleSuffix idents
    member x.FormatIdents (idents: Idents) = x.FixParentModuleSuffix idents
    member x.FormatEntityFullName (entity: FSharpEntity) =
        // remove number of arguments from generic types
        // e.g. System.Collections.Generic.Dictionary`2 -> System.Collections.Generic.Dictionary
        // and System.Data.Listeners`1.Func -> System.Data.Listeners.Func
        let removeGenericParamsCount (idents: Idents) =
            idents 
            |> Array.map (fun ident ->
                if ident.Length > 0 && Char.IsDigit ident.[ident.Length - 1] then
                    let lastBacktickIndex = ident.LastIndexOf '`' 
                    if lastBacktickIndex <> -1 then
                        ident.Substring(0, lastBacktickIndex)
                    else ident
                else ident)

        entity.GetFullName()
        |> Option.map (fun x -> removeGenericParamsCount (x.Split '.'))
        |> Option.map x.FixParentModuleSuffix

module AssemblyContentProvider =
    open System.IO
    open System.Collections.Generic
            
    let fixParentModuleSuffix (parent: Idents option) (idents: Idents) =
        match parent with
        | Some p when p.Length <= idents.Length -> 
            idents 
            |> Array.mapi (fun i ident -> 
                if i < p.Length then p.[i] else ident)
        | _ -> idents

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
                match ty.GetFullName() with
                | None -> false
                | Some fullName ->
                    fullName = "System.Attribute" || isAttributeType (getBaseType ty)
        isAttributeType (Some entity)

    let createEntity ns (parent: Parent) (entity: FSharpEntity) =
        parent.FormatEntityFullName entity
        |> Option.map (fun fullName ->
            { Idents = fullName
              Namespace = ns
              IsPublic = entity.Accessibility.IsPublic
              TopRequireQualifiedAccessParent = parent.RequiresQualifiedAccess |> Option.map parent.FormatIdents
              AutoOpenParent = parent.AutoOpen |> Option.map parent.FormatIdents
              Kind = if isAttribute entity then EntityKind.Attribute else EntityKind.Type })

    let rec traverseEntity contentType (parent: Parent) (entity: FSharpEntity) = 

        seq { if not entity.IsProvided then
                match contentType, entity.Accessibility.IsPublic with
                | Full, _ | Public, true ->
                    let ns = entity.Namespace |> Option.map (fun x -> x.Split '.') |> Option.orElse parent.Namespace

                    if not entity.IsFSharpModule then
                        match createEntity ns parent entity with
                        | Some x -> yield x
                        | None -> ()

                    let parentWithModuleSuffix =
                        if entity.IsFSharpModule && hasModuleSuffixAttribute entity then 
                            Some entity
                        else None
                        |> Option.bind parent.FormatEntityFullName
                        |> Option.map (fun idents -> 
                             if idents.Length > 0 then
                                 let lastIdent = idents.[idents.Length - 1]
                                 if lastIdent.EndsWith "Module" then
                                    idents.[idents.Length - 1] <- lastIdent.Substring(0, lastIdent.Length - 6)
                             idents)

                    let fixModuleSuffix = Parent.RewriteParentIdents parentWithModuleSuffix
                    
                    let currentParent =
                        { RequiresQualifiedAccess =
                            parent.RequiresQualifiedAccess
                            |> Option.orElse (
                                if hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes then 
                                    parent.FormatEntityFullName entity |> Option.map fixModuleSuffix
                                else None)
                          AutoOpen =
                            let isAutoOpen = entity.IsFSharpModule && hasAttribute<AutoOpenAttribute> entity.Attributes
                            match isAutoOpen, parent.AutoOpen with
                            // if parent is also AutoOpen, then keep the parent
                            | true, Some parent -> Some parent 
                            // if parent is not AutoOpen, but current entity is, peek the latter as a new AutoOpen module
                            | true, None -> 
                                parent.FormatEntityFullName entity |> Option.map fixModuleSuffix 
                            // if current entity is not AutoOpen, we discard whatever parent was
                            | false, _ -> None 

                          WithModuleSuffix = parentWithModuleSuffix |> Option.orElse parent.WithModuleSuffix
                          Namespace = ns }

                    if entity.IsFSharpModule then
                        for func in entity.MembersFunctionsAndValues do
                            let e =
                                    { Idents = func.FullName.Split '.' |> currentParent.FormatIdents
                                      Namespace = ns
                                      IsPublic = func.Accessibility.IsPublic
                                      TopRequireQualifiedAccessParent = 
                                          currentParent.RequiresQualifiedAccess |> Option.map currentParent.FormatIdents
                                      AutoOpenParent = currentParent.AutoOpen |> Option.map currentParent.FormatIdents
                                      Kind = EntityKind.FunctionOrValue }
                            yield e

                    for e in (try entity.NestedEntities :> _ seq with _ -> Seq.empty) do
                        yield! traverseEntity contentType currentParent e 
                | _ -> () }

    let getAssemblySignatureContent contentType (signature: FSharpAssemblySignature) =
            seq { for e in (try signature.Entities :> _ seq with _ -> Seq.empty) do
                    yield! traverseEntity contentType Parent.Empty e }
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
                debug "[AssemblyContentProvider] Return entities from %s from cache." fileName
                entities
            | _ ->
                debug "[AssemblyContentProvider] Getting entities from %s." fileName
                let entities = getAssemblySignatureContent contentType asm.Contents
                entityCache.[fileName] <- (assemblyWriteTime, contentType, entities)
                entities
        | None -> 
            debug "[AssemblyContentProvider] Getting entities from an assembly with no FileName: %s." asm.QualifiedName
            getAssemblySignatureContent contentType asm.Contents
        |> List.filter (fun entity -> 
            match contentType, entity.IsPublic with
            | Full, _ | Public, true -> true
            | _ -> false)