namespace FSharpVSPowerTools

open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal ShortIdent = string
type internal Idents = ShortIdent[]
type IsAutoOpen = bool
type ModuleKind = { IsAutoOpen: bool; HasModuleSuffix: bool }

type EntityKind =
    | Attribute
    | Type
    | FunctionOrValue of isActivePattern:bool
    | Module of ModuleKind
    override x.ToString() = sprintf "%A" x

type RawEntity = 
    { /// Full entity name as it's seen in compiled code (raw FSharpEntity.FullName, FSharpValueOrFunction.FullName). 
      FullName: string
      /// Entity name parts with removed module suffixes (Ns.M1Module.M2Module.M3.entity -> Ns.M1.M2.M3.entity)
      /// and replaced compiled names with display names (FSharpEntity.DisplayName, FSharpValueOrFucntion.DisplayName).
      /// Note: *all* parts are cleaned, not the last one. 
      CleanedIdents: Idents
      Namespace: Idents option
      IsPublic: bool
      TopRequireQualifiedAccessParent: Idents option
      AutoOpenParent: Idents option
      Kind: EntityKind }
    override x.ToString() = sprintf "%A" x  

type AssemblyPath = string
type AssemblyContentType = Public | Full

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
    
    member x.FixParentModuleSuffix (idents: Idents) =
        Parent.RewriteParentIdents x.WithModuleSuffix idents

    member __.FormatEntityFullName (entity: FSharpEntity) =
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

        let removeModuleSuffix (idents: Idents) =
            if entity.IsFSharpModule && idents.Length > 0 && hasModuleSuffixAttribute entity then
                let lastIdent = idents.[idents.Length - 1]
                if lastIdent.EndsWith "Module" then
                    idents |> Array.replace (idents.Length - 1) (lastIdent.Substring(0, lastIdent.Length - 6))
                else idents
            else idents

        entity.TryGetFullName()
        |> Option.bind (fun fullName -> 
            entity.TryGetFullDisplayName()
            |> Option.map (fun fullDisplayName ->
                fullName,
                fullDisplayName.Split '.' 
                |> removeGenericParamsCount 
                |> removeModuleSuffix))

module AssemblyContentProvider =
    open System.IO
    open System.Collections.Generic

    let private createEntity ns (parent: Parent) (entity: FSharpEntity) =
        parent.FormatEntityFullName entity
        |> Option.map (fun (fullName, cleanIdents) ->
            { FullName = fullName
              CleanedIdents = cleanIdents
              Namespace = ns
              IsPublic = entity.Accessibility.IsPublic
              TopRequireQualifiedAccessParent = parent.RequiresQualifiedAccess |> Option.map parent.FixParentModuleSuffix
              AutoOpenParent = parent.AutoOpen |> Option.map parent.FixParentModuleSuffix
              Kind = 
                match entity with
                | TypedAstPatterns.Attribute -> EntityKind.Attribute 
                | FSharpModule ->
                    EntityKind.Module 
                        { IsAutoOpen = hasAttribute<AutoOpenAttribute> entity.Attributes
                          HasModuleSuffix = hasModuleSuffixAttribute entity }
                | _ -> EntityKind.Type })

    let rec private traverseEntity contentType (parent: Parent) (entity: FSharpEntity) = 

        seq { if not entity.IsProvided then
                match contentType, entity.Accessibility.IsPublic with
                | Full, _ | Public, true ->
                    let ns = entity.Namespace |> Option.map (fun x -> x.Split '.') |> Option.orElse parent.Namespace

                    let currentEntity = createEntity ns parent entity
                    match currentEntity with
                    | Some x -> yield x
                    | None -> ()

                    let currentParent =
                        { RequiresQualifiedAccess =
                            parent.RequiresQualifiedAccess
                            |> Option.orElse (
                                if hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes then 
                                    parent.FormatEntityFullName entity |> Option.map snd
                                else None)
                          AutoOpen =
                            let isAutoOpen = entity.IsFSharpModule && hasAttribute<AutoOpenAttribute> entity.Attributes
                            match isAutoOpen, parent.AutoOpen with
                            // if parent is also AutoOpen, then keep the parent
                            | true, Some parent -> Some parent 
                            // if parent is not AutoOpen, but current entity is, peek the latter as a new AutoOpen module
                            | true, None -> parent.FormatEntityFullName entity |> Option.map snd
                            // if current entity is not AutoOpen, we discard whatever parent was
                            | false, _ -> None 

                          WithModuleSuffix = 
                            if entity.IsFSharpModule && hasModuleSuffixAttribute entity then 
                                currentEntity |> Option.map (fun e -> e.CleanedIdents) 
                            else parent.WithModuleSuffix
                          Namespace = ns }

                    if entity.IsFSharpModule then
                        for func in entity.MembersFunctionsAndValues do
                            match func.TryGetFullDisplayName() with
                            | Some displayName ->
                                if displayName = "SourceCodeClassifierTests.M.( |Pattern|_| )" then
                                    debug "!!!"
                                
                                let fullNameAndIdents =
                                    let rawIdents = displayName.Split '.'

                                    if false && func.IsActivePattern then
                                        func.CompiledName.Split([|'|'|], StringSplitOptions.RemoveEmptyEntries)
                                        |> Array.filter ((<>) "_")
                                        |> Array.map (fun patternCase ->
                                             let idents = Array.append rawIdents [| patternCase |]
                                             idents |> String.concat ".", idents)
                                    else [| func.FullName, rawIdents |]
                                
                                //let fullName = func.GetFullCompiledNameIdents()

                                yield!
                                    fullNameAndIdents
                                    |> Array.map (fun (fullName, cleanIdents) ->
                                        { FullName = fullName
                                          CleanedIdents = currentParent.FixParentModuleSuffix cleanIdents
                                          Namespace = ns
                                          IsPublic = func.Accessibility.IsPublic
                                          TopRequireQualifiedAccessParent = 
                                              currentParent.RequiresQualifiedAccess |> Option.map currentParent.FixParentModuleSuffix
                                          AutoOpenParent = currentParent.AutoOpen |> Option.map currentParent.FixParentModuleSuffix
                                          Kind = EntityKind.FunctionOrValue func.IsActivePattern })
                            | None -> ()

                    for e in (try entity.NestedEntities :> _ seq with _ -> Seq.empty) do
                        yield! traverseEntity contentType currentParent e 
                | _ -> () }

    let getAssemblySignatureContent contentType (signature: FSharpAssemblySignature) =
            signature.TryGetEntities()
            |> Seq.map (traverseEntity contentType Parent.Empty)
            |> Seq.concat
            |> Seq.distinct
            |> Seq.toList

    let private getAssemblySignaturesContent contentType (assemblies: FSharpAssembly list) = 
        assemblies 
        |> List.map (fun asm -> getAssemblySignatureContent contentType asm.Contents)
        |> Seq.concat 
        |> Seq.toList

    let private entityCache = Dictionary<AssemblyPath, DateTime * AssemblyContentType * RawEntity list>()

    let getAssemblyContent contentType (fileName: string option) (assemblies: FSharpAssembly list) =
        match fileName with
        | Some fileName ->
            let assemblyWriteTime = FileInfo(fileName).LastWriteTime
            match contentType, entityCache.TryGetValue fileName with
            | _, (true, (cacheWriteTime, Full, entities))
            | Public, (true, (cacheWriteTime, _, entities)) when cacheWriteTime = assemblyWriteTime -> 
                //debug "[AssemblyContentProvider] Return entities from %s from cache." fileName
                entities
            | _ ->
                //debug "[AssemblyContentProvider] Getting entities from %s." fileName
                let entities = getAssemblySignaturesContent contentType assemblies
                entityCache.[fileName] <- (assemblyWriteTime, contentType, entities)
                entities
        | None -> 
            //debug "[AssemblyContentProvider] Getting entities from an assembly with no FileName: %s." asm.QualifiedName
            getAssemblySignaturesContent contentType assemblies
        |> List.filter (fun entity -> 
            match contentType, entity.IsPublic with
            | Full, _ | Public, true -> true
            | _ -> false)