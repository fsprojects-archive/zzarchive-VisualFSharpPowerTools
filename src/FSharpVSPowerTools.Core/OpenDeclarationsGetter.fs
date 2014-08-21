namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

type RawOpenDeclaration =
    { Idents: Idents
      Parent: Idents option }

type OpenDeclWithAutoOpens =
    { Declarations: Idents list
      Parent: Idents option
      IsUsed: bool }

[<NoComparison>]
type OpenDeclaration =
    { Declarations: OpenDeclWithAutoOpens list
      DeclarationRange: Range.range
      ScopeRange: Range.range
      IsUsed: bool }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OpenDeclWithAutoOpens =
    let updateBySymbolPrefix (symbolPrefix: Idents) (decl: OpenDeclWithAutoOpens) =
        let matched = decl.Declarations |> List.exists ((=) symbolPrefix)
        if not decl.IsUsed && matched then debug "OpenDeclarationWithAutoOpens %A is used by %A" decl symbolPrefix
        matched, { decl with IsUsed = decl.IsUsed || matched }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OpenDeclaration =
    let updateBySymbolPrefix (symbolPrefix: Idents) (decl: OpenDeclaration) =
        let decls =
            decl.Declarations 
            |> List.map (OpenDeclWithAutoOpens.updateBySymbolPrefix symbolPrefix)
        let matched = decls |> List.exists fst
        let isUsed = decls |> List.exists (fun (_, decl) -> decl.IsUsed)
        if not decl.IsUsed && isUsed then debug "OpenDeclaration %A is used by %A" decl symbolPrefix
        matched, { decl with Declarations = decls |> List.map snd; IsUsed = isUsed }

module OpenDeclarationGetter =
    open UntypedAstUtils
    open System 
    open System.Diagnostics

    let getAutoOpenModules entities =
        entities 
        |> List.filter (fun e -> 
             match e.Kind with
             | EntityKind.Module { IsAutoOpen = true } -> true
             | _ -> false)
        |> List.map (fun e -> e.CleanedIdents)

    let parseTooltip (ToolTipText elems): RawOpenDeclaration list =
        elems
        |> List.map (fun e -> 
            let rawStrings =
                match e with
                | ToolTipElement.ToolTipElement (s, _) -> [s]
                | ToolTipElement.ToolTipElementGroup elems -> 
                    elems |> List.map fst
                | _ -> []
            
            let removePrefix prefix (str: string) =
                if str.StartsWith prefix then Some (str.Substring(prefix.Length).Trim()) else None

            (* Tooltip at this point is a bunch of lines. One or two lines corespond to each open statement.
               If an open statement is fully qualified, then a single line corresponds to it:
               "module My.Module" or "namespace My.Namespace" (for namespaces).
               If it's a relative module open statement, then two lines correspond to it:
               "module Module"
               "from My".
               If a relative module open statement is actually opens several modules (being a suffix of several parent 
               modules / namespaces), then several "module/namespace + from" line pairs correspont to it:
               "module Module"
               "from My"
               "module Module"
               "from My2"
            *)
            rawStrings
            |> List.choose (fun (s: string) ->
                 maybe {
                    let! name, from = 
                        match s.Split ([|'\n'|], StringSplitOptions.RemoveEmptyEntries) with
                        | [|name; from|] -> Some (name, Some from)
                        | [|name|] -> Some (name, None)
                        | _ -> None

                    let! name = 
                        name 
                        |> removePrefix "namespace"
                        |> Option.orElse (name |> removePrefix "module")
                     
                    let from = from |> Option.bind (removePrefix "from")
                    let fullName = (from |> Option.map (fun from -> from + ".") |> Option.getOrElse "") + name
                    return { RawOpenDeclaration.Idents = fullName.Split '.'
                             Parent = from |> Option.map (fun from -> from.Split '.') }
                }))
        |> List.concat

    let updateOpenDeclsWithSymbolPrefix symbolPrefix symbolRange openDecls = 
        openDecls 
        |> List.fold (fun (acc, foundMatchedDecl) openDecl -> 
            // We already found a matched open declaration or the symbol range is out or the next open declaration.
            // Do nothing, just add the open decl to the accumulator as is.
            if foundMatchedDecl || not (Range.rangeContainsRange openDecl.ScopeRange symbolRange) then
                openDecl :: acc, foundMatchedDecl
            else
                let matched, updatedDecl = openDecl |> OpenDeclaration.updateBySymbolPrefix symbolPrefix
                updatedDecl :: acc, matched
            ) ([], false)
        |> fst
        |> List.rev

    let setOpenDeclsIsUsedFlag idents (openDecls: OpenDeclaration list) =
        openDecls
        |> List.map (fun decl -> 
            let declarations = 
                decl.Declarations 
                |> List.map (fun d -> 
                    if d.IsUsed then d
                    else { d with IsUsed = d.Declarations |> List.exists ((=) idents) })
            let isUsed = declarations |> List.exists (fun d -> d.IsUsed)
            { decl with Declarations = declarations; IsUsed = isUsed })

    let spreadIsUsedFlagToParents (openDecls: OpenDeclaration list) =
        let res = 
            openDecls 
            |> List.fold (fun res decl ->
                if not decl.IsUsed then res
                else
                    let parents = decl.Declarations |> List.choose (fun decl -> decl.Parent)
                    parents 
                    |> List.fold (fun res parent -> res |> setOpenDeclsIsUsedFlag parent)
                       res
               ) openDecls
        debug "[OpenDeclarationsGetter] spreadIsUsedFlagToParents: Before = %A, After = %A" openDecls res
        res

    type Line = int
    type EndColumn = int

    let getOpenDeclarations (ast: ParsedInput option) (entities: RawEntity list option) 
                            (qualifyOpenDeclarations: Line -> EndColumn -> Idents -> RawOpenDeclaration list) =
        match ast, entities with
        | Some (ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _))), Some entities ->
            let autoOpenModules = getAutoOpenModules entities
            //debug "All AutoOpen modules: %A" autoOpenModules

            let rec walkModuleOrNamespace acc (decls, moduleRange) =
                let openStatements =
                    decls
                    |> List.fold (fun acc -> 
                        function
                        | SynModuleDecl.NestedModule (_, nestedModuleDecls, _, nestedModuleRange) -> 
                            walkModuleOrNamespace acc (nestedModuleDecls, nestedModuleRange)
                        | SynModuleDecl.Open (LongIdentWithDots(longIdent, _), openStatementRange) ->
                            let identArray = 
                                longIdent
                                |> List.mapi (fun i ident -> 
                                    match i, ident.idText with
                                    | 0, "`global`" ->
                                        let r = ident.idRange
                                        // Make sure that we don't filter out ``global`` and the like
                                        if r.StartLine = r.EndLine && r.EndColumn - r.StartColumn = 6 then
                                            None
                                        else Some ident
                                    | _ -> 
                                        Some ident)
                                |> List.choose id
                                |> longIdentToArray
                            let rawOpenDeclarations =  
                                identArray
                                |> qualifyOpenDeclarations openStatementRange.StartLine openStatementRange.EndColumn

                            for openDecl in rawOpenDeclarations do
//                                Debug.Assert (openDecl.Idents |> Array.endsWith identArray, 
//                                                sprintf "%A must be suffix for %A" identArray openDecl.Idents)
                                for ident in openDecl.Idents do
                                    Debug.Assert (IdentifierUtils.isIdentifier ident,
                                                  sprintf "%s as part of %A must be an identifier" ident openDecl.Idents)

                            (* The idea that each open declaration can actually open itself and all direct AutoOpen modules,
                                children AutoOpen modules and so on until a non-AutoOpen module is met.
                                Example:
                                   
                                module M =
                                    [<AutoOpen>]                                  
                                    module A1 =
                                        [<AutoOpen>] 
                                        module A2 =
                                            module A3 = 
                                                [<AutoOpen>] 
                                                module A4 = ...
                                         
                                // this declaration actually open M, M.A1, M.A1.A2, but NOT M.A1.A2.A3 or M.A1.A2.A3.A4
                                open M
                            *)

                            let rec loop acc maxLength =
                                let newModules =
                                    autoOpenModules
                                    |> List.filter (fun autoOpenModule -> 
                                        autoOpenModule.Length = maxLength + 1
                                        && acc |> List.exists (fun collectedAutoOpenModule ->
                                            autoOpenModule |> Array.startsWith collectedAutoOpenModule))
                                match newModules with
                                | [] -> acc
                                | _ -> loop (acc @ newModules) (maxLength + 1)
                                
                            let identsAndAutoOpens = 
                                rawOpenDeclarations
                                |> List.map (fun openDecl -> 
                                        { Declarations = loop [openDecl.Idents] openDecl.Idents.Length 
                                          Parent = openDecl.Parent
                                          IsUsed = false })

                            (* For each module that has ModuleSuffix attribute value we add additional Idents "<Name>Module". For example:
                                   
                                module M =
                                    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                                    module M1 =
                                        module M2 =
                                            let func _ = ()
                                open M.M1.M2
                                The last line will produce two Idents: "M.M1.M2" and "M.M1Module.M2".
                                The reason is that FCS return different FullName for entities declared in ModuleSuffix modules
                                depending on whether the module is in the current project or not. 
                            *)
                            let finalOpenDecls = 
                                identsAndAutoOpens
                                |> Seq.distinct
                                |> Seq.toList

                            { Declarations = finalOpenDecls
                              DeclarationRange = openStatementRange
                              ScopeRange = Range.mkRange openStatementRange.FileName openStatementRange.Start moduleRange.End
                              IsUsed = false } :: acc
                        | _ -> acc) [] 
                openStatements @ acc

            modules
            |> List.fold (fun acc (SynModuleOrNamespace(_, _, decls, _, _, _, moduleRange)) ->
                    walkModuleOrNamespace acc (decls, moduleRange) @ acc) []       
        | _ -> []
