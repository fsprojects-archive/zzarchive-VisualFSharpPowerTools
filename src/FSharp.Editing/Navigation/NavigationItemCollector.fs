﻿namespace FSharp.Editing.Navigation

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

[<RequireQualifiedAccess>]
type NavigationItemKind = 
    | Module
    | ModuleAbbreviation
    | Exception
    | Type
    | ModuleValue
    | Field
    | Property
    | Constructor
    | Member
    | EnumCase
    | UnionCase
    override x.ToString() = sprintf "%+A" x

type NavigationItemPos =
    { Row: int
      Col: int }

type NavigationItemRange =
    { Start: NavigationItemPos
      End: NavigationItemPos }
    static member FromRange (r: Range.range) =
        let _, range01 = Range.toFileZ r
        let (startRow, startCol), (endRow, endCol) = range01
        { Start = { Row = startRow; Col = startCol }
          End = { Row = endRow; Col = endCol }}

type NavigationItem = 
    {
        FilePath: string
        Name: string
        Range: NavigationItemRange
        IsSignature: bool
        Kind: NavigationItemKind
    }

module NavigationItemCollector =
    let rec private lastInLid (lid: LongIdent) = 
        match lid with
        | [x] -> Some x
        | _ :: xs -> lastInLid xs
        | _ -> None // empty lid is possible in case of broken ast
     
    let collect filePath (parsedInput: ParsedInput) = 
        let result = ResizeArray()
        
        let addIdent kind (id: Ident) (isSignature: bool) = 
            if not (String.IsNullOrEmpty id.idText) then
                let item = 
                    { FilePath = filePath
                      Name = id.idText
                      Range = NavigationItemRange.FromRange id.idRange
                      IsSignature = isSignature; Kind = kind  }
                result.Add item

        let addModule  lid isSig = 
            match lastInLid lid with
            | Some id -> addIdent NavigationItemKind.Module id isSig
            | _ -> ()

        let addModuleAbbreviation (id: Ident) isSig =
            addIdent NavigationItemKind.ModuleAbbreviation id isSig 
        
        let addExceptionRepr (SynExceptionDefnRepr(_, UnionCase(_, id, _, _, _, _), _, _, _, _)) isSig = 
            addIdent NavigationItemKind.Exception id isSig

        let addComponentInfo kind (ComponentInfo(_, _, _, lid, _, _, _, _)) isSig = 
            match lastInLid lid with
            | Some id -> addIdent kind id isSig
            | _ -> ()

        let addValSig kind (ValSpfn(_, id, _, _, _, _, _, _, _, _, _)) isSig = 
            addIdent kind id isSig
        
        let addField(SynField.Field(_, _, id, _, _, _, _, _)) isSig = 
            match id with
            | Some id -> addIdent NavigationItemKind.Field id isSig
            | _ -> ()
        
        let addEnumCase(EnumCase(_, id, _, _, _)) isSig = 
            addIdent NavigationItemKind.EnumCase id isSig
        
        let addUnionCase(UnionCase(_, id, _, _, _, _)) isSig = 
            addIdent NavigationItemKind.UnionCase id isSig

        let mapMemberKind mk = 
            match mk with
            | MemberKind.ClassConstructor // ?
            | MemberKind.Constructor -> NavigationItemKind.Constructor
            | MemberKind.PropertyGet
            | MemberKind.PropertySet
            | MemberKind.PropertyGetSet -> NavigationItemKind.Property
            | MemberKind.Member -> NavigationItemKind.Member

        let addBinding (Binding(_, _, _, _, _, _, valData, headPat, _, _, _, _)) itemKind =
            let (SynValData(memberFlagsOpt, _, _)) = valData
            let kind =
                match itemKind with
                | Some x -> x
                | _ ->
                    match memberFlagsOpt with
                    | Some mf -> mapMemberKind mf.MemberKind
                    | _ -> NavigationItemKind.ModuleValue

            match headPat with
            | SynPat.LongIdent(LongIdentWithDots([_; id], _), _, _, _, _access, _) ->
                // instance members
                addIdent kind id false
            | SynPat.LongIdent(LongIdentWithDots([id], _), _, _, _, _, _) ->
                // functions
                addIdent kind id false
            | SynPat.Named(_, id, _, _, _) ->
                // values
                addIdent kind id false
            | _ -> ()

        let addMember valSig (memberFlags: MemberFlags) isSig = 
            let ctor = mapMemberKind memberFlags.MemberKind
            addValSig ctor valSig isSig

        let rec walkSigFileInput (ParsedSigFileInput(_, _, _, _, moduleOrNamespaceList)) = 
            for item in moduleOrNamespaceList do
                walkSynModuleOrNamespaceSig item

        and walkSynModuleOrNamespaceSig (SynModuleOrNamespaceSig(lid, _, isModule, decls, _, _, _, _)) = 
            if isModule then
                addModule lid true
            for decl in decls do
                walkSynModuleSigDecl decl

        and walkSynModuleSigDecl(decl: SynModuleSigDecl) = 
            match decl with
            | SynModuleSigDecl.ModuleAbbrev(lhs, _, _range) ->
                addModuleAbbreviation lhs true
            | SynModuleSigDecl.Exception(SynExceptionSig(representation, _, _), _) ->
                addExceptionRepr representation true
            | SynModuleSigDecl.NamespaceFragment fragment ->
                walkSynModuleOrNamespaceSig fragment
            | SynModuleSigDecl.NestedModule(componentInfo, _, nestedDecls, _) ->
                addComponentInfo NavigationItemKind.Module componentInfo true
                for decl in nestedDecls do
                    walkSynModuleSigDecl decl
            | SynModuleSigDecl.Types(types, _) ->
                for ty in types do
                    walkSynTypeDefnSig ty
            | SynModuleSigDecl.Val(valSig, _range) ->
                addValSig NavigationItemKind.ModuleValue valSig true
            | SynModuleSigDecl.HashDirective _
            | SynModuleSigDecl.Open _ -> ()

        and walkSynTypeDefnSig (TypeDefnSig(componentInfo, repr, members, _)) = 
            addComponentInfo NavigationItemKind.Type componentInfo true
            for m in members do
                walkSynMemberSig m
            match repr with
            | SynTypeDefnSigRepr.ObjectModel(_, membersSigs, _) ->
                for m in membersSigs do
                    walkSynMemberSig m
            | SynTypeDefnSigRepr.Simple(repr, _) ->
                walkSynTypeDefnSimpleRepr repr true
            | SynTypeDefnSigRepr.Exception _ -> ()

        and walkSynMemberSig (synMemberSig: SynMemberSig) = 
            match synMemberSig with
            | SynMemberSig.Member(valSig, memberFlags, _) ->
                addMember valSig memberFlags true
            | SynMemberSig.ValField(synField, _) ->
                addField synField true
            | SynMemberSig.NestedType(synTypeDef, _) ->
                walkSynTypeDefnSig synTypeDef
            | SynMemberSig.Inherit _
            | SynMemberSig.Interface _ -> ()

        and walkImplFileInpit (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            for item in moduleOrNamespaceList do
                walkSynModuleOrNamespace item

        and walkSynModuleOrNamespace(SynModuleOrNamespace(lid, _, isModule, decls, _, _, _, _)) =
            if isModule then
                addModule lid false
            for decl in decls do
                walkSynModuleDecl decl

        and walkSynModuleDecl(decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.Exception(SynExceptionDefn(repr, synMembers, _), _) -> 
                addExceptionRepr repr false
                for m in synMembers do
                    walkSynMemberDefn m
            | SynModuleDecl.Let(_, bindings, _) ->
                for binding in bindings do
                    addBinding binding None
            | SynModuleDecl.ModuleAbbrev(lhs, _, _) ->
                addModuleAbbreviation lhs false
            | SynModuleDecl.NamespaceFragment(fragment) ->
                walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(componentInfo, _, modules, _, _) ->
                addComponentInfo NavigationItemKind.Module componentInfo false
                for m in modules do
                    walkSynModuleDecl m
            | SynModuleDecl.Types(typeDefs, _range) ->
                for t in typeDefs do
                    walkSynTypeDefn t
            | SynModuleDecl.Attributes _
            | SynModuleDecl.DoExpr _
            | SynModuleDecl.HashDirective _
            | SynModuleDecl.Open _ -> ()

        and walkSynTypeDefn(TypeDefn(componentInfo, representation, members, _)) = 
            addComponentInfo NavigationItemKind.Type componentInfo false
            walkSynTypeDefnRepr representation
            for m in members do
                walkSynMemberDefn m

        and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_, members, _) ->
                for m in members do
                    walkSynMemberDefn m
            | SynTypeDefnRepr.Simple(repr, _) -> 
                walkSynTypeDefnSimpleRepr repr false
            | SynTypeDefnRepr.Exception _ -> ()

        and walkSynTypeDefnSimpleRepr(repr: SynTypeDefnSimpleRepr) isSig = 
            match repr with
            | SynTypeDefnSimpleRepr.Enum(enumCases, _) ->
                for c in enumCases do
                    addEnumCase c isSig
            | SynTypeDefnSimpleRepr.Record(_, fields, _) ->
                for f in fields do
                    // TODO: add specific case for record field?
                    addField f isSig
            | SynTypeDefnSimpleRepr.Union(_, unionCases, _) ->
                for uc in unionCases do
                    addUnionCase uc isSig
            | SynTypeDefnSimpleRepr.General _
            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _
            | SynTypeDefnSimpleRepr.None _
            | SynTypeDefnSimpleRepr.TypeAbbrev _
            | SynTypeDefnSimpleRepr.Exception _ -> ()

        and walkSynMemberDefn (memberDefn: SynMemberDefn) =
            match memberDefn with
            | SynMemberDefn.AbstractSlot(synValSig, memberFlags, _) ->
                addMember synValSig memberFlags false
            | SynMemberDefn.AutoProperty(_, _, id, _, _, _, _, _, _, _, _) ->
                addIdent NavigationItemKind.Property id false
            | SynMemberDefn.Interface(_, members, _) ->
                match members with
                | Some members ->
                    for m in members do
                        walkSynMemberDefn m
                | None -> ()
            | SynMemberDefn.Member(binding, _) ->
                addBinding binding None
            | SynMemberDefn.NestedType(typeDef, _, _) -> 
                walkSynTypeDefn typeDef
            | SynMemberDefn.ValField(field, _) ->
                addField field false
            | SynMemberDefn.LetBindings (bindings, _, _, _) -> 
                bindings |> List.iter (fun binding -> addBinding binding (Some NavigationItemKind.Field))
            | SynMemberDefn.Open _
            | SynMemberDefn.ImplicitInherit _
            | SynMemberDefn.Inherit _
            | SynMemberDefn.ImplicitCtor _ -> ()

        match parsedInput with
        | ParsedInput.SigFile input -> walkSigFileInput input
        | ParsedInput.ImplFile input -> walkImplFileInpit input

        result :> seq<_>
