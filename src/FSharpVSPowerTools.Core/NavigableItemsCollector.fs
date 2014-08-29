namespace FSharpVSPowerTools.Navigation

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast

[<RequireQualifiedAccess>]
type NavigableItemKind = 
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

[<NoComparison; NoEquality>]
type NavigableItem = 
    {
        Name: string
        Range: Range.range
        IsSignature: bool
        Kind: NavigableItemKind
    }

module NavigableItemsCollector =
    
    let rec private lastInLid (lid: LongIdent) = 
        match lid with
        | [x] -> Some x
        | _::xs -> lastInLid xs
        | _ -> None // empty lid is possible in case of broken ast
     
    let collect (parsedInput: ParsedInput) = 
        let result = ResizeArray()
        
        let addIdent kind (id: Ident) (isSignature: bool) = 
            if not (String.IsNullOrEmpty id.idText) then
                let item = { Name = id.idText; Range = id.idRange; IsSignature = isSignature; Kind = kind  }
                result.Add item

        let addModule  lid isSig = 
            match lastInLid lid with
            | Some id -> addIdent NavigableItemKind.Module id isSig
            | _ -> ()

        let addModuleAbbreviation (id: Ident) isSig =
            addIdent NavigableItemKind.ModuleAbbreviation id isSig 
        
        let addExceptionRepr (ExceptionDefnRepr(_attributes, UnionCase(_unionAttributes, id, _type, _unionXmlDoc, _unionAccess, _unionRange), _lid, _xmlDoc, _access, _range)) isSig = 
            addIdent NavigableItemKind.Exception id isSig

        let addComponentInfo kind (ComponentInfo(_attributes, _typeArgs, _constraints, lid, _xmlDoc, _preferPostfix, _access, _componentRange)) isSig = 
            match lastInLid lid with
            | Some id -> addIdent kind id isSig
            | _ -> ()

        let addValSig kind (ValSpfn(_attributes, id, _typeArgDecls, _type, _valInfo, _, _mutable, _xmlDoc, _access, _rhs, _range)) isSig = 
            addIdent kind id isSig
        
        let addField(SynField.Field(_attributes, _isStatic, id, _type, _, _xmlDoc, _access, _fieldRange)) isSig = 
            match id with
            | Some id -> addIdent NavigableItemKind.Field id isSig
            | _ -> ()
        
        let addEnumCase(EnumCase(_attributes, id, _rhs, _xmlDoc, _range)) isSig = 
            addIdent NavigableItemKind.EnumCase id isSig
        
        let addUnionCase(UnionCase(_attributes, id, _type, _xmlDoc, _access, _range)) isSig = 
            addIdent NavigableItemKind.UnionCase id isSig

        let mapMemberKind mk = 
            match mk with
            | MemberKind.ClassConstructor // ?
            | MemberKind.Constructor -> NavigableItemKind.Constructor
            | MemberKind.PropertyGet
            | MemberKind.PropertySet
            | MemberKind.PropertyGetSet -> NavigableItemKind.Property
            | MemberKind.Member -> NavigableItemKind.Member

        let addBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, valData, headPat, _retTy, 
                                _rhs, _bindingRange, _seqPoint)) itemKind =
            let (SynValData(memberFlagsOpt, _valInfo, _idOpt)) = valData
            let kind =
                match itemKind with
                | Some x -> x
                | _ ->
                    match memberFlagsOpt with
                    | Some mf -> mapMemberKind mf.MemberKind
                    | _ -> NavigableItemKind.ModuleValue

            match headPat with
            | SynPat.LongIdent(LongIdentWithDots([_this; id], _ranges), _, _typeArgs, _constructorDecls, _access, _range) ->
                // instance members
                addIdent kind id false
            | SynPat.LongIdent(LongIdentWithDots([id], _ranges), _, _typeArgs, _constructorDecls, _access, _range) ->
                // functions
                addIdent kind id false
            | SynPat.Named(_pat, id, _isThis, _access, _range) ->
                // values
                addIdent kind id false
            | _ -> ()

        let addMember valSig (memberFlags: MemberFlags) isSig = 
            let ctor = mapMemberKind memberFlags.MemberKind
            addValSig ctor valSig isSig

        let rec walkSigFileInput (ParsedSigFileInput(_name, _fileName, _scopedPragmas, _hashDirectives, moduleOrNamespaceList)) = 
            for item in moduleOrNamespaceList do
                walkSynModuleOrNamespaceSig item

        and walkSynModuleOrNamespaceSig (SynModuleOrNamespaceSig(lid, isModule, decls, _xmldoc, _attributes, _access, _range)) = 
            if isModule then
                addModule lid true
            for decl in decls do
                walkSynModuleSigDecl decl

        and walkSynModuleSigDecl(decl: SynModuleSigDecl) = 
            match decl with
            | SynModuleSigDecl.ModuleAbbrev(lhs, _rhs, _range) ->
                addModuleAbbreviation lhs true
            | SynModuleSigDecl.Exception(ExceptionSig(representation, _members, _exceptionRange), _range) ->
                addExceptionRepr representation true
            | SynModuleSigDecl.NamespaceFragment fragment ->
                walkSynModuleOrNamespaceSig fragment
            | SynModuleSigDecl.NestedModule(componentInfo, nestedDecls, _range) ->
                addComponentInfo NavigableItemKind.Module componentInfo true
                for decl in nestedDecls do
                    walkSynModuleSigDecl decl
            | SynModuleSigDecl.Types(types, _range) ->
                for ty in types do
                    walkSynTypeDefnSig ty
            | SynModuleSigDecl.Val(valSig, _range) ->
                addValSig NavigableItemKind.ModuleValue valSig true
            | SynModuleSigDecl.HashDirective _
            | SynModuleSigDecl.Open _ -> ()

        and walkSynTypeDefnSig (TypeDefnSig(componentInfo, repr, members, _range)) = 
            addComponentInfo NavigableItemKind.Type componentInfo true
            for m in members do
                walkSynMemberSig m
            match repr with
            | SynTypeDefnSigRepr.ObjectModel(_kind, membersSigs, _range) ->
                for m in membersSigs do
                    walkSynMemberSig m
            | SynTypeDefnSigRepr.Simple(repr, _range) ->
                walkSynTypeDefnSimpleRepr repr true

        and walkSynMemberSig (synMemberSig: SynMemberSig) = 
            match synMemberSig with
            | SynMemberSig.Member(valSig, memberFlags, _range) ->
                addMember valSig memberFlags true
            | SynMemberSig.ValField(synField, _range) ->
                addField synField true
            | SynMemberSig.NestedType(synTypeDef, _range) ->
                walkSynTypeDefnSig synTypeDef
            | SynMemberSig.Inherit _
            | SynMemberSig.Interface _ -> ()

        and walkImplFileInpit (ParsedImplFileInput(_name, _isScript, _fileName, _scopedPragmas, _hashDirectives, moduleOrNamespaceList, _)) = 
            for item in moduleOrNamespaceList do
                walkSynModuleOrNamespace item

        and walkSynModuleOrNamespace(SynModuleOrNamespace(lid, isModule, decls, _xmldoc, _attributes, _access, _range)) =
            if isModule then
                addModule lid false
            for decl in decls do
                walkSynModuleDecl decl

        and walkSynModuleDecl(decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.Exception(ExceptionDefn(repr, synMembers, _defnRange), _range) -> 
                addExceptionRepr repr false
                for m in synMembers do
                    walkSynMemberDefn m
            | SynModuleDecl.Let(_isRecursive, bindings, _range) ->
                for binding in bindings do
                    addBinding binding None
            | SynModuleDecl.ModuleAbbrev(lhs, _rhs, _range) ->
                addModuleAbbreviation lhs false
            | SynModuleDecl.NamespaceFragment(fragment) ->
                walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(componentInfo, modules, _isContinuing, _range) ->
                addComponentInfo NavigableItemKind.Module componentInfo false
                for m in modules do
                    walkSynModuleDecl m
            | SynModuleDecl.Types(typeDefs, _range) ->
                for t in typeDefs do
                    walkSynTypeDefn t
            | SynModuleDecl.Attributes _
            | SynModuleDecl.DoExpr _
            | SynModuleDecl.HashDirective _
            | SynModuleDecl.Open _ -> ()

        and walkSynTypeDefn(TypeDefn(componentInfo, representation, members, _range)) = 
            addComponentInfo NavigableItemKind.Type componentInfo false
            walkSynTypeDefnRepr representation
            for m in members do
                walkSynMemberDefn m

        and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
                for m in members do
                    walkSynMemberDefn m
            | SynTypeDefnRepr.Simple(repr, _range) -> 
                walkSynTypeDefnSimpleRepr repr false

        and walkSynTypeDefnSimpleRepr(repr: SynTypeDefnSimpleRepr) isSig = 
            match repr with
            | SynTypeDefnSimpleRepr.Enum(enumCases, _range) ->
                for c in enumCases do
                    addEnumCase c isSig
            | SynTypeDefnSimpleRepr.Record(_access, fields, _range) ->
                for f in fields do
                    // TODO: add specific case for record field?
                    addField f isSig
            | SynTypeDefnSimpleRepr.Union(_access, unionCases, _range) ->
                for uc in unionCases do
                    addUnionCase uc isSig
            | SynTypeDefnSimpleRepr.General _
            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _
            | SynTypeDefnSimpleRepr.None _
            | SynTypeDefnSimpleRepr.TypeAbbrev _ -> ()

        and walkSynMemberDefn (memberDefn: SynMemberDefn) =
            match memberDefn with
            | SynMemberDefn.AbstractSlot(synValSig, memberFlags, _range) ->
                addMember synValSig memberFlags false
            | SynMemberDefn.AutoProperty(_attributes, _isStatic, id, _type, _memberKind, _memberFlags, _xmlDoc, _access, _rhs, _r1, _r2) ->
                addIdent NavigableItemKind.Property id false
            | SynMemberDefn.Interface(_interfaceType, members, _range) ->
                match members with
                | Some members ->
                    for m in members do
                        walkSynMemberDefn m
                | None -> ()
            | SynMemberDefn.Member(binding, _range) ->
                addBinding binding None
            | SynMemberDefn.NestedType(typeDef, _access, _range) -> 
                walkSynTypeDefn typeDef
            | SynMemberDefn.ValField(field, _range) ->
                addField field false
            | SynMemberDefn.LetBindings (bindings, _, _, _) -> 
                bindings |> List.iter (fun binding -> addBinding binding (Some NavigableItemKind.Field))
            | SynMemberDefn.Open _
            | SynMemberDefn.ImplicitInherit _
            | SynMemberDefn.Inherit _
            | SynMemberDefn.ImplicitCtor _ -> ()

        match parsedInput with
        | ParsedInput.SigFile input -> walkSigFileInput input
        | ParsedInput.ImplFile input -> walkImplFileInpit input

        result :> seq<_>
