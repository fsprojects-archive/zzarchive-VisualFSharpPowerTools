module FSharpVSPowerTools.UntypedAstUtils

open Microsoft.FSharp.Compiler.Ast
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

type internal ShortIdent = string
type internal Idents = ShortIdent[]

let internal longIdentToArray (longIdent: LongIdent): Idents =
    longIdent |> Seq.map string |> Seq.toArray

    /// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
let rec (|Sequentials|_|) = function
    | SynExpr.Sequential(_, _, e, Sequentials es, _) ->
        Some(e::es)
    | SynExpr.Sequential(_, _, e1, e2, _) ->
        Some [e1; e2]
    | _ -> None

let (|ConstructorPats|) = function
    | SynConstructorArgs.Pats ps -> ps
    | SynConstructorArgs.NamePatPairs(xs, _) -> List.map snd xs

/// Returns all Idents and LongIdents found in an untyped AST.
let internal getLongIdents (input: ParsedInput option) : IDictionary<Range.pos, Idents> =
    let identsByEndPos = Dictionary<Range.pos, Idents>()

    let addLongIdent (longIdent: LongIdent) =
        let idents = longIdentToArray longIdent
        for ident in longIdent do
            identsByEndPos.[ident.idRange.End] <- idents

    let addLongIdentWithDots (LongIdentWithDots (longIdent, lids) as value) = 
        match longIdentToArray longIdent with
        | [||] -> ()
        | [|_|] as idents -> identsByEndPos.[value.Range.End] <- idents
        | idents ->
            for dotRange in lids do 
                identsByEndPos.[Range.mkPos dotRange.EndLine (dotRange.EndColumn - 1)] <- idents
            identsByEndPos.[value.Range.End] <- idents
    
    let addIdent (ident: Ident) = 
        identsByEndPos.[ident.idRange.End] <- [|ident.idText|]

    let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
        List.iter walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, decls, _, attrs, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkSynModuleDecl decls

    and walkAttribute (attr: SynAttribute) = 
        addLongIdentWithDots attr.TypeName 
        walkExpr attr.ArgExpr

    and walkTyparDecl (SynTyparDecl.TyparDecl (attrs, typar)) = 
        List.iter walkAttribute attrs
        walkTypar typar
            
    and walkTypeConstraint = function
        | SynTypeConstraint.WhereTyparDefaultsToType (t1, t2, _) -> 
            walkTypar t1
            walkType t2
        | SynTypeConstraint.WhereTyparIsValueType(t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparIsReferenceType(t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparIsUnmanaged(t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparSupportsNull (t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparIsComparable(t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparIsEquatable(t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparSubtypeOfType(t, ty, _) -> 
            walkTypar t
            walkType ty
        | SynTypeConstraint.WhereTyparSupportsMember(ts, sign, _) -> 
            List.iter walkTypar ts 
            walkMemberSig sign
        | SynTypeConstraint.WhereTyparIsEnum(t, ts, _) -> 
            walkTypar t
            List.iter walkType ts
        | SynTypeConstraint.WhereTyparIsDelegate(t, ts, _) -> 
            walkTypar t
            List.iter walkType ts

    and walkPat = function
        | SynPat.Ands (pats, _) -> List.iter walkPat pats
        | SynPat.Named (pat, ident, _, _, _) -> 
            walkPat pat
            addIdent ident
        | SynPat.Typed(pat, t, _) -> 
            walkPat pat
            walkType t
        | SynPat.Attrib(pat, attrs, _) -> 
            walkPat pat
            List.iter walkAttribute attrs
        | SynPat.Or(pat1, pat2, _) -> List.iter walkPat [pat1; pat2]
        | SynPat.LongIdent(ident, _, typars, ConstructorPats pats, _, _) -> 
            addLongIdentWithDots ident
            typars
            |> Option.iter (fun (SynValTyparDecls (typars, _, constraints)) ->
                 List.iter walkTyparDecl typars
                 List.iter walkTypeConstraint constraints)
            List.iter walkPat pats
        | SynPat.Tuple(pats, _) -> List.iter walkPat pats
        | SynPat.Paren(pat, _) -> walkPat pat
        | SynPat.ArrayOrList(_, pats, _) -> List.iter walkPat pats
        | SynPat.IsInst(t, _) -> walkType t
        | SynPat.QuoteExpr(e, _) -> walkExpr e
        | _ -> ()

    and walkTypar (Typar (_, _, _)) = ()

    and walkBinding (SynBinding.Binding(_, _, _, _, attrs, _, _, pat, returnInfo, e, _, _)) =
        List.iter walkAttribute attrs
        walkPat pat
        walkExpr e
        returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

    and walkInterfaceImpl (InterfaceImpl(_, bindings, _)) = List.iter walkBinding bindings

    and walkIndexerArg = function
        | SynIndexerArg.One e -> walkExpr e
        | SynIndexerArg.Two (e1, e2) -> List.iter walkExpr [e1; e2]

    and walkType = function
        | SynType.LongIdent ident -> addLongIdentWithDots ident
        | SynType.App(ty, _, types, _, _, _, _) -> 
            walkType ty
            List.iter walkType types
        | SynType.LongIdentApp(_, _, _, types, _, _, _) -> List.iter walkType types
        | SynType.Tuple(ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
        | SynType.Array(_, t, _) -> walkType t
        | SynType.Fun(t1, t2, _) -> 
            walkType t1
            walkType t2
        | SynType.WithGlobalConstraints(t, typeConstraints, _) -> 
            walkType t
            List.iter walkTypeConstraint typeConstraints
        | SynType.HashConstraint(t, _) -> walkType t
        | SynType.MeasureDivide(t1, t2, _) -> 
            walkType t1
            walkType t2
        | SynType.MeasurePower(t, _, _) -> walkType t
        | _ -> ()

    and walkClause (Clause(pat, e1, e2, _, _)) =
        walkPat pat 
        walkExpr e2
        e1 |> Option.iter walkExpr

    and walkExpr = function
        | SynExpr.LongIdent (_, ident, _, _) -> addLongIdentWithDots ident
        | SynExpr.Ident ident -> addIdent ident
        | SynExpr.Paren (e, _, _, _) -> walkExpr e
        | SynExpr.Quote(_, _, e, _, _) -> walkExpr e
        | SynExpr.Typed(e, _, _) -> walkExpr e
        | SynExpr.Tuple(es, _, _) -> List.iter walkExpr es
        | SynExpr.ArrayOrList(_, es, _) -> List.iter walkExpr es
        | SynExpr.Record(_, _, fields, _) -> 
            fields |> List.iter (fun ((ident, _), e, _) -> 
                        addLongIdentWithDots ident
                        e |> Option.iter walkExpr)
        | SynExpr.New(_, t, e, _) -> 
            walkExpr e
            walkType t
        | SynExpr.ObjExpr(ty, argOpt, bindings, ifaces, _, _) -> 
            argOpt |> Option.iter (fun (e, ident) -> 
                walkExpr e
                ident |> Option.iter addIdent)
            walkType ty
            List.iter walkBinding bindings
            List.iter walkInterfaceImpl ifaces
        | SynExpr.While(_, e1, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.For(_, ident, e1, _, e2, e3, _) -> 
            addIdent ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.ForEach(_, _, _, pat, e1, e2, _) -> 
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.ArrayOrListOfSeqExpr(_, e, _) -> walkExpr e
        | SynExpr.CompExpr(_, _, e, _) -> walkExpr e
        | SynExpr.Lambda(_, _, _, e, _) -> walkExpr e
        | SynExpr.MatchLambda(_, _, synMatchClauseList, _, _) -> 
            List.iter walkClause synMatchClauseList
        | SynExpr.Match(_, e, synMatchClauseList, _, _) -> 
            walkExpr e 
            List.iter walkClause synMatchClauseList
        | SynExpr.Do(e, _) -> walkExpr e
        | SynExpr.Assert(e, _) -> walkExpr e
        | SynExpr.App(_, _, e1, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.TypeApp(e, _, tys, _, _, _, _) -> 
            walkExpr e 
            List.iter walkType tys
        | SynExpr.LetOrUse(_, _, bindings, e, _) -> 
            List.iter walkBinding bindings 
            walkExpr e
        | SynExpr.TryWith(e, _, clauses, _, _, _, _) -> 
            List.iter walkClause clauses
            walkExpr e
        | SynExpr.TryFinally(e1, e2, _, _, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.Lazy(e, _) -> walkExpr e
        | Sequentials es -> List.iter walkExpr es
        | SynExpr.IfThenElse(e1, e2, e3, _, _, _, _) -> 
            List.iter walkExpr [e1; e2]
            e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet(ident, e, _) -> 
            addLongIdentWithDots ident
            walkExpr e
        | SynExpr.DotGet(e, _, idents, _) -> 
            addLongIdentWithDots idents
            walkExpr e
        | SynExpr.DotSet(e1, idents, e2, _) -> 
            walkExpr e1
            addLongIdentWithDots idents
            walkExpr e2
        | SynExpr.DotIndexedGet(e, args, _, _) -> 
            walkExpr e
            List.iter walkIndexerArg args
        | SynExpr.DotIndexedSet(e1, args, e2, _, _, _) -> 
            walkExpr e1
            List.iter walkIndexerArg args
            walkExpr e2
        | SynExpr.NamedIndexedPropertySet(ident, e1, e2, _) -> 
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2]
        | SynExpr.DotNamedIndexedPropertySet(e1, ident, e2, e3, _) -> 
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.TypeTest(e, t, _) -> 
            walkExpr e
            walkType t
        | SynExpr.Upcast(e, t, _) -> 
            walkExpr e
            walkType t
        | SynExpr.Downcast(e, t, _) -> 
            walkExpr e
            walkType t
        | SynExpr.InferredUpcast(e, _) -> walkExpr e
        | SynExpr.InferredDowncast(e, _) -> walkExpr e
        | SynExpr.AddressOf(_, e, _, _) -> walkExpr e
        | SynExpr.JoinIn(e1, _, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.YieldOrReturn(_, e, _) -> walkExpr e
        | SynExpr.YieldOrReturnFrom(_, e, _) -> walkExpr e
        | SynExpr.LetOrUseBang(_, _, _, pat, e1, e2, _) -> 
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.DoBang(e, _) -> walkExpr e
        | SynExpr.TraitCall (ts, sign, e, _) ->
            List.iter walkTypar ts 
            walkMemberSig sign
            walkExpr e
        | _ -> ()

    and walkSimplePat = function
        | SynSimplePat.Attrib (pat, attrs, _) ->
            walkSimplePat pat 
            List.iter walkAttribute attrs
        | SynSimplePat.Typed(pat, t, _) ->
            walkSimplePat pat
            walkType t
        | _ -> ()

    and walkField (SynField.Field(attrs, _, _, t, _, _, _, _)) =
        List.iter walkAttribute attrs 
        walkType t

    and walkValSig (SynValSig.ValSpfn(attrs, _, _, t, _, _, _, _, _, _, _)) =
        List.iter walkAttribute attrs 
        walkType t

    and walkMemberSig = function
        | SynMemberSig.Inherit (t, _) -> walkType t
        | SynMemberSig.Member(vs, _, _) -> walkValSig vs
        | SynMemberSig.Interface(t, _) -> walkType t
        | SynMemberSig.ValField(f, _) -> walkField f
        | SynMemberSig.NestedType(SynTypeDefnSig.TypeDefnSig (info, repr, memberSigs, _), _) -> 
            let isTypeExtensionOrAlias = 
                match repr with
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.TyconAbbrev, _, _)
                | SynTypeDefnSigRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
                | _ -> false
            walkComponentInfo isTypeExtensionOrAlias info
            walkTypeDefnSigRepr repr
            List.iter walkMemberSig memberSigs

    and walkMember = function
        | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
        | SynMemberDefn.Member(binding, _) -> walkBinding binding
        | SynMemberDefn.ImplicitCtor(_, attrs, pats, _, _) -> 
            List.iter walkAttribute attrs 
            List.iter walkSimplePat pats
        | SynMemberDefn.ImplicitInherit(t, e, _, _) -> walkType t; walkExpr e
        | SynMemberDefn.LetBindings(bindings, _, _, _) -> List.iter walkBinding bindings
        | SynMemberDefn.Interface(t, members, _) -> 
            walkType t 
            members |> Option.iter (List.iter walkMember)
        | SynMemberDefn.Inherit(t, _, _) -> walkType t
        | SynMemberDefn.ValField(field, _) -> walkField field
        | SynMemberDefn.NestedType(tdef, _, _) -> walkTypeDefn tdef
        | SynMemberDefn.AutoProperty(attrs, _, _, t, _, _, _, _, e, _, _) -> 
            List.iter walkAttribute attrs
            Option.iter walkType t
            walkExpr e
        | _ -> ()

    and walkEnumCase (EnumCase(attrs, _, _, _, _)) = List.iter walkAttribute attrs

    and walkUnionCaseType = function
        | SynUnionCaseType.UnionCaseFields fields -> List.iter walkField fields
        | SynUnionCaseType.UnionCaseFullType(t, _) -> walkType t

    and walkUnionCase (SynUnionCase.UnionCase(attrs, _, t, _, _, _)) = 
        List.iter walkAttribute attrs 
        walkUnionCaseType t

    and walkTypeDefnSimple = function
        | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union(_, cases, _) -> List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record(_, fields, _) -> List.iter walkField fields
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, t, _) -> walkType t
        | _ -> ()

    and walkComponentInfo isTypeExtensionOrAlias (ComponentInfo(attrs, typars, constraints, longIdent, _, _, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkTyparDecl typars
        List.iter walkTypeConstraint constraints
        if isTypeExtensionOrAlias then
            addLongIdent longIdent

    and walkTypeDefnRepr = function
        | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
        | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn

    and walkTypeDefnSigRepr = function
        | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
        | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn

    and walkTypeDefn (TypeDefn (info, repr, members, _)) =
        let isTypeExtensionOrAlias = 
            match repr with
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _)
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
            | _ -> false
        walkComponentInfo isTypeExtensionOrAlias info
        walkTypeDefnRepr repr
        List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
        | SynModuleDecl.NestedModule(info, modules, _, _) ->
            walkComponentInfo false info
            List.iter walkSynModuleDecl modules
        | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
        | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
        | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
        | SynModuleDecl.Attributes (attrs, _) -> List.iter walkAttribute attrs
        | _ -> ()

    match input with 
    | Some (ParsedInput.ImplFile input) -> 
         walkImplFileInput input
    | _ -> ()
    //debug "%A" idents
    identsByEndPos :> _

let getLongIdentAt ast pos =
    let idents = getLongIdents (Some ast)
    match idents.TryGetValue pos with
    | true, idents -> Some idents
    | _ -> None

/// Returns ranges of all quotations found in an untyped AST
let getQuatationRanges ast =
    let quotationRanges = ResizeArray()

    let rec visitExpr = function
        | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpr cond
            visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr 
        | SynExpr.LetOrUse (_, _, bindings, body, _) -> 
            visitBindindgs bindings
            visitExpr body
        | SynExpr.LetOrUseBang (_, _, _, _, rhsExpr, body, _) -> 
            visitExpr rhsExpr
            visitExpr body
        | SynExpr.Quote (_, _isRaw, _quotedExpr, _, range) -> quotationRanges.Add range
        | SynExpr.App (_,_, funcExpr, argExpr, _) -> 
            visitExpr argExpr
            visitExpr funcExpr
        | SynExpr.Lambda (_, _, _, expr, _) -> visitExpr expr
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (_, expr, _) -> expr) |> List.iter visitExpr
        | SynExpr.ArrayOrListOfSeqExpr (_, expr, _) -> visitExpr expr
        | SynExpr.CompExpr (_, _, expr, _) -> visitExpr expr
        | SynExpr.ForEach (_, _, _, _, _, body, _) -> visitExpr body
        | SynExpr.YieldOrReturn (_, expr, _) -> visitExpr expr
        | SynExpr.YieldOrReturnFrom (_, expr, _) -> visitExpr expr
        | SynExpr.Do (expr, _) -> visitExpr expr
        | SynExpr.DoBang (expr, _) -> visitExpr expr
        | SynExpr.Downcast (expr, _, _) -> visitExpr expr
        | SynExpr.For (_, _, _, _, _, expr, _) -> visitExpr expr
        | SynExpr.Lazy (expr, _) -> visitExpr expr
        | SynExpr.Match (_, expr, clauses, _, _) -> 
            visitExpr expr
            visitMatches clauses 
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (_, _, bindings, _, _ , _) -> visitBindindgs bindings
        | SynExpr.Typed (expr, _, _) -> visitExpr expr
        | SynExpr.Paren (expr, _, _, _) -> visitExpr expr
        | Sequentials  es ->
            List.iter visitExpr es
        | SynExpr.LongIdentSet (_, expr, _) -> visitExpr expr
        | SynExpr.Tuple (exprs, _, _) -> 
            for expr in exprs do 
                visitExpr expr
        | SynExpr.TryFinally (expr1, expr2, _, _, _) ->
            visitExpr expr1
            visitExpr expr2
        | SynExpr.TryWith (expr, _, clauses, _, _, _, _) ->
            visitExpr expr
            visitMatches clauses
        | SynExpr.ArrayOrList(_, exprs, _) -> List.iter visitExpr exprs
        | SynExpr.New(_, _, expr, _) -> visitExpr expr
        | SynExpr.While(_, expr1, expr2, _) -> 
            visitExpr expr1
            visitExpr expr2
        | SynExpr.Assert(expr, _) -> visitExpr expr
        | SynExpr.TypeApp(expr, _, _, _, _, _, _) -> visitExpr expr
        | SynExpr.DotSet(_, _, expr, _) -> visitExpr expr
        | SynExpr.DotIndexedSet(_, _, expr, _, _, _) -> visitExpr expr
        | SynExpr.NamedIndexedPropertySet(_, _, expr, _) -> visitExpr expr
        | SynExpr.DotNamedIndexedPropertySet(_, _, _, expr, _) -> visitExpr expr
        | SynExpr.TypeTest(expr, _, _) -> visitExpr expr
        | SynExpr.Upcast(expr, _, _) -> visitExpr expr
        | SynExpr.InferredUpcast(expr, _) -> visitExpr expr
        | SynExpr.InferredDowncast(expr, _) -> visitExpr expr
        | SynExpr.AddressOf(_, expr, _, _) -> visitExpr expr
        | _ -> ()

    and visitBinding (Binding(_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
    and visitBindindgs = List.iter visitBinding

    and visitPattern = function
        | SynPat.QuoteExpr(expr, _) -> visitExpr expr
        | SynPat.Named(pat, _, _, _, _) -> visitPattern pat
        | SynPat.Typed(pat, _, _) -> visitPattern pat
        | SynPat.Or(pat1, pat2, _) -> visitPattern pat1; visitPattern pat2
        | SynPat.Ands(pats, _) -> List.iter visitPattern pats
        | SynPat.LongIdent(_, _, _, ctorArgs, _, _) -> 
            match ctorArgs with
            | SynConstructorArgs.Pats pats -> List.iter visitPattern pats
            | SynConstructorArgs.NamePatPairs(xs, _) -> 
                xs |> List.map snd |> List.iter visitPattern
        | SynPat.Tuple(pats, _) -> List.iter visitPattern pats
        | SynPat.Paren(pat, _) -> visitPattern pat
        | SynPat.ArrayOrList(_, pats, _) -> List.iter visitPattern pats
        | SynPat.Record(xs, _) -> xs |> List.map snd |> List.iter visitPattern
        | _ -> ()

    and visitMatch (SynMatchClause.Clause (pat, _, expr, _, _)) = 
        visitPattern pat
        visitExpr expr

    and visitMatches = List.iter visitMatch
    
    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
        | _ -> () 

    let visitType ty =
        let (SynTypeDefn.TypeDefn (_, repr, defns, _)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, defns, _) ->
            for d in defns do visitMember d
        | _ -> ()
        for d in defns do visitMember d

    let rec visitDeclarations decls = 
        for declaration in decls do
            match declaration with
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> for ty in types do visitType ty
            | SynModuleDecl.NestedModule (_, decls, _, _) -> visitDeclarations decls
            | _ -> ()

    let visitModulesAndNamespaces modulesOrNss =
        for moduleOrNs in modulesOrNss do
            let (SynModuleOrNamespace(_, _, decls, _, _, _, _)) = moduleOrNs
            visitDeclarations decls

    ast 
    |> Option.iter (function
        | ParsedInput.ImplFile implFile ->
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
            visitModulesAndNamespaces modules
        | _ -> ())
    quotationRanges
   
/// Returns all string literal ranges
let internal getStringLiterals ast : Range.range list =
    let result = ResizeArray() 
     
    let visitType ty =
        match ty with
        | SynType.StaticConstant (SynConst.String(_, r), _) -> result.Add r
        | _ -> ()

    let rec visitExpr = function 
        | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpr cond
            visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr 
        | SynExpr.LetOrUse (_, _, bindings, body, _) -> 
            visitBindindgs bindings
            visitExpr body
        | SynExpr.LetOrUseBang (_, _, _, _, rhsExpr, body, _) -> 
            visitExpr rhsExpr
            visitExpr body
        | SynExpr.App (_,_, funcExpr, argExpr, _) -> 
            visitExpr argExpr
            visitExpr funcExpr
        | SynExpr.Lambda (_, _, _, expr, _) -> visitExpr expr
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (_, expr, _) -> expr) |> List.iter visitExpr
        | SynExpr.ArrayOrListOfSeqExpr (_, expr, _) -> visitExpr expr
        | SynExpr.CompExpr (_, _, expr, _) -> visitExpr expr
        | SynExpr.ForEach (_, _, _, _, _, body, _) -> visitExpr body
        | SynExpr.YieldOrReturn (_, expr, _) -> visitExpr expr
        | SynExpr.YieldOrReturnFrom (_, expr, _) -> visitExpr expr
        | SynExpr.Do (expr, _) -> visitExpr expr
        | SynExpr.DoBang (expr, _) -> visitExpr expr
        | SynExpr.Downcast (expr, _, _) -> visitExpr expr
        | SynExpr.For (_, _, _, _, _, expr, _) -> visitExpr expr
        | SynExpr.Lazy (expr, _) -> visitExpr expr
        | SynExpr.Match (_, expr, clauses, _, _) -> 
            visitExpr expr
            visitMatches clauses 
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (_, _, bindings, _, _ , _) -> visitBindindgs bindings
        | SynExpr.Typed (expr, _, _) -> visitExpr expr
        | SynExpr.Paren (expr, _, _, _) -> visitExpr expr
        | Sequentials es ->
            List.iter visitExpr es
        | SynExpr.LongIdentSet (_, expr, _) -> visitExpr expr
        | SynExpr.Tuple (exprs, _, _) -> List.iter visitExpr exprs
        | SynExpr.Const (SynConst.String (_, r), _) -> result.Add r
        | SynExpr.ArrayOrList(_, exprs, _) -> List.iter visitExpr exprs
        | SynExpr.New(_, _, expr, _) -> visitExpr expr
        | SynExpr.While(_, e1, e2, _) -> visitExpr e1; visitExpr e2
        | SynExpr.Assert(e, _) -> visitExpr e
        | SynExpr.TryWith(e, _, clauses, _, _, _, _) -> visitExpr e; visitMatches clauses
        | SynExpr.TryFinally(e1, e2, _, _, _) -> visitExpr e1; visitExpr e2
        | SynExpr.NamedIndexedPropertySet(_, e1, e2, _) -> visitExpr e1; visitExpr e2
        | SynExpr.DotNamedIndexedPropertySet(_, _, e1, e2, _) -> visitExpr e1; visitExpr e2
        | SynExpr.TypeTest(e, _, _) -> visitExpr e
        | SynExpr.Upcast(e, _, _) -> visitExpr e
        | SynExpr.InferredUpcast(e, _) -> visitExpr e
        | SynExpr.InferredDowncast(e, _) -> visitExpr e
        | SynExpr.DotGet(e, _, _, _) -> visitExpr e
        | SynExpr.TypeApp(_, _, tys, _, _, _, _) -> List.iter visitType tys
        | _ -> ()
         
    and visitBinding (Binding(_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
    and visitBindindgs = List.iter visitBinding
    and visitMatch (SynMatchClause.Clause (_, _, expr, _, _)) = visitExpr expr
    and visitMatches = List.iter visitMatch
    
    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
        | _ -> () 

    let visitTypeDefn ty =
        let (SynTypeDefn.TypeDefn (_, repr, memberDefns, _)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, defns, _) ->
            for d in defns do visitMember d
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(_, SynType.App(_, _, tys, _,_ , _, _), _), _) ->
            List.iter visitType tys
        | _ -> ()
        List.iter visitMember memberDefns

    let rec visitDeclarations decls = 
        for declaration in decls do
            match declaration with
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> for ty in types do visitTypeDefn ty
            | SynModuleDecl.NestedModule (_, decls, _, _) -> visitDeclarations decls
            | _ -> ()

    let visitModulesAndNamespaces modulesOrNss =
        for moduleOrNs in modulesOrNss do
            let (SynModuleOrNamespace(_, _, decls, _, _, _, _)) = moduleOrNs
            visitDeclarations decls

    ast 
    |> Option.iter (function
        | ParsedInput.ImplFile implFile ->
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
            visitModulesAndNamespaces modules
        | _ -> ())

    List.ofSeq result

/// Get path to containing module/namespace of a given position
let getModuleOrNamespacePath (pos: pos) (ast: ParsedInput) =
    let idents =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) ->
            let rec walkModuleOrNamespace idents (decls, moduleRange) =
                decls
                |> List.fold (fun acc -> 
                    function
                    | SynModuleDecl.NestedModule (componentInfo, nestedModuleDecls, _, nestedModuleRange) -> 
                        if rangeContainsPos moduleRange pos then
                            let (ComponentInfo(_,_,_,longIdent,_,_,_,_)) = componentInfo
                            walkModuleOrNamespace (longIdent::acc) (nestedModuleDecls, nestedModuleRange)
                        else acc
                    | _ -> acc) idents

            modules
            |> List.fold (fun acc (SynModuleOrNamespace(longIdent, _, decls, _, _, _, moduleRange)) ->
                    if rangeContainsPos moduleRange pos then
                        walkModuleOrNamespace (longIdent::acc) (decls, moduleRange) @ acc
                    else acc) []
        | ParsedInput.SigFile(ParsedSigFileInput(_, _, _, _, modules)) -> 
            let rec walkModuleOrNamespaceSig idents (decls, moduleRange) =
                decls
                |> List.fold (fun acc -> 
                    function
                    | SynModuleSigDecl.NestedModule (componentInfo, nestedModuleDecls, nestedModuleRange) -> 
                        if rangeContainsPos moduleRange pos then
                            let (ComponentInfo(_,_,_,longIdent,_,_,_,_)) = componentInfo
                            walkModuleOrNamespaceSig (longIdent::acc) (nestedModuleDecls, nestedModuleRange)
                        else acc
                    | _ -> acc) idents

            modules
            |> List.fold (fun acc (SynModuleOrNamespaceSig(longIdent, _, decls, _, _, _, moduleRange)) ->
                    if rangeContainsPos moduleRange pos then
                        walkModuleOrNamespaceSig (longIdent::acc) (decls, moduleRange) @ acc
                    else acc) []
    idents
    |> List.rev
    |> Seq.concat
    |> Seq.map (fun ident -> ident.idText)
    |> String.concat "."


module HashDirectiveInfo =
    open System.IO
    type IncludeDirective =
        | ResolvedDirectory of string

    type LoadDirective =
        | ExistingFile of string
        | UnresolvableFile of string * previousIncludes : string array

    [<NoComparison>]
    type Directive =
        | Include of IncludeDirective * range
        | Load of LoadDirective * range

    /// returns an array of LoadScriptResolutionEntries
    /// based on #I and #load directives
    let getIncludeAndLoadDirectives ast =
        // the Load items are resolved using fallback resolution relying on previously parsed #I directives
        // (this behaviour is undocumented in F# but it seems to be how it works).
        
        // list of #I directives so far (populated while encountering those in order)
        // TODO: replace by List.fold if possible
        let includesSoFar = new System.Collections.Generic.List<_>()
        let pushInclude = includesSoFar.Add

        // those might need to be abstracted away from real filesystem operations
        let fileExists = File.Exists
        let directoryExists = Directory.Exists
        let isPathRooted = Path.IsPathRooted
        let getDirectoryOfFile = Path.GetFullPathSafe >> Path.GetDirectoryName
        let getRootedDirectory = Path.GetFullPathSafe
        let makeRootedDirectoryIfNecessary baseDirectory directory =
            if not (isPathRooted directory) then
                getRootedDirectory (baseDirectory </> directory)
            else
                directory

        // separate function to reduce nesting one level
        let parseDirectives modules file =
            [|
            let baseDirectory = getDirectoryOfFile file
            for (SynModuleOrNamespace(_, _, declarations, _, _, _, _)) in modules do
                for decl in declarations do
                    match decl with
                    | SynModuleDecl.HashDirective(ParsedHashDirective("I",[directory],range),_) ->
                        let directory = makeRootedDirectoryIfNecessary (getDirectoryOfFile file) directory
                        
                        if directoryExists directory then
                            let includeDirective = ResolvedDirectory(directory)
                            pushInclude includeDirective
                            yield Include (includeDirective, range)

                    | SynModuleDecl.HashDirective(ParsedHashDirective("load",files,range),_) ->
                        for f in files do
                            if isPathRooted f && fileExists f then

                                // this is absolute reference to an existing script, easiest case
                                yield Load (ExistingFile f, range)

                            else

                                // I'm not sure if the order is correct, first checking relative to file containing the #load directive
                                // then checking for undocumented resolution using previously parsed #I directives
                                let fileRelativeToCurrentFile = baseDirectory </> f
                                if fileExists fileRelativeToCurrentFile then

                                    // this is existing file relative to current file
                                    yield Load (ExistingFile fileRelativeToCurrentFile, range)

                                else

                                    // match file against first include which seemingly have it found
                                    let maybeFile =
                                        includesSoFar
                                        |> Seq.tryPick (function
                                            | (ResolvedDirectory d) ->
                                                let filePath = d </> f
                                                if fileExists filePath then Some filePath else None
                                        )
                                    match maybeFile with
                                    | None -> () // can't load this file even using any of the #I directives...
                                    | Some f ->
                                        yield Load (ExistingFile f, range)

                    | _ -> ()
            |]

        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(fn,_,_,_,_,modules,_)) -> parseDirectives modules fn
        | _ -> [||]

    /// returns the Some (complete file name of a resolved #load directive at position) or None
    let getHashLoadDirectiveResolvedPathAtPosition (pos: pos) (ast: ParsedInput) : string option =
        getIncludeAndLoadDirectives ast
        |> Array.tryPick (
            function
            | Load(ExistingFile f, range) 
                // check the line is within the range
                // (doesn't work when there are multiple files given to a single #load directive)
                when rangeContainsPos range pos
                    -> Some f
            | _     -> None
        )


/// Set of visitor utilies, designed for the express purpose of fetching ranges
/// from an untyped AST for the purposes of outlining.
module Outlining =
    module private Range =
        let endToEnd (r1: range) (r2: range) =
            mkFileIndexRange r1.FileIndex r1.End r2.End

    let private visitBinding (b: SynBinding) =
        let (Binding(_, kind, _, _, _, _, _, _, _, _, range, _)) = b
        match kind with
        | NormalBinding ->
            let r1 = b.RangeOfBindingSansRhs
            let r2 = b.RangeOfBindingAndRhs
            Some (Range.endToEnd r1 r2)
        | DoBinding ->
            let doEnd = mkPos range.Start.Line (range.Start.Column + 2)
            Some (mkFileIndexRange range.FileIndex doEnd range.End)
        | _ -> None

    let rec private visitSynMemberDefn d =
        seq {
            match d with
            | SynMemberDefn.Member (binding, _) ->
                match visitBinding binding with
                | Some(r) -> yield r
                | _ -> ()
            | SynMemberDefn.LetBindings (bindings, _, _, _) ->
                yield! Seq.choose visitBinding bindings
            | SynMemberDefn.Interface(tp, mmembers, _) ->
                yield Range.endToEnd tp.Range d.Range
                match mmembers with
                | Some members -> yield! Seq.collect visitSynMemberDefn members
                | None -> ()
            | SynMemberDefn.NestedType(td, _, _) ->
                yield! visitTypeDefn td
            | _ -> ()
        }

    and private visitTypeDefn (TypeDefn(componentInfo, objectModel, members, range)) =
        seq {
            yield Range.endToEnd componentInfo.Range range
            match objectModel with
            | ObjectModel(_, members, _) -> yield! Seq.collect visitSynMemberDefn members
            | Simple _ -> yield! Seq.collect visitSynMemberDefn members
        }

    let private getConsecutiveModuleDecls (predicate: SynModuleDecl -> range option) (decls: SynModuleDecls) =
        let groupConsecutiveDecls input =
            let rec loop (input: range list) (res: range list list) currentBulk =
                match input, currentBulk with
                | [], [] -> List.rev res
                | [], _ -> currentBulk :: res |> List.rev
                | r :: rest, [] -> loop rest res [r]
                | r :: rest, last :: _ when r.StartLine = last.EndLine + 1 -> 
                    loop rest res (r :: currentBulk)
                | r :: rest, _ -> loop rest (currentBulk :: res) [r]
            loop input [] []


        decls 
        |> List.choose predicate
        |> groupConsecutiveDecls
        |> List.filter (fun x -> x.Length > 1)
        |> List.map (fun ranges ->
            let last = List.head ranges
            let first = List.rev ranges |> List.head
            Range.mkRange "" first.Start last.End)

    let collectOpens = getConsecutiveModuleDecls (function SynModuleDecl.Open (_, r) -> Some r | _ -> None)
    let collectHashDirectives = getConsecutiveModuleDecls (function SynModuleDecl.HashDirective (_, r) -> Some r | _ -> None)

    let rec private visitDeclaration (decl: SynModuleDecl) = 
        seq {
            match decl with
            | SynModuleDecl.Let(_, bindings, _) ->
                yield! Seq.choose visitBinding bindings
            | SynModuleDecl.Types(types, _) ->
                yield! Seq.collect visitTypeDefn types
            | SynModuleDecl.NestedModule(cmpInfo, decls, _, _) ->
                yield Range.endToEnd cmpInfo.Range decl.Range
                yield! collectOpens decls
                yield! Seq.collect visitDeclaration decls
            | _ -> ()
        }

    let private visitModuleOrNamespace moduleOrNs =
        seq {
            let (SynModuleOrNamespace(_, _, decls, _, _, _, _)) = moduleOrNs
            yield! collectHashDirectives decls
            yield! collectOpens decls
            yield! Seq.collect visitDeclaration decls
        }

    let getOutliningRanges tree =
        seq {
            match tree with
            | ParsedInput.ImplFile(implFile) ->
                let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
                yield! Seq.collect visitModuleOrNamespace modules
            | _ -> ()
        }
