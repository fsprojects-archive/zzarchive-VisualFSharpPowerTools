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
        | SynTypeConstraint.WhereTyparIsValueType(t, _)
        | SynTypeConstraint.WhereTyparIsReferenceType(t, _)
        | SynTypeConstraint.WhereTyparIsUnmanaged(t, _)
        | SynTypeConstraint.WhereTyparSupportsNull (t, _)
        | SynTypeConstraint.WhereTyparIsComparable(t, _)
        | SynTypeConstraint.WhereTyparIsEquatable(t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, _)
        | SynTypeConstraint.WhereTyparSubtypeOfType(t, ty, _) -> walkTypar t; walkType ty
        | SynTypeConstraint.WhereTyparIsEnum(t, ts, _)
        | SynTypeConstraint.WhereTyparIsDelegate(t, ts, _) -> walkTypar t; List.iter walkType ts
        | SynTypeConstraint.WhereTyparSupportsMember(ts, sign, _) -> List.iter walkTypar ts; walkMemberSig sign

    and walkPat = function
        | SynPat.Tuple(pats, _)
        | SynPat.ArrayOrList(_, pats, _)
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
        | SynPat.Paren(pat, _) -> walkPat pat
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
        | SynType.Array(_, t, _)
        | SynType.HashConstraint(t, _)
        | SynType.MeasurePower(t, _, _) -> walkType t
        | SynType.Fun(t1, t2, _)
        | SynType.MeasureDivide(t1, t2, _) -> walkType t1; walkType t2
        | SynType.LongIdent ident -> addLongIdentWithDots ident
        | SynType.App(ty, _, types, _, _, _, _) -> walkType ty; List.iter walkType types
        | SynType.LongIdentApp(_, _, _, types, _, _, _) -> List.iter walkType types
        | SynType.Tuple(ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
        | SynType.WithGlobalConstraints(t, typeConstraints, _) -> 
            walkType t; List.iter walkTypeConstraint typeConstraints
        | _ -> ()

    and walkClause (Clause(pat, e1, e2, _, _)) =
        walkPat pat 
        walkExpr e2
        e1 |> Option.iter walkExpr

    and walkExpr = function
        | SynExpr.Paren (e, _, _, _)
        | SynExpr.Quote (_, _, e, _, _)
        | SynExpr.Typed (e, _, _)
        | SynExpr.InferredUpcast (e, _)
        | SynExpr.InferredDowncast (e, _) 
        | SynExpr.AddressOf (_, e, _, _)
        | SynExpr.DoBang (e, _)
        | SynExpr.YieldOrReturn (_, e, _)
        | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
        | SynExpr.CompExpr (_, _, e, _)
        | SynExpr.Lambda (_, _, _, e, _)
        | SynExpr.Do (e, _)
        | SynExpr.Assert (e, _)
        | SynExpr.Lazy (e, _)
        | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
        | SynExpr.New (_, t, e, _)
        | SynExpr.TypeTest (e, t, _)
        | SynExpr.Upcast (e, t, _) 
        | SynExpr.Downcast (e, t, _) -> walkExpr e; walkType t
        | SynExpr.Tuple (es, _, _)
        | Sequentials es
        | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
        | SynExpr.App(_, _, e1, e2, _)
        | SynExpr.TryFinally(e1, e2, _, _, _)
        | SynExpr.While(_, e1, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.Record(_, _, fields, _) -> 
            fields |> List.iter (fun ((ident, _), e, _) -> 
                        addLongIdentWithDots ident
                        e |> Option.iter walkExpr)
        | SynExpr.Ident ident -> addIdent ident
        | SynExpr.ObjExpr(ty, argOpt, bindings, ifaces, _, _) -> 
            argOpt |> Option.iter (fun (e, ident) -> 
                walkExpr e
                ident |> Option.iter addIdent)
            walkType ty
            List.iter walkBinding bindings
            List.iter walkInterfaceImpl ifaces
        | SynExpr.LongIdent (_, ident, _, _) -> addLongIdentWithDots ident
        | SynExpr.For (_, ident, e1, _, e2, e3, _) -> 
            addIdent ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.ForEach (_, _, _, pat, e1, e2, _) -> 
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) -> 
            List.iter walkClause synMatchClauseList
        | SynExpr.Match (_, e, synMatchClauseList, _, _) -> 
            walkExpr e 
            List.iter walkClause synMatchClauseList
        | SynExpr.TypeApp (e, _, tys, _, _, _, _) -> 
            List.iter walkType tys; walkExpr e
        | SynExpr.LetOrUse (_, _, bindings, e, _) -> 
            List.iter walkBinding bindings; walkExpr e
        | SynExpr.TryWith (e, _, clauses, _, _, _, _) -> 
            List.iter walkClause clauses;  walkExpr e
        | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) -> 
            List.iter walkExpr [e1; e2]
            e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet (ident, e, _)
        | SynExpr.DotGet (e, _, ident, _) -> 
            addLongIdentWithDots ident
            walkExpr e
        | SynExpr.DotSet (e1, idents, e2, _) -> 
            walkExpr e1
            addLongIdentWithDots idents
            walkExpr e2
        | SynExpr.DotIndexedGet (e, args, _, _) -> 
            walkExpr e
            List.iter walkIndexerArg args
        | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) -> 
            walkExpr e1
            List.iter walkIndexerArg args
            walkExpr e2
        | SynExpr.NamedIndexedPropertySet (ident, e1, e2, _) -> 
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2]
        | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, _) -> 
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.LetOrUseBang (_, _, _, pat, e1, e2, _) -> 
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.TraitCall (ts, sign, e, _) ->
            List.iter walkTypar ts 
            walkMemberSig sign
            walkExpr e
        | SynExpr.Const (SynConst.Measure(_, m), _) -> walkMeasure m
        | _ -> ()

    and walkMeasure = function
        | SynMeasure.Product (m1, m2, _)
        | SynMeasure.Divide (m1, m2, _) -> walkMeasure m1; walkMeasure m2
        | SynMeasure.Named (longIdent, _) -> addLongIdent longIdent
        | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
        | SynMeasure.Power (m, _, _) -> walkMeasure m
        | SynMeasure.Var (ty, _) -> walkTypar ty
        | SynMeasure.One
        | SynMeasure.Anon _ -> ()

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

    and walkValSig (SynValSig.ValSpfn(attrs, _, _, t, SynValInfo(argInfos, argInfo), _, _, _, _, _, _)) =
        List.iter walkAttribute attrs 
        walkType t
        argInfo :: (argInfos |> List.concat) 
        |> List.map (fun (SynArgInfo(attrs, _, _)) -> attrs) 
        |> List.concat 
        |> List.iter walkAttribute

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
        | SynMemberDefn.Member (binding, _) -> walkBinding binding
        | SynMemberDefn.ImplicitCtor (_, attrs, pats, _, _) -> 
            List.iter walkAttribute attrs 
            List.iter walkSimplePat pats
        | SynMemberDefn.ImplicitInherit (t, e, _, _) -> walkType t; walkExpr e
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
        | SynMemberDefn.Interface (t, members, _) -> 
            walkType t 
            members |> Option.iter (List.iter walkMember)
        | SynMemberDefn.Inherit (t, _, _) -> walkType t
        | SynMemberDefn.ValField (field, _) -> walkField field
        | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
        | SynMemberDefn.AutoProperty (attrs, _, _, t, _, _, _, _, e, _, _) -> 
            List.iter walkAttribute attrs
            Option.iter walkType t
            walkExpr e
        | _ -> ()

    and walkEnumCase (EnumCase(attrs, _, _, _, _)) = List.iter walkAttribute attrs

    and walkUnionCaseType = function
        | SynUnionCaseType.UnionCaseFields fields -> List.iter walkField fields
        | SynUnionCaseType.UnionCaseFullType (t, _) -> walkType t

    and walkUnionCase (SynUnionCase.UnionCase (attrs, _, t, _, _, _)) = 
        List.iter walkAttribute attrs 
        walkUnionCaseType t

    and walkTypeDefnSimple = function
        | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union (_, cases, _) -> List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record (_, fields, _) -> List.iter walkField fields
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> walkType t
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
        | SynModuleDecl.NestedModule (info, modules, _, _) ->
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
let getQuotationRanges ast =
    let quotationRanges = ResizeArray()

    let rec visitExpr = function
        | SynExpr.LongIdentSet (_, expr, _)
        | SynExpr.Typed (expr, _, _)
        | SynExpr.Paren (expr, _, _, _)
        | SynExpr.New (_, _, expr, _)
        | SynExpr.ArrayOrListOfSeqExpr (_, expr, _)
        | SynExpr.CompExpr (_, _, expr, _)
        | SynExpr.ForEach (_, _, _, _, _, expr(*body*), _)
        | SynExpr.YieldOrReturn (_, expr, _)
        | SynExpr.YieldOrReturnFrom (_, expr, _)
        | SynExpr.Do (expr, _)
        | SynExpr.DoBang (expr, _)
        | SynExpr.Downcast (expr, _, _)
        | SynExpr.For (_, _, _, _, _, expr, _)
        | SynExpr.Lazy (expr, _)
        | SynExpr.Assert (expr, _) 
        | SynExpr.TypeApp (expr, _, _, _, _, _, _) 
        | SynExpr.DotSet (_, _, expr, _) 
        | SynExpr.DotIndexedSet (_, _, expr, _, _, _) 
        | SynExpr.NamedIndexedPropertySet (_, _, expr, _) 
        | SynExpr.DotNamedIndexedPropertySet (_, _, _, expr, _) 
        | SynExpr.TypeTest (expr, _, _) 
        | SynExpr.Upcast (expr, _, _) 
        | SynExpr.InferredUpcast (expr, _) 
        | SynExpr.InferredDowncast (expr, _) 
        | SynExpr.Lambda (_, _, _, expr, _)
        | SynExpr.AddressOf (_, expr, _, _) -> 
            visitExpr expr
        | SynExpr.App (_,_, expr1(*funcExpr*),expr2(*argExpr*), _)
        | SynExpr.LetOrUseBang (_, _, _, _,expr1(*rhsExpr*),expr2(*body*), _)
        | SynExpr.TryFinally (expr1, expr2, _, _, _)
        | SynExpr.While (_, expr1, expr2, _) -> 
            visitExpr expr1; visitExpr expr2
        | SynExpr.Tuple (exprs, _, _)
        | SynExpr.ArrayOrList (_, exprs, _)
        | Sequentials  exprs ->
            List.iter visitExpr exprs
        | SynExpr.TryWith (expr, _, clauses, _, _, _, _)
        | SynExpr.Match (_, expr, clauses, _, _) ->
            visitExpr expr; visitMatches clauses 
        | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpr cond; visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr 
        | SynExpr.LetOrUse (_, _, bindings, body, _) -> visitBindindgs bindings; visitExpr body
        | SynExpr.Quote (_, _isRaw, _quotedExpr, _, range) -> quotationRanges.Add range
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (_, _, bindings, _, _ , _) -> visitBindindgs bindings
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (_, expr, _) -> expr) |> List.iter visitExpr
        | _ -> ()

    and visitBinding (Binding(_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
    and visitBindindgs = List.iter visitBinding

    and visitPattern = function
        | SynPat.QuoteExpr (expr, _) -> visitExpr expr
        | SynPat.Named (pat, _, _, _, _) 
        | SynPat.Paren (pat, _)
        | SynPat.Typed (pat, _, _) -> visitPattern pat
        | SynPat.Ands (pats, _)
        | SynPat.Tuple (pats, _)
        | SynPat.ArrayOrList (_, pats, _) -> List.iter visitPattern pats
        | SynPat.Or (pat1, pat2, _) -> visitPattern pat1; visitPattern pat2
        | SynPat.LongIdent (_, _, _, ctorArgs, _, _) -> 
            match ctorArgs with
            | SynConstructorArgs.Pats pats -> List.iter visitPattern pats
            | SynConstructorArgs.NamePatPairs(xs, _) -> 
                xs |> List.map snd |> List.iter visitPattern
        | SynPat.Record(xs, _) -> xs |> List.map snd |> List.iter visitPattern
        | _ -> ()

    and visitMatch (SynMatchClause.Clause (pat, _, expr, _, _)) = visitPattern pat; visitExpr expr

    and visitMatches = List.iter visitMatch
    
    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
        | _ -> () 

    let visitType ty =
        let (SynTypeDefn.TypeDefn (_, repr, defns, _)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, objDefns, _) ->
            for d in objDefns do visitMember d
        | _ -> ()
        for d in defns do visitMember d

    let rec visitDeclarations decls = 
        decls |> List.iter
           (function
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> List.iter visitType types
            | SynModuleDecl.NestedModule (_, decls, _, _) -> visitDeclarations decls
            | _ -> () )

    let visitModulesAndNamespaces modulesOrNss =
        modulesOrNss
        |> Seq.iter (fun (SynModuleOrNamespace(_, _, decls, _, _, _, _)) -> visitDeclarations decls) 
    ast 
    |> Option.iter (function
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
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
        | SynExpr.ArrayOrListOfSeqExpr (_, expr, _) 
        | SynExpr.CompExpr (_, _, expr, _) 
        | SynExpr.Lambda (_, _, _, expr, _) 
        | SynExpr.YieldOrReturn (_, expr, _) 
        | SynExpr.YieldOrReturnFrom (_, expr, _) 
        | SynExpr.New (_, _, expr, _) 
        | SynExpr.Assert (expr, _) 
        | SynExpr.Do (expr, _) 
        | SynExpr.Typed (expr, _, _) 
        | SynExpr.Paren (expr, _, _, _) 
        | SynExpr.DoBang (expr, _) 
        | SynExpr.Downcast (expr, _, _) 
        | SynExpr.For (_, _, _, _, _, expr, _) 
        | SynExpr.Lazy (expr, _) 
        | SynExpr.TypeTest(expr, _, _) 
        | SynExpr.Upcast(expr, _, _) 
        | SynExpr.InferredUpcast(expr, _)
        | SynExpr.InferredDowncast(expr, _)
        | SynExpr.LongIdentSet (_, expr, _) 
        | SynExpr.DotGet (expr, _, _, _) 
        | SynExpr.ForEach (_, _, _, _, _,expr(*body*), _) -> visitExpr expr
        | SynExpr.App (_,_, expr1(*funcExpr*), expr2(*argExpr*), _) 
        | SynExpr.TryFinally (expr1, expr2, _, _, _) 
        | SynExpr.NamedIndexedPropertySet (_, expr1, expr2, _) 
        | SynExpr.DotNamedIndexedPropertySet (_, _, expr1, expr2, _) 
        | SynExpr.LetOrUseBang (_, _, _, _,expr1(*rhsExpr*), expr2(*body*), _)
        | SynExpr.While (_, expr1, expr2, _) -> 
            visitExpr expr1; visitExpr expr2
        | Sequentials exprs
        | SynExpr.Tuple (exprs, _, _) 
        | SynExpr.ArrayOrList(_, exprs, _) -> List.iter visitExpr exprs
        | SynExpr.Match (_, expr, clauses, _, _)
        | SynExpr.TryWith(expr, _, clauses, _, _, _, _) ->
            visitExpr expr; visitMatches clauses 
        | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpr cond
            visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr 
        | SynExpr.LetOrUse (_, _, bindings, body, _) -> 
            visitBindindgs bindings
            visitExpr body
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (_, expr, _) -> expr) |> List.iter visitExpr
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (_, _, bindings, _, _ , _) -> visitBindindgs bindings
        | SynExpr.Const (SynConst.String (_, r), _) -> result.Add r
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
        Seq.iter (fun (SynModuleOrNamespace(_, _, decls, _, _, _, _)) -> visitDeclarations decls) modulesOrNss

    ast 
    |> Option.iter (function
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
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
            for (SynModuleOrNamespace (_, _, declarations, _, _, _, _)) in modules do
                for decl in declarations do
                    match decl with
                    | SynModuleDecl.HashDirective (ParsedHashDirective("I",[directory],range),_) ->
                        let directory = makeRootedDirectoryIfNecessary (getDirectoryOfFile file) directory
                        
                        if directoryExists directory then
                            let includeDirective = ResolvedDirectory(directory)
                            pushInclude includeDirective
                            yield Include (includeDirective, range)

                    | SynModuleDecl.HashDirective (ParsedHashDirective ("load",files,range),_) ->
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
                                        yield Load (ExistingFile f,range)
                    | _ -> ()
            |]

        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput(fn,_,_,_,_,modules,_)) -> parseDirectives modules fn
        | _ -> [||]

    /// returns the Some (complete file name of a resolved #load directive at position) or None
    let getHashLoadDirectiveResolvedPathAtPosition (pos: pos) (ast: ParsedInput) : string option =
        getIncludeAndLoadDirectives ast
        |> Array.tryPick (
            function
            | Load (ExistingFile f,range) 
                // check the line is within the range
                // (doesn't work when there are multiple files given to a single #load directive)
                when rangeContainsPos range pos
                    -> Some f
            | _     -> None
        )


/// Set of visitor utilies, designed for the express purpose of fetching ranges
/// from an untyped AST for the purposes of outlining.
module Outlining =
    [<RequireQualifiedAccess>]
    module private Range =
        /// Create a range starting at the end of r1 and finishing at the end of r2
        let inline endToEnd   (r1: range) (r2: range) = mkFileIndexRange r1.FileIndex r1.End   r2.End

        /// Create a range beginning at the start of r1 and finishing at the end of r2
        let inline startToEnd (r1: range) (r2: range) = mkFileIndexRange r1.FileIndex r1.Start r2.End
        
        /// Create a range starting at the end of r1 modified by m1 and finishing at the end of r2 modified by m2
        let inline endToEndmod (r1: range) (m1:int) (r2: range) (m2:int) = 
            let modstart,modend = mkPos r1.EndLine (r1.EndColumn+m1),mkPos r2.EndLine (r2.EndColumn+m2)
            mkFileIndexRange r1.FileIndex modstart modend


        let inline ofAttributes (attrs:SynAttributes) =            
            match attrs with | [] -> range () | _  -> startToEnd attrs.[0].Range attrs.[List.length attrs - 1].ArgExpr.Range


    ///  Scope indicates the way a range/snapshot should be collapsed. |Scope.Scope.Same| is for a scope inside 
    ///  some kind of scope delimiter, e.g. `[| ... |]`, `[ ... ]`, `{ ... }`, etc.  |Scope.Below| is for expressions 
    ///  following a binding or the the right hand side of a pattern, e.g. `let x = ...` 
    type Collapse = 
        | Below = 0 
        | Same = 1
        | Arrow = 2

    type Scope =
        | Open = 0
        | Namespace = 1
        | Module = 2
        | Type = 3
        | Member = 4
        | LetOrUse = 5
        | Match = 6
        /// MatchLambda = function | expr -> .... | expr ->...  
        | MatchLambda = 7
        | CompExpr = 8
        | IfThenElse = 9
        | ThenInIfThenElse = 10
        | ElseInIfThenElse = 11
        | TryWith = 12
        | TryInTryWith = 13
        | WithInTryWith = 14
        | TryFinally = 15 
        | TryInTryFinally = 16
        | FinallyInTryFinally = 17       
        | ArrayOrList = 18
        | ObjExpr = 19
        | For = 20
        | While = 21
        | CompExprInternal = 22
        | Quote = 23
        | Record = 24
        | Tuple = 25
        | SpecialFunc = 26
        | Do = 27
        | Lambda = 28
        | MatchClause = 29
        | Attribute = 30
        | Interface = 31
        | HashDirective = 32

    type [< NoComparison; Struct >] ScopeRange (scope:Scope,collapse:Collapse, r:range) = 
            member __.Scope = scope
            member __.Collapse = collapse
            member __.Range = r

   // let inline mkSrange scope collapse r = ScopeRange (scope,collapse,r)
    
    /// Produce a new range by adding modStart to the StartColumn of `r` 
    /// and subtracting modEnd from the EndColumn of `r`
    let inline rangeMod (r:range) modStart modEnd =
        let rStart = Range.mkPos r.StartLine (r.StartColumn+modStart) 
        let rEnd   = Range.mkPos r.EndLine   (r.EndColumn - modEnd) 
        mkFileIndexRange r.FileIndex rStart rEnd

    // Only yield a range that spans 2 or more lines
    let inline private rcheck scope collapse (r:range) = 
        seq { if r.StartLine <> r.EndLine then yield ScopeRange(scope,collapse,r )}

    let rec private visitExpr expression = 
        seq {
            match expression with
            | SynExpr.Upcast (e,_,_) 
            | SynExpr.Downcast (e,_,_)  
            | SynExpr.AddressOf(_,e,_,_) 
            | SynExpr.InferredDowncast(e,_) 
            | SynExpr.InferredUpcast(e,_)
            | SynExpr.DotGet(e,_,_,_)
            | SynExpr.DotSet(e,_,_,_)
            | SynExpr.Do (e,_)
            | SynExpr.New (_,_,e,_)
            | SynExpr.Typed(e,_,_)
            | SynExpr.DotIndexedGet(e,_,_,_)
            | SynExpr.DotIndexedSet(e,_,_,_,_,_) -> yield! visitExpr e       
            | SynExpr.YieldOrReturn (_,e,r)
            | SynExpr.DoBang (e,r)
            | SynExpr.LetOrUseBang (_,_,_,_,_,e,r)
            | SynExpr.YieldOrReturnFrom (_,e,r) ->
                yield! rcheck Scope.CompExprInternal Collapse.Below r 
                yield! visitExpr e
            | SynExpr.For (_,_,_,_,_,e,r)
            | SynExpr.ForEach (_,_,_,_,_,e,r) ->
                yield! rcheck Scope.For Collapse.Below r
                yield! visitExpr e
            | SynExpr.LetOrUse (_,_,bindings, body,_) ->
                yield! visitBindings bindings
                yield! visitExpr body
            | SynExpr.Match (seqPointAtBinding,_,clauses,_,r) ->
                match seqPointAtBinding with
                | SequencePointAtBinding pr ->
                    yield! rcheck Scope.Match Collapse.Below <| Range.endToEnd pr r
                | _ -> ()
                yield! visitMatchClauses clauses
            | SynExpr.MatchLambda (_,_,clauses,seqPointAtBinding,r) ->
                match seqPointAtBinding with
                | SequencePointAtBinding pr ->
                    yield! rcheck Scope.MatchLambda Collapse.Below <| Range.endToEnd pr r
                | _ -> ()
                yield! rcheck Scope.MatchLambda Collapse.Below r 
                //yield! rcheck Scope.Same <| e.Range  // Collapse the scope after `->`  start scope for after -> here?
                yield! visitMatchClauses clauses
            | SynExpr.App (atomicFlag,isInfix,funcExpr,argExpr,r) ->
                // seq exprs, custom operators, etc
                if ExprAtomicFlag.NonAtomic=atomicFlag && isInfix=false 
                   && (function|SynExpr.Ident _->true|_->false) funcExpr 
                   // if the argExrp is a computation expression another match will handle the outlining
                   // these cases must be removed to prevent creating unnecessary tags for the same scope
                   && (function|SynExpr.CompExpr _->false|_->true) argExpr then
                        yield! rcheck Scope.SpecialFunc Collapse.Below r
                yield! visitExpr argExpr
                yield! visitExpr funcExpr
            | SynExpr.Sequential (_,_,e1,e2,_) ->
                yield! visitExpr e1
                yield! visitExpr e2
            | SynExpr.ArrayOrListOfSeqExpr (isArray,e,r) ->
                yield! rcheck  Scope.ArrayOrList Collapse.Same <| rangeMod r (if isArray then 2 else 1) (if isArray then 2 else 1)
                yield! visitExpr e
            | SynExpr.CompExpr (arrayOrList,_,e,r) ->
                if arrayOrList then 
                    yield! visitExpr e
                else  // exclude the opening { and closing } on the cexpr from collapsing
                    yield! rcheck Scope.CompExpr Collapse.Same <| rangeMod r 1 1
                yield! visitExpr e
            | SynExpr.ObjExpr (_,_,bindings,_,newRange,wholeRange) ->
                let r = mkFileIndexRange newRange.FileIndex newRange.End (Range.mkPos wholeRange.EndLine (wholeRange.EndColumn - 1))
                yield! rcheck Scope.ObjExpr Collapse.Below r
                yield! visitBindings bindings
            | SynExpr.TryWith (e,_,matchClauses,tryRange,withRange,tryPoint,withPoint) ->
                match tryPoint with
                | SequencePointAtTry r -> 
                    yield! rcheck Scope.TryWith Collapse.Below <| Range.endToEnd r tryRange
                | _ -> ()
                match withPoint with
                | SequencePointAtWith r ->
                    yield! rcheck Scope.WithInTryWith Collapse.Below <| Range.endToEnd r withRange
                | _ -> ()
                yield! visitExpr e
                yield! visitMatchClauses matchClauses
            | SynExpr.TryFinally (tryExpr,finallyExpr,r,tryPoint,finallyPoint) ->
                match tryPoint with
                | SequencePointAtTry tryRange ->
                    yield! rcheck Scope.TryFinally Collapse.Below<| Range.endToEnd tryRange r
                | _ -> ()
                match finallyPoint with
                | SequencePointAtFinally finallyRange ->                    
                    yield! rcheck  Scope.FinallyInTryFinally Collapse.Below <| Range.endToEnd finallyRange r
                | _ -> ()
                yield! visitExpr tryExpr
                yield! visitExpr finallyExpr
            | SynExpr.IfThenElse (e1,e2,e3,seqPointInfo,_,_,r) ->                
                // Outline the entire IfThenElse
                yield! rcheck Scope.IfThenElse Collapse.Below r
                // Outline the `then` scope
                match seqPointInfo with
                | SequencePointInfoForBinding.SequencePointAtBinding rt -> 
                    yield! rcheck  Scope.ThenInIfThenElse Collapse.Below <| Range.endToEnd rt e2.Range
                | _ -> ()
                yield! visitExpr e1
                yield! visitExpr e2
                match e3 with
                | Some e -> 
                    yield! rcheck Scope.ElseInIfThenElse Collapse.Same e.Range
                    yield! visitExpr e
                | None -> ()
            | SynExpr.While (_,_,e,r) ->
                yield! rcheck Scope.While Collapse.Below  r
                yield! visitExpr e
            | SynExpr.Lambda (_,_,_,e,r) ->
                yield! rcheck Scope.Lambda Collapse.Arrow r
                yield! visitExpr e
            | SynExpr.Lazy (e,r) ->
                yield! rcheck Scope.SpecialFunc Collapse.Below r
                yield! visitExpr e
            | SynExpr.Quote (_,isRaw,e,_,r) ->
                // subtract columns so the @@> or @> is not collapsed
                yield! rcheck Scope.Quote Collapse.Same <| rangeMod r (if isRaw then 3 else 2) (if isRaw then 3 else 2)
                yield! visitExpr e
            | SynExpr.Tuple (es,_,r) ->
                yield! rcheck Scope.Tuple Collapse.Same r
                yield! Seq.collect visitExpr es
            | SynExpr.Paren (e,_,_,_) ->
                yield! visitExpr e
            | SynExpr.Record (recCtor,recCopy,recordFields,r) ->
                if recCtor.IsSome then 
                    let (_,ctorArgs,_,_,_) = recCtor.Value
                    yield! visitExpr ctorArgs
                if recCopy.IsSome then
                    let (e,_) = recCopy.Value
                    yield! visitExpr e
                yield! recordFields |> (Seq.choose(fun(_,e,_)->e) >> Seq.collect visitExpr)
                // exclude the opening `{` and closing `}` of the record from collapsing
                yield! rcheck Scope.Record Collapse.Same <| rangeMod r 1 1         
            | _ -> ()
        }

    and private visitMatchClause (SynMatchClause.Clause (synpat,_,e,_,_)) = 
        seq {        
                yield! rcheck Scope.MatchClause Collapse.Arrow <| Range.startToEnd synpat.Range e.Range  // Collapse the scope after `->`
                yield! visitExpr e 
            }

    and private visitMatchClauses = Seq.collect visitMatchClause

    and private visitAttributes (attrs:SynAttributes) =
        seq{
            let attrListRange  = 
                if attrs.Length = 0 then Seq.empty else 
                rcheck Scope.Attribute Collapse.Same  <| Range.startToEnd (attrs.[0].Range) (attrs.[attrs.Length-1].ArgExpr.Range)
            match  attrs with
            | [] -> ()
            | [_] -> yield! attrListRange
            | hd::tl -> 
                yield! attrListRange
                yield! visitExpr hd.ArgExpr
                // If there are more than 2 attributes only add tags to the 2nd and beyond, to avoid double collapsing on the first attribute
                yield! tl |> Seq.collect (fun attr -> rcheck Scope.Attribute Collapse.Same <| Range.startToEnd attr.Range attr.ArgExpr.Range)
                // visit the expressions inside each attribute
                yield! attrs |> Seq.collect (fun attr -> visitExpr attr.ArgExpr)
        }

    and private visitBinding (Binding (_,kind,_,_,attrs,_,_,_,_,e,_,_) as b) =        
        seq {
            match kind with
            | SynBindingKind.NormalBinding ->
                let r1 = b.RangeOfBindingSansRhs
                let r2 = b.RangeOfBindingAndRhs
                yield! rcheck Scope.LetOrUse Collapse.Below <| Range.endToEnd r1 r2
            | _ -> () 
            yield! visitAttributes attrs
            yield! visitExpr e
        }

    and private visitBindings = Seq.collect visitBinding 

    and private visitSynMemberDefn d =
        seq {
            match d with
            | SynMemberDefn.Member (binding,r) ->
                yield! rcheck Scope.Member Collapse.Below r
                yield! visitBinding binding
            | SynMemberDefn.LetBindings (bindings,_,_,r) ->
                yield! rcheck Scope.LetOrUse Collapse.Below r
                yield! visitBindings bindings
            | SynMemberDefn.Interface (tp,iMembers,_) ->
                yield! rcheck Scope.Interface Collapse.Below <| Range.endToEnd tp.Range d.Range
                match iMembers with
                | Some members -> yield! Seq.collect visitSynMemberDefn members
                | None -> ()
            | SynMemberDefn.NestedType (td,_,_) ->
                yield! visitTypeDefn td
            | SynMemberDefn.AbstractSlot (_,_,r) ->
                yield! rcheck Scope.Member Collapse.Below r
            | SynMemberDefn.AutoProperty (_,_,_,_,(*memkind*)_,_,_,_,e,_,r) ->
                yield! rcheck Scope.Member Collapse.Below r
                yield! visitExpr e   
            | _ -> ()
        }


    and private visitTypeDefn (TypeDefn (componentInfo,objectModel,members,range)) =
        seq {            
            yield! rcheck Scope.Type Collapse.Below <| Range.endToEnd componentInfo.Range range
            match objectModel with
            | SynTypeDefnRepr.ObjectModel (_,objMembers,_) -> 
                yield! Seq.collect visitSynMemberDefn objMembers
                yield! Seq.collect visitSynMemberDefn members
            | SynTypeDefnRepr.Simple _ -> 
                yield! Seq.collect visitSynMemberDefn members
        }


    let private getConsecutiveModuleDecls (predicate: SynModuleDecl -> range option) (scope:Scope) (decls: SynModuleDecls) =
        let groupConsecutiveDecls input =
            let rec loop (input: range list) (res: range list list) currentBulk =
                match input, currentBulk with
                | [], [] -> List.rev res
                | [], _ -> List.rev (currentBulk::res)
                | r :: rest, [] -> loop rest res [r]
                | r :: rest, last :: _ when r.StartLine = last.EndLine + 1 -> 
                    loop rest res (r::currentBulk)
                | r :: rest, _ -> loop rest (currentBulk::res) [r]
            loop input [] []
        
        let selectRanges (ranges: range list) =
            match ranges with
            | [] -> None
            | [r] when r.StartLine = r.EndLine -> None
            | [r] -> Some <| ScopeRange(scope, Collapse.Same, (Range.mkRange "" r.Start r.End))
            | lastRange :: rest ->
                let firstRange = Seq.last rest
                Some <| ScopeRange(scope, Collapse.Same, (Range.mkRange "" firstRange.Start lastRange.End))

        decls |> (List.choose predicate>>groupConsecutiveDecls>>List.choose selectRanges)


    let collectOpens = getConsecutiveModuleDecls (function SynModuleDecl.Open (_,r) -> Some r | _ -> None) Scope.Open


    let collectHashDirectives =
         getConsecutiveModuleDecls(
            function 
            | SynModuleDecl.HashDirective (ParsedHashDirective (directive,_,_),r) ->
                let prefixLength = "#".Length + directive.Length + " ".Length
                Some (Range.mkRange "" (Range.mkPos r.StartLine prefixLength) r.End)
            | _ -> None) Scope.HashDirective


    let rec private visitDeclaration (decl: SynModuleDecl) = 
        seq {
            match decl with
            | SynModuleDecl.Let (_,bindings,_) ->
                yield! visitBindings bindings
            | SynModuleDecl.Types (types,_) ->
                yield! Seq.collect visitTypeDefn types
            // Fold the attributes above a module
            | SynModuleDecl.NestedModule (SynComponentInfo.ComponentInfo (attrs,_,_,_,_,_,_,cmpRange), decls,_,_) ->
                // Outline the full scope of the module
                yield! rcheck Scope.Module Collapse.Below <| Range.endToEnd cmpRange decl.Range
                // A module's component info stores the ranges of its attributes
                yield! visitAttributes attrs
                yield! collectOpens decls
                yield! Seq.collect visitDeclaration decls
            | SynModuleDecl.DoExpr (_,e,_) ->
                yield! visitExpr e
            | SynModuleDecl.Attributes (attrs,_) ->
                yield! visitAttributes attrs
            | _ -> ()
        }


    let private visitModuleOrNamespace moduleOrNs =
        seq {
            let (SynModuleOrNamespace.SynModuleOrNamespace (_,_,decls,_,_,_,_)) = moduleOrNs
            yield! collectHashDirectives decls
            yield! collectOpens decls
            yield! Seq.collect visitDeclaration decls
        }


    let getOutliningRanges tree =
        seq {
            match tree with
            | ParsedInput.ImplFile(implFile) ->
                let (ParsedImplFileInput (_,_,_,_,_,modules,_)) = implFile
                yield! Seq.collect visitModuleOrNamespace modules
            | _ -> ()
        }

