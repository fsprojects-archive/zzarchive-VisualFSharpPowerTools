module FSharpVSPowerTools.SourceCodeClassifier

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

[<RequireQualifiedAccess>]
type Category =
    | ReferenceType
    | ValueType
    | PatternCase
    | Function
    | MutableVar
    | Quotation
    | Module
    | Unused
    | Other
    override x.ToString() = sprintf "%A" x

let getIdentifierCategory (symbol: FSharpSymbol) =
    match symbol with
    | Entity (_, ValueType, _) -> Category.ValueType
    | Entity Class -> Category.ReferenceType
    | Entity (_, FSharpModule, _) -> Category.Module
    | Entity (_, _, Tuple) -> Category.ReferenceType
    | Entity (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType    
    | _ ->
        //debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Category.Other 

let internal getCategory (symbolUse: FSharpSymbolUse) =
    match symbolUse.Symbol with
    | Field (MutableVar, _)
    | Field (_, RefCell) -> Category.MutableVar
    | Pattern -> Category.PatternCase
    | Entity (_, ValueType, _) -> Category.ValueType
    | Entity Class -> Category.ReferenceType
    | Entity (_, FSharpModule, _) -> Category.Module
    | Entity (_, _, Tuple) -> Category.ReferenceType
    | Entity (_, (FSharpType | ProvidedType | ByRef | Array), _) -> Category.ReferenceType
    | MemberFunctionOrValue (Constructor ValueType) -> Category.ValueType
    | MemberFunctionOrValue (Constructor _) -> Category.ReferenceType
    | MemberFunctionOrValue (Function symbolUse.IsFromComputationExpression) -> Category.Function
    | MemberFunctionOrValue MutableVar -> Category.MutableVar
    | MemberFunctionOrValue func ->
        match func.FullTypeSafe with
        | Some RefCell -> Category.MutableVar
        | _ -> Category.Other
    | _ ->
        //debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Category.Other 

type CategorizedColumnSpan =
    { Category: Category
      WordSpan: WordSpan }

[<NoComparison>]
type OpenDeclaration =
    { Idents: Idents list
      DeclRange: Range.range
      Range: Range.range }

type private Line = int

let longIdentToArray (longIdent: LongIdent): Idents =
    longIdent |> List.map string |> List.toArray

// If "what" span is entirely included in "from" span, then truncate "from" to the end of "what".
// Example: for ReferenceType symbol "System.Diagnostics.DebuggerDisplay" there are "System", "Diagnostics" and "DebuggerDisplay"
// plane symbols. After excluding "System", we get "Diagnostics.DebuggerDisplay",
// after excluding "Diagnostics", we get "DebuggerDisplay" and we are done.
let excludeWordSpan from what =
    if what.EndCol < from.EndCol && what.EndCol > from.StartCol then
        { from with StartCol = what.EndCol + 1 } // the dot between parts
    else from

let getLongIdents (input: ParsedInput) : (Range.range * Idents)[] =
    let idents = ResizeArray<Range.range * Idents>()
    let addLongIdentWithDots (value: LongIdentWithDots) = 
        idents.Add (value.Range, longIdentToArray value.Lid)
    let addIdent (ident: Ident) = idents.Add (ident.idRange, [|ident.idText|])

    let (|ConstructorPats|) = function
        | Pats ps -> ps
        | NamePatPairs(xs, _) -> List.map snd xs

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
        | SynPat.Named(SynPat.Wild _ as pat, ident, _, _, _) -> 
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
        | SynType.WithGlobalConstraints(t, _, _) -> walkType t
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
            fields |> List.iter (fun (_, e, _) -> e |> Option.iter walkExpr)
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
        | SynExpr.Sequential(_, _, e1, e2, _) -> List.iter walkExpr [e1; e2]
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
            walkComponentInfo info
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

    and walkComponentInfo (ComponentInfo(attrs, typars, constraints, _, _, _, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkTyparDecl typars
        List.iter walkTypeConstraint constraints

    and walkTypeDefnRepr = function
        | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
        | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn

    and walkTypeDefnSigRepr = function
        | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
        | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn

    and walkTypeDefn (TypeDefn (info, repr, members, _)) =
        walkComponentInfo info
        walkTypeDefnRepr repr
        List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
        | SynModuleDecl.NestedModule(info, modules, _, _) ->
            walkComponentInfo info
            List.iter walkSynModuleDecl modules
        | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
        | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
        | _ -> ()

    match input with 
    | ParsedInput.SigFile _ -> ()
    | ParsedInput.ImplFile input -> walkImplFileInput input
    //debug "%A" idents
    Seq.toArray idents
 
type IsSymbolUsed = bool

let getCategoriesAndLocations (allSymbolsUses: SymbolUse[], allEntities: RawEntity list option,
                               untypedAst: ParsedInput option, lexer: LexerBase) =
    let allSymbolsUses' =
        allSymbolsUses
        |> Seq.groupBy (fun su -> su.SymbolUse.RangeAlternate.EndLine)
        |> Seq.map (fun (line, sus) ->
            let tokens = lexer.TokenizeLine (line - 1)
            sus
            |> Seq.choose (fun su ->
                let r = su.SymbolUse.RangeAlternate
                lexer.GetSymbolFromTokensAtLocation (tokens, line - 1, r.End.Column - 1)
                |> Option.bind (fun sym -> 
                    match sym.Kind with
                    | SymbolKind.Ident ->
                        // FCS returns inaccurate ranges for multiline method chains
                        // Specifically, only the End is right. So we use the lexer to find Start for such symbols.
                        if r.StartLine < r.EndLine then
                            Some (su, { Line = r.End.Line; StartCol = r.End.Column - sym.Text.Length; EndCol = r.End.Column })
                        else 
                            Some (su, { Line = r.End.Line; StartCol = r.Start.Column; EndCol = r.End.Column })
                    | _ -> None)))
        |> Seq.concat
        |> Seq.toArray
       
    // index all symbol usages by LineNumber 
    let wordSpans = 
        allSymbolsUses'
        |> Seq.map (fun (_, span) -> span)
        |> Seq.groupBy (fun span -> span.Line)
        |> Seq.map (fun (line, ranges) -> line, ranges)
        |> Map.ofSeq

    let spansBasedOnSymbolsUses = 
        allSymbolsUses'
        |> Seq.choose (fun (symbolUse, span) ->
            let span = 
                match wordSpans.TryFind span.Line with
                | Some spans -> spans |> Seq.fold (fun result span -> excludeWordSpan result span) span
                | _ -> span

            let span' = 
                if (span.EndCol - span.StartCol) - symbolUse.SymbolUse.Symbol.DisplayName.Length > 0 then
                    // The span is wider that the simbol's display name.
                    // This means that we have not managed to extract last part of a long ident accurately.
                    // Particulary, it happens for chained method calls like Guid.NewGuid().ToString("N").Substring(1).
                    // So we get ident from the lexer.
                    match lexer.GetSymbolAtLocation (span.Line - 1, span.EndCol - 1) with
                    | Some s -> 
                        match s.Kind with
                        | Ident -> 
                            // Lexer says that our span is too wide. Adjust it's left column.
                            if span.StartCol < s.LeftColumn then { span with StartCol = s.LeftColumn }
                            else span
                        | _ -> span
                    | _ -> span
                else span

            let categorizedSpan =
                if span'.EndCol <= span'.StartCol then None
                else Some { Category = 
                                if not symbolUse.IsUsed then Category.Unused 
                                else getCategory symbolUse.SymbolUse
                            WordSpan = span' }
        
            categorizedSpan)
        |> Seq.groupBy (fun span -> span.WordSpan)
        |> Seq.map (fun (_, spans) -> 
                match List.ofSeq spans with
                | [span] -> span
                | spans -> 
                    spans 
                    |> List.sortBy (fun span -> 
                        match span.Category with
                        | Category.Unused -> 1
                        | Category.Other -> 2
                        | _ -> 0)
                    |> List.head)
        |> Seq.distinct
        |> Seq.toArray

    let quotationRanges = ref (ResizeArray<_>())

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
        | SynExpr.Quote (_, _isRaw, _quotedExpr, _, range) -> (!quotationRanges).Add range
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
        | SynExpr.Sequential (_, _, expr1, expr2, _) ->
            visitExpr expr1
            visitExpr expr2
        | SynExpr.LongIdentSet (_, expr, _) -> visitExpr expr
        | SynExpr.Tuple (exprs, _, _) -> 
            for expr in exprs do 
                visitExpr expr
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

    let visitType ty =
        let (SynTypeDefn.TypeDefn (_, repr, _, _)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, defns, _) ->
            for d in defns do visitMember d
        | _ -> ()

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

    untypedAst |> Option.iter (function
        | ParsedInput.ImplFile implFile ->
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
            visitModulesAndNamespaces modules
        | _ -> ())

    let openDeclarations =
        match untypedAst with
        | Some ast ->
            match ast with
            | ParsedInput.ImplFile implFile ->
                let autoOpenModules =
                    match allEntities with
                    | Some entities ->
                        entities |> List.filter (fun e -> 
                            match e.Kind with
                            | EntityKind.Module { IsAutoOpen = true } -> true
                            | _ -> false)
                    | None -> []
                    |> List.map (fun e -> e.Idents)

                debug "All AutoOpen modules: %A" autoOpenModules

                let modulesWithModuleSuffix =
                    match allEntities with
                    | Some entities ->
                        entities |> List.choose (fun e -> 
                            match e.Kind with
                            | EntityKind.Module { HasModuleSuffix = true } ->
                                // remove Module suffix
                                let lastIdent = e.Idents.[e.Idents.Length - 1]
                                let result = Array.copy e.Idents
                                result.[result.Length - 1] <- lastIdent.Substring (0, lastIdent.Length - 6)
                                Some result
                            | _ -> None)
                    | None -> []

                let rec walkModuleOrNamespace (parent: LongIdent) acc (decls, moduleRange) =
                    let openStatements =
                        decls
                        |> List.fold (fun acc -> 
                            function
                            | SynModuleDecl.NestedModule (ComponentInfo(_, _, _, ident, _, _, _, _), nestedModuleDecls, _, nestedModuleRange) -> 
                                walkModuleOrNamespace (parent @ ident) acc (nestedModuleDecls, nestedModuleRange)
                            | SynModuleDecl.Open (LongIdentWithDots(ident, _), openStatementRange) ->
                                (* The idents are "relative" because an open declaration can be not a fully qualified namespace
                                   or module, but a relatively qualified module:

                                   module M1 = 
                                       let x = ()
                                   module M2 =
                                       open M1
                                       open System
                                       let _ = x
    
                                   here "open M1" is relative, but "open System" is not.
                                *)
                                let relativeIdents = longIdentToArray ident
                                // Construct all possible parents taking firts ident, then two first idents and so on.
                                // It's not 100% accurate but it's the best we can do with the current FCS.
                                let grandParents = 
                                    let parent = longIdentToArray parent
                                    [ for i in -1..parent.Length - 1 -> parent.[0..i]]
                                
                                // All possible full entity idents. Again, it's not 100% accurate.
                                let fullIdentsList = 
                                    grandParents
                                    |> List.map (fun grandParent -> 
                                        Array.append grandParent relativeIdents)
                                
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
                                    fullIdentsList
                                    |> List.map (fun fullIdents ->
                                         let res = relativeIdents :: loop [fullIdents] fullIdents.Length
                                         debug "[SourceCodeClassifier] Open decl. Relative = %A, Full = %A, With AutoOpens = %A" 
                                               relativeIdents fullIdents res
                                         res)
                                    |> List.concat

                                (* For each module that has ModuleSuffix attribute value we add additional Idents "<Name>Module". For example:
                                   
                                   module M =
                                       [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                                       module M1 =
                                           module M2 =
                                               let func _ = ()
                                   open M.M1.M2
                                   The last line will produce two Idents: "M.M1.M2" and "M.M1Module.M2".
                                   The reason is that FCS return different FullName for entities declared in ModuleSuffix modules
                                   depending on thether the module is in the current project or not. 
                                *)
                                let idents = 
                                    identsAndAutoOpens
                                    |> List.map (fun idents ->
                                        [ yield idents 
                                          match modulesWithModuleSuffix |> List.tryFind (fun m -> idents |> Array.startsWith m) with
                                          | Some m ->
                                              let index = (Array.length m) - 1
                                              let modifiedIdents = Array.copy idents
                                              modifiedIdents.[index] <- idents.[index] + "Module"
                                              yield modifiedIdents
                                          | None -> ()])
                                    |> List.concat
                                    |> Seq.distinct
                                    |> Seq.toList

                                { Idents = idents
                                  DeclRange = openStatementRange
                                  Range = (Range.mkRange openStatementRange.FileName openStatementRange.Start moduleRange.End) } :: acc 
                            | _ -> acc) [] 
                    openStatements @ acc

                let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
                modules
                |> List.fold (fun acc (SynModuleOrNamespace(ident, _, decls, _, _, _, moduleRange)) ->
                     walkModuleOrNamespace ident acc (decls, moduleRange) @ acc) []
            | _ -> []
        | None -> []

    debug "[LanguageService] Open declarations: %A" openDeclarations

    let longIdentsByLine: Map<Line, (Range.range * Idents) seq> = 
        match untypedAst with
        | Some ast -> 
            getLongIdents ast
            |> Seq.filter (fun (_, idents) -> idents.Length > 0)
            |> Seq.groupBy (fun (range, _) -> range.StartLine)
            |> Map.ofSeq
        | None -> Map.empty

    let symbolUsesPotentiallyRequireOpenDecls =
        allSymbolsUses'
        |> Array.filter (fun (symbolUse, _) ->
            match symbolUse.SymbolUse.Symbol with
            | UnionCase _ 
            | Entity (Class | (ValueType | Record | UnionType | Interface | FSharpModule | Delegate), _, _)
            | MemberFunctionOrValue (Constructor _ | ExtensionMember) -> true
            | MemberFunctionOrValue func -> not func.IsMember
            | _ -> false) 
        |> Array.map (fun (symbolUse, _) -> symbolUse)

    // Filter out symbols which ranges are fully included into a bigger symbols. 
    // For example, for this code: Ns.Module.Module1.Type.NestedType.Method() FCS returns the followint symbols: 
    // Ns, Module, Module1, Type, NestedType, Method.
    // We want to filter all of them but the longest one (Method).
    let symbolUsesWithoutNested =
        symbolUsesPotentiallyRequireOpenDecls
        |> Seq.groupBy (fun symbolUse -> symbolUse.SymbolUse.RangeAlternate.StartLine)
        |> Seq.map (fun (line, symbolUses) ->
            // We use LongIdents collected from the untyped AST to determine that a bunch of entities
            // actually represents a single entity. 
            match longIdentsByLine |> Map.tryFind line with
            | Some longIdents ->
                // Group all SymbolUses by LongIdents in which they are belong (by ranges).
                symbolUses
                |> Seq.map (fun sUse -> 
                    sUse, longIdents |> Seq.tryFind (fun (r, _) -> Range.rangeContainsRange r sUse.SymbolUse.RangeAlternate))
                |> Seq.groupBy (fun (_, longIdent) -> longIdent)
                |> Seq.map (fun (longIdent, symbolUses) -> 
                    match longIdent with
                    | Some _ ->
                        (* Find all longest SymbolUses which has unique roots. For example:
                           
                           module Top
                           module M =
                               type System.IO.Path with
                                   member static ExtensionMethod() = ()

                           open M
                           open System
                           let _ = IO.Path.ExtensionMethod()

                           The last line contains three SymbolUses: "System.IO", "System.IO.Path" and "Top.M.ExtensionMethod". 
                           So, we filter out "System.IO" since it's a prefix of "System.IO.Path".

                        *)
                        let res =
                            symbolUses
                            |> Seq.filter (fun (nextSymbolUse, _) ->
                                let res = 
                                    symbolUses
                                    |> Seq.exists (fun (sUse, _) -> 
                                        nextSymbolUse <> sUse
                                        && (sUse.FullNames.Value |> Array.exists (fun fullName ->
                                            nextSymbolUse.FullNames.Value |> Array.exists (fun nextSymbolFullName ->
                                            fullName.Length > nextSymbolFullName.Length
                                            && fullName |> Array.startsWith nextSymbolFullName))))
                                    |> not
                                res)
                            |> Seq.toArray
                            |> Array.toSeq
                        res
                    | None -> symbolUses)
                |> Seq.map (Seq.map fst)
                |> Seq.concat
            | None -> symbolUses)
        |> Seq.concat
        |> Seq.toArray

    //debug "LongIdents by line: %A" longIdentsByLine

    let qualifiedSymbols: (Range.range * Idents) [] =
        symbolUsesWithoutNested
        |> Array.map (fun symbolUse ->
            let sUseRange = symbolUse.SymbolUse.RangeAlternate
            symbolUse.FullNames.Value
            |> Array.map (fun fullName ->
                sUseRange,
                match longIdentsByLine |> Map.tryFind sUseRange.StartLine with
                | Some idents ->
                    match idents |> Seq.tryFind (fun (identRange, _) ->
                        identRange.StartColumn < sUseRange.EndColumn
                        && identRange.EndColumn >= sUseRange.EndColumn) with
                    | Some (identRange, longIdent) ->
                        let rec loop matchFound longIdents symbolIdents =
                            match longIdents, symbolIdents with
                            | [], _ -> symbolIdents
                            | _, [] -> []
                            | lh :: lt, sh :: st ->
                                if lh <> sh then
                                    if matchFound then symbolIdents else loop matchFound lt symbolIdents
                                else loop true lt st
                        
                        let prefix = 
                            loop false (longIdent |> Array.rev |> List.ofArray) (fullName |> Array.rev |> List.ofArray)
                            |> List.rev
                            |> List.toArray
                            
                        debug "[QS] FullName = %A, Symbol range = %A, Ident range = %A, Res = %A" fullName sUseRange identRange prefix
                        prefix
                    | _ -> 
                        debug "[!QS] Symbol is out of any LongIdent: FullName = %A, Range = %A" fullName sUseRange
                        fullName
                | _ -> 
                    debug "[!QS] Symbol is out of any LongIdent: FullName = %A, Range = %A" fullName sUseRange
                    fullName))
        |> Array.concat

    //debug "Qualified symbols: %A" qualifiedSymbols
        
    let unusedOpenDeclarations: OpenDeclaration list =
        Array.foldBack (fun (symbolRange: Range.range, fullName) openDecls ->
            openDecls |> List.fold (fun (acc, found) (openDecl, used) -> 
                if found then
                    (openDecl, used) :: acc, found
                else
                    let usedByCurrentSymbol =
                        Range.rangeContainsRange openDecl.Range symbolRange
                        && (let isUsed = openDecl.Idents |> List.exists ((=) fullName)
                            if isUsed then debug "Open decl %A is used by %A (exact prefix)" openDecl fullName
                            isUsed)
                    (openDecl, used || usedByCurrentSymbol) :: acc, usedByCurrentSymbol) ([], false)
            |> fst
            |> List.rev
        ) qualifiedSymbols (openDeclarations |> List.map (fun openDecl -> openDecl, false))
        |> List.filter (fun (_, used) -> not used)
        |> List.map fst

    let unusedOpenDeclarationSpans =
        unusedOpenDeclarations
        |> List.map (fun decl -> { Category = Category.Unused
                                   WordSpan = { Line = decl.DeclRange.StartLine 
                                                StartCol = decl.DeclRange.StartColumn
                                                EndCol = decl.DeclRange.EndColumn }})
        |> List.toArray
    
    //printfn "AST: %A" untypedAst
    
    let trimWhitespaces = 
        Seq.skipWhile (fun t -> t.CharClass = TokenCharKind.WhiteSpace) >> Seq.toList

    let quotations =
        !quotationRanges 
        |> Seq.map (fun (r: Range.range) -> 
            if r.EndLine = r.StartLine then
                seq [ { Category = Category.Quotation
                        WordSpan = { Line = r.StartLine
                                     StartCol = r.StartColumn
                                     EndCol = r.EndColumn }} ]
            else
                [r.StartLine..r.EndLine]
                |> Seq.choose (fun line ->
                     let tokens = lexer.TokenizeLine (line - 1)

                     let tokens =
                        match tokens |> List.tryFind (fun t -> t.TokenName = "RQUOTE") with
                        | Some rquote -> 
                            tokens
                            |> List.rev
                            |> Seq.skipWhile ((<>) rquote)
                            |> Seq.toList
                            |> List.rev
                        | _ -> 
                            match tokens |> List.tryFind (fun t -> t.TokenName = "LQUOTE") with
                            | Some lquote -> tokens |> Seq.skipWhile (fun t -> t <> lquote) |> Seq.toList
                            | _ -> tokens 

                     let tokens = tokens |> trimWhitespaces |> List.rev |> trimWhitespaces |> List.rev
                     
                     match tokens with
                     | [] -> None
                     | _ ->
                        let minCol = tokens |> List.map (fun t -> t.LeftColumn) |> function [] -> 0 | xs -> xs |> List.min
                 
                        let maxCol = 
                            let tok = tokens |> List.maxBy (fun t -> t.RightColumn) 
                            tok.LeftColumn + tok.FullMatchedLength

                        Some { Category = Category.Quotation
                               WordSpan = { Line = line
                                            StartCol = minCol
                                            EndCol = maxCol }}))
        |> Seq.concat
        |> Seq.toArray

    let allSpans = spansBasedOnSymbolsUses |> Array.append quotations |> Array.append unusedOpenDeclarationSpans

//    for span in allSpans do
//       debug "-=O=- %A" span

    allSpans
