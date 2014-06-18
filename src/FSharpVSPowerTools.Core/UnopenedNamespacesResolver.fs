namespace FSharpVSPowerTools

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast

type Namespace = string[]

type Entity = 
    { Namespace: string
      Name: string } 
    override x.ToString() = sprintf "%A" x
       
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
    let rec private getRelativeName (ns: Namespace) (fullName: string[]) =
        match ns, fullName with
        | [||], _ 
        | _, [||] -> fullName
        | _ when ns.[0] = fullName.[0] ->
            getRelativeName ns.[1..] fullName.[1..]
        | _ -> fullName

    let tryCreate (ns: Namespace) (ident: string) (fullName: string) =
        fullName
        |> Option.ofNull
        |> Option.bind (fun fullName ->
            let fullName = fullName.Split '.'
            if fullName.Length = 0 then None
            elif fullName.[fullName.Length - 1] <> ident then None
            else
                match getRelativeName ns fullName with
                | [||] | [|_|] -> None
                | relativeName ->
                    Some { Namespace = String.Join (".", relativeName.[0..relativeName.Length - 2])
                           Name = relativeName.[relativeName.Length - 1] })

type Pos = 
    { Line: int
      Col: int }
       
module Ast =
    type EndLine = int
    type EntityFullName = string
    type Ident = string

    let isEntity (ast: ParsedInput) (pos: Range.pos) : bool =
        let (|ConstructorPats|) = function
            | Pats ps -> ps
            | NamePatPairs(xs, _) -> List.map snd xs

        let ifPosInRange range f =
            if Range.rangeContainsPos range pos then f()
            else false

        let isPosInRange range = ifPosInRange range (fun _ -> true)

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.exists walkSynModuleOrNamespace moduleOrNamespaceList

        and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, decls, _, _, _, r)) =
            ifPosInRange r (fun _ -> List.exists walkSynModuleDecl decls)

        and walkAttribute (attr: SynAttribute) = isPosInRange attr.Range

        and walkPat = function
            | SynPat.Ands (pats, _) -> List.exists walkPat pats
            | SynPat.Named(SynPat.Wild nameRange as pat, _, _, _, _) -> 
                if isPosInRange nameRange then false
                else walkPat pat
            | SynPat.Typed(pat, t, r) -> walkPat pat || walkType r t
            | SynPat.Attrib(pat, attrs, _) -> walkPat pat || List.exists walkAttribute attrs
            | SynPat.Or(pat1, pat2, _) -> List.exists walkPat [pat1; pat2]
            | SynPat.LongIdent(_, _, _, ConstructorPats pats, _, _) -> 
                List.exists walkPat pats
            | SynPat.Tuple(pats, _) -> List.exists walkPat pats
            | SynPat.Paren(pat, _) -> walkPat pat
            | SynPat.ArrayOrList(_, pats, _) -> List.exists walkPat pats
            | SynPat.IsInst(t, r) -> walkType r t
            | SynPat.QuoteExpr(e, _) -> walkExpr e
            | _ -> false

        and walkBinding (SynBinding.Binding(_, _, _, _, attrs, _, _, pat, returnInfo, e, _, _)) =
            List.exists walkAttribute attrs
            || walkPat pat
            || walkExpr e 
            || (match returnInfo with
                | Some (SynBindingReturnInfo (t, r, _)) -> walkType r t
                | None -> false)

        and walkInterfaceImpl (InterfaceImpl(_, bindings, _)) =
            List.exists walkBinding bindings

        and walkIndexerArg = function
            | SynIndexerArg.One e -> walkExpr e
            | SynIndexerArg.Two(e1, e2) -> List.exists walkExpr [e1; e2]

        and walkType r t =
            ifPosInRange r (fun _ ->
                match t with
                | SynType.LongIdent _ -> true
                | SynType.App(_, _, types, _, _, _, r) -> List.exists (walkType r) types
                | SynType.LongIdentApp(_, _, _, types, _, _, r) -> List.exists (walkType r) types
                | SynType.Tuple(ts, r) -> ts |> List.exists (fun (_, t) -> walkType r t)
                | SynType.Array(_, t, r) -> walkType r t
                | SynType.Fun(t1, t2, r) -> walkType r t1 || walkType r t2
                | SynType.WithGlobalConstraints(t, _, r) -> walkType r t
                | SynType.HashConstraint(t, r) -> walkType r t
                | SynType.MeasureDivide(t1, t2, r) -> walkType r t1 || walkType r t2
                | SynType.MeasurePower(t, _, r) -> walkType r t
                | _ -> false)

        and walkClause (Clause(pat, e1, e2, _, _)) =
            walkPat pat 
            || walkExpr e2 
            || match e1 with Some e -> walkExpr e | _ -> false

        and walkExpr = function
            | SynExpr.LongIdent (_, _, _, r) -> isPosInRange r
            | SynExpr.Paren (e, _, _, _) -> walkExpr e
            | SynExpr.Quote(_, _, e, _, _) -> walkExpr e
            | SynExpr.Typed(e, _, _) -> walkExpr e
            | SynExpr.Tuple(es, _, _) -> List.exists walkExpr es
            | SynExpr.ArrayOrList(_, es, _) -> List.exists walkExpr es
            | SynExpr.Record(_, _, fields, r) -> 
                ifPosInRange r (fun _ ->
                    fields 
                    |> List.exists (fun (_, e, _) -> 
                        match e with
                        | None -> false
                        | Some e -> walkExpr e))
            | SynExpr.New(_, _, e, _) -> walkExpr e
            | SynExpr.ObjExpr(_, _, bindings, ifaces, _, _) -> 
                List.exists walkBinding bindings || List.exists walkInterfaceImpl ifaces
            | SynExpr.While(_, e1, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.For(_, _, e1, _, e2, e3, _) -> List.exists walkExpr [e1; e2; e3]
            | SynExpr.ForEach(_, _, _, _, e1, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.ArrayOrListOfSeqExpr(_, e, _) -> walkExpr e
            | SynExpr.CompExpr(_, _, e, _) -> walkExpr e
            | SynExpr.Lambda(_, _, _, e, _) -> walkExpr e
            | SynExpr.MatchLambda(_, _, synMatchClauseList, _, _) -> 
                List.exists walkClause synMatchClauseList
            | SynExpr.Match(_, e, synMatchClauseList, _, _) -> 
                walkExpr e || List.exists walkClause synMatchClauseList
            | SynExpr.Do(e, _) -> walkExpr e
            | SynExpr.Assert(e, _) -> walkExpr e
            | SynExpr.App(_, _, e1, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.TypeApp(e, _, _, _, _, _, _) -> walkExpr e
            | SynExpr.LetOrUse(_, _, bindings, _, _) -> List.exists walkBinding bindings
            | SynExpr.TryWith(e, _, _, _, _, _, _) -> walkExpr e
            | SynExpr.TryFinally(e1, e2, _, _, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.Lazy(e, _) -> walkExpr e
            | SynExpr.Sequential(_, _, e1, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.IfThenElse(e1, e2, e3, _, _, _, _) -> 
                List.exists walkExpr [e1; e2] || match e3 with None -> false | Some e -> walkExpr e
            | SynExpr.Ident _ -> true
            | SynExpr.LongIdentSet(_, e, _) -> walkExpr e
            | SynExpr.DotGet(e, _, _, _) -> walkExpr e
            | SynExpr.DotSet(e, _, _, _) -> walkExpr e
            | SynExpr.DotIndexedGet(e, args, _, _) -> walkExpr e || List.exists walkIndexerArg args
            | SynExpr.DotIndexedSet(e, args, _, _, _, _) -> walkExpr e || List.exists walkIndexerArg args
            | SynExpr.NamedIndexedPropertySet(_, e1, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.DotNamedIndexedPropertySet(e1, _, e2, e3, _) -> List.exists walkExpr [e1; e2; e3]
            | SynExpr.TypeTest(e, t, r) -> walkExpr e || walkType r t
            | SynExpr.Upcast(e, t, r) -> walkExpr e || walkType r t
            | SynExpr.Downcast(e, t, r) -> walkExpr e || walkType r t
            | SynExpr.InferredUpcast(e, _) -> walkExpr e
            | SynExpr.InferredDowncast(e, _) -> walkExpr e
            | SynExpr.AddressOf(_, e, _, _) -> walkExpr e
            | SynExpr.JoinIn(e1, _, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.YieldOrReturn(_, e, _) -> walkExpr e
            | SynExpr.YieldOrReturnFrom(_, e, _) -> walkExpr e
            | SynExpr.LetOrUseBang(_, _, _, _, e1, e2, _) -> List.exists walkExpr [e1; e2]
            | SynExpr.DoBang(e, _) -> walkExpr e
            | _ -> false

        and walkSimplePat = function
            | SynSimplePat.Attrib (pat, attrs, _) ->
                walkSimplePat pat || List.exists walkAttribute attrs
            | SynSimplePat.Typed(pat, t, r) -> 
                walkSimplePat pat || walkType r t
            | _ -> false

        and walkField (SynField.Field(attrs, _, _, t, _, _, _, r)) =
            List.exists walkAttribute attrs
            || walkType r t

        and walkMember = function
            | SynMemberDefn.AbstractSlot (_, _, _) -> false
//            | SynMemberDefn.Member(SynBinding.Binding(_, _, _, _, attrs, _, _, pat, returnInfo, e, _, _) as binding, r) -> 
//                match pat with
//                | SynPat.LongIdent _ ->
//                    walkBinding r binding
            | SynMemberDefn.Member(binding, _) -> walkBinding binding
            | SynMemberDefn.ImplicitCtor(_, attrs, pats, _, _) -> 
                List.exists walkAttribute attrs
                || List.exists walkSimplePat pats
            | SynMemberDefn.ImplicitInherit(t, e, _, r) -> walkType r t || walkExpr e
            | SynMemberDefn.LetBindings(bindings, _, _, _) -> List.exists walkBinding bindings
            | SynMemberDefn.Interface(t, members, r) -> 
                walkType r t 
                || match members with 
                   | None -> false
                   | Some members -> List.exists walkMember members
            | SynMemberDefn.Inherit(t, _, r) -> walkType r t
            | SynMemberDefn.ValField(field, _) -> walkField field
            | SynMemberDefn.NestedType(tdef, _, _) -> walkTypeDefn tdef
            | SynMemberDefn.AutoProperty(attrs, _, _, t, _, _, _, _, e, _, r) -> 
                List.exists walkAttribute attrs
                || match t with None -> false | Some t -> walkType r t
                || walkExpr e
            | _ -> false

        and walkEnumCase (EnumCase(attrs, _, _, _, _)) = List.exists walkAttribute attrs

        and walkUnionCaseType r t =
            match t with
            | SynUnionCaseType.UnionCaseFields fields -> List.exists walkField fields
            | SynUnionCaseType.UnionCaseFullType(t, _) -> walkType r t

        and walkUnionCase (UnionCase(attrs, _, t, _, _, r)) = 
            List.exists walkAttribute attrs
            || walkUnionCaseType r t

        and walkTypeDefnSimple = function
            | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.exists walkEnumCase cases
            | SynTypeDefnSimpleRepr.Union(_, cases, _) -> List.exists walkUnionCase cases
            | SynTypeDefnSimpleRepr.Record(_, fields, _) -> List.exists walkField fields
            | SynTypeDefnSimpleRepr.TypeAbbrev(_, t, r) -> walkType r t
            | _ -> false

        and walkTypeDefn (TypeDefn(ComponentInfo(attrs, _, _, _, _, _, _, _), repr, members, _)) =
            List.exists walkAttribute attrs
            ||
            (match repr with
                | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.exists walkMember defns
                | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn)
            || List.exists walkMember members

        and walkSynModuleDecl (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(_, modules, _, range) ->
                ifPosInRange range (fun _ -> List.exists walkSynModuleDecl modules)
            | SynModuleDecl.Open _ -> false
            | SynModuleDecl.Let (_, bindings, r) ->
                ifPosInRange r (fun _ -> List.exists walkBinding bindings)
            | SynModuleDecl.Types (types, _) -> List.exists walkTypeDefn types
            | _ -> false

        let res = 
            match ast with 
            | ParsedInput.SigFile _ -> false
            | ParsedInput.ImplFile input -> walkImplFileInput input
        //debug "%A" ast
        res

    type Col = int

    let findNearestOpenStatementBlock (currentLine: int) (ast: ParsedInput) = 
        let result = ref None
        let modules = ResizeArray<LongIdent * EndLine * Col>()  
         
        let doRange (ns: LongIdent) line col = 
            if line <= currentLine then
                match !result with
                | None -> result := Some (ns, { Line = line; Col = col})
                | Some (oldNs, { Line = oldLine}) when oldLine < line -> 
                    result := Some ((match ns with [] -> oldNs | _ -> ns), { Line = line; Col = col }) 
                | _ -> ()

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.iter (walkSynModuleOrNamespace []) moduleOrNamespaceList

        and walkSynModuleOrNamespace (parent: LongIdent) (SynModuleOrNamespace(ident, isModule, decls, _, _, _, range)) =
            if range.EndLine >= currentLine then
                let fullIdent = parent @ ident
                let startLine =
                    if isModule then range.StartLine
                    else range.StartLine - 1

                doRange fullIdent startLine range.StartColumn
                modules.Add (fullIdent, range.EndLine, range.StartColumn)
                List.iter (walkSynModuleDecl fullIdent) decls

        and walkSynModuleDecl (parent: LongIdent) (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace parent fragment
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, ident, _, _, _, _), modules', _, range) ->
                let fullIdent = parent @ ident
                modules.Add (fullIdent, range.EndLine, range.StartColumn) 
                if range.EndLine >= currentLine then
                    doRange fullIdent range.StartLine (range.StartColumn + 4)
                    List.iter (walkSynModuleDecl fullIdent) modules'
            | SynModuleDecl.Open (_, range) -> doRange [] range.EndLine (range.StartColumn - 5)
            | _ -> ()

        match ast with 
        | ParsedInput.SigFile _input -> ()
        | ParsedInput.ImplFile input -> walkImplFileInput input

        let res = !result |> Option.map (fun (ns, pos) -> 
            ns |> List.map (fun x -> string x) |> List.toArray, { pos with Line = pos.Line + 1 }) 
        //debug "[UnopenedNamespaceResolver] Ident, line, col = %A, AST = %A" (!result) ast
        //printfn "[UnopenedNamespaceResolver] Ident, line, col = %A, AST = %A" (!result) ast
        let modules = 
            modules 
            |> Seq.filter (fun (_, endLine, _) -> endLine < currentLine)
            |> Seq.map (fun (m, endLine, startCol) -> String.Join (".", m |> Seq.map string), endLine, startCol) 
            |> Seq.sortBy (fun (m, _, _) -> -m.Length)
            |> Seq.toList
        fun (ident: Ident) (entityFullName: EntityFullName) ->
            res 
            |> Option.bind (fun (ns, pos) -> Entity.tryCreate ns ident entityFullName |> Option.map (fun e -> e, pos))
            |> Option.map (fun (entity, pos) ->
                entity,
                match modules 
                      |> List.filter (fun (m, _, _) -> 
                            entityFullName.StartsWith m 
                            && entityFullName.Length > m.Length && entityFullName.[m.Length] = '.') with
                | [] -> pos
                | (_, endLine, startCol) as _m :: _ ->
                    //printfn "All modules: %A, Win module: %A" modules m
                    { Line = endLine + 1; Col = startCol })
