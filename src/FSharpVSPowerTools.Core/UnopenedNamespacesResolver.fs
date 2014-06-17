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
            | SynPat.Named(pat, _, _, _, _) -> walkPat pat
            | SynPat.Typed(pat, t, r) -> walkPat pat || walkType r t
            | SynPat.Attrib(pat, attrs, _) -> walkPat pat || List.exists walkAttribute attrs
            | SynPat.Or(pat1, pat2, _) -> List.exists walkPat [pat1; pat2]
            | SynPat.LongIdent(_, _, _, _, _, r) -> isPosInRange r
            | SynPat.Tuple(pats, _) -> List.exists walkPat pats
            | SynPat.Paren(pat, _) -> walkPat pat
            | SynPat.ArrayOrList(_, pats, _) -> List.exists walkPat pats
            | SynPat.IsInst(t, r) -> walkType r t
            | SynPat.QuoteExpr(e, r) -> walkExpr r e
            | _ -> false

        and walkBinding r (SynBinding.Binding(_, _, _, _, attrs, _, _, pat, returnInfo, e, _, _)) =
            List.exists walkAttribute attrs
            || walkPat pat
            || walkExpr r e 
            || (match returnInfo with
                | Some (SynBindingReturnInfo (t, r, _)) -> walkType r t
                | None -> false)

        and walkInterfaceImpl r (InterfaceImpl(_, bindings, _)) =
            List.exists (walkBinding r) bindings

        and walkIndexerArg r e = 
            ifPosInRange r (fun _ ->
                match e with
                | SynIndexerArg.One e -> walkExpr r e
                | SynIndexerArg.Two(e1, e2) -> List.exists (walkExpr r) [e1; e2])

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

        and walkExpr range expr = ifPosInRange range <| fun _ ->
            match expr with
            | SynExpr.LongIdent (_, _, _, r) -> isPosInRange r
            | SynExpr.Paren (e, _, _, r) -> walkExpr r e
            | SynExpr.Quote(_, _, e, _, r) -> walkExpr r e
            | SynExpr.Typed(e, _, r) -> walkExpr r e
            | SynExpr.Tuple(es, _, r) -> List.exists (walkExpr r ) es
            | SynExpr.ArrayOrList(_, es, r) -> List.exists (walkExpr r) es
            | SynExpr.Record(_, _, fields, r) -> 
                ifPosInRange r (fun _ ->
                    fields 
                    |> List.exists (fun (_, e, _) -> 
                        match e with
                        | None -> false
                        | Some e -> walkExpr r e))
            | SynExpr.New(_, _, e, r) -> walkExpr r e
            | SynExpr.ObjExpr(_, _, bindings, ifaces, _, r) -> 
                List.exists (walkBinding r) bindings || List.exists (walkInterfaceImpl r) ifaces
            | SynExpr.While(_, e1, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.For(_, _, e1, _, e2, e3, r) -> List.exists (walkExpr r) [e1; e2; e3]
            | SynExpr.ForEach(_, _, _, _, e1, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.ArrayOrListOfSeqExpr(_, e, r) -> walkExpr r e
            | SynExpr.CompExpr(_, _, e, r) -> walkExpr r e
            | SynExpr.Lambda(_, _, _, e, r) -> walkExpr r e
            | SynExpr.MatchLambda(_, _, synMatchClauseList, _, _) -> 
                synMatchClauseList |> List.exists (fun (Clause(_, _, e, r, _)) -> walkExpr r e)
            | SynExpr.Match(_, _, synMatchClauseList, _, _) -> 
                synMatchClauseList |> List.exists (fun (Clause(_, _, e, r, _)) -> walkExpr r e)
            | SynExpr.Do(e, r) -> walkExpr r e
            | SynExpr.Assert(e, r) -> walkExpr r e
            | SynExpr.App(_, _, e1, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.TypeApp(e, _, _, _, _, _, r) -> walkExpr r e
            | SynExpr.LetOrUse(_, _, bindings, _, r) -> List.exists (walkBinding r) bindings
            | SynExpr.TryWith(e, r, _, _, _, _, _) -> walkExpr r e
            | SynExpr.TryFinally(e1, e2, r, _, _) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.Lazy(e, r) -> walkExpr r e
            | SynExpr.Sequential(_, _, e1, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.IfThenElse(e1, e2, e3, _, _, _, r) -> 
                List.exists (walkExpr r) [e1; e2] || match e3 with None -> false | Some e -> walkExpr r e
            | SynExpr.Ident _ -> true
            | SynExpr.LongIdentSet(_, e, r) -> walkExpr r e
            | SynExpr.DotGet(e, _, _, r) -> walkExpr r e
            | SynExpr.DotSet(e, _, _, r) -> walkExpr r e
            | SynExpr.DotIndexedGet(e, args, _, r) -> walkExpr r e || List.exists (walkIndexerArg r) args
            | SynExpr.DotIndexedSet(e, args, _, _, _, r) -> walkExpr r e || List.exists (walkIndexerArg r) args
            | SynExpr.NamedIndexedPropertySet(_, e1, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.DotNamedIndexedPropertySet(e1, _, e2, e3, r) -> List.exists (walkExpr r) [e1; e2; e3]
            | SynExpr.TypeTest(e, t, r) -> walkExpr r e || walkType r t
            | SynExpr.Upcast(e, t, r) -> walkExpr r e || walkType r t
            | SynExpr.Downcast(e, t, r) -> walkExpr r e || walkType r t
            | SynExpr.InferredUpcast(e, r) -> walkExpr r e
            | SynExpr.InferredDowncast(e, r) -> walkExpr r e
            | SynExpr.AddressOf(_, e, _, r) -> walkExpr r e
            | SynExpr.JoinIn(e1, _, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.YieldOrReturn(_, e, r) -> walkExpr r e
            | SynExpr.YieldOrReturnFrom(_, e, r) -> walkExpr r e
            | SynExpr.LetOrUseBang(_, _, _, _, e1, e2, r) -> List.exists (walkExpr r) [e1; e2]
            | SynExpr.DoBang(e, r) -> walkExpr r e
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
            | SynMemberDefn.Member(binging, r) -> walkBinding r binging
            | SynMemberDefn.ImplicitCtor(_, attrs, pats, _, _) -> 
                List.exists walkAttribute attrs
                || List.exists walkSimplePat pats
            | SynMemberDefn.ImplicitInherit(t, e, _, r) -> walkType r t || walkExpr r e
            | SynMemberDefn.LetBindings(bindings, _, _, r) -> List.exists (walkBinding r) bindings
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
                || walkExpr r e
            | _ -> false

        and walkTypeDefn (TypeDefn(_, repr, members, r)) =
            ifPosInRange r (fun _ -> 
                match repr with
                | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.exists walkMember defns
                | _ -> false
                || List.exists walkMember members)

        and walkSynModuleDecl (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(_, modules, _, range) ->
                ifPosInRange range (fun _ -> List.exists walkSynModuleDecl modules)
            | SynModuleDecl.Open _ -> false
            | SynModuleDecl.Let (_, bindings, r) ->
                ifPosInRange r (fun _ -> List.exists (walkBinding r) bindings)
            | SynModuleDecl.Types (types, r) -> 
                ifPosInRange r (fun _ -> List.exists walkTypeDefn types)
            | _ -> false

        let res = 
            match ast with 
            | ParsedInput.SigFile _ -> false
            | ParsedInput.ImplFile input -> walkImplFileInput input
        if not res then
            printfn "Not an entity for (%d, %d). Ast = %A" pos.Line pos.Column ast
        res

    let findNearestOpenStatementBlock (currentLine: int) (ast: ParsedInput) = 
        let result = ref None
        let modules = ResizeArray<LongIdent * EndLine>()  
         
        let doRange (ns: LongIdent) line col = 
            if line <= currentLine then
                match !result with
                | None -> result := Some (ns, { Line = line; Col = col})
                | Some (oldNs, { Line = oldLine}) when oldLine < line -> 
                    result := Some ((match ns with [] -> oldNs | _ -> ns), { Line = line; Col = col }) 
                | _ -> ()

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.iter (walkSynModuleOrNamespace []) moduleOrNamespaceList

        and walkSynModuleOrNamespace (parent: LongIdent) (SynModuleOrNamespace(ident, _, decls, _, _, _, range)) =
            if range.EndLine >= currentLine then
                let fullIdent = parent @ ident
                doRange fullIdent range.StartLine range.StartColumn
                modules.Add (fullIdent, range.EndLine)
                List.iter (walkSynModuleDecl fullIdent) decls

        and walkSynModuleDecl (parent: LongIdent) (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace parent fragment
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, ident, _, _, _, _), modules', _, range) ->
                let fullIdent = parent @ ident
                modules.Add (fullIdent, range.EndLine) 
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
            |> Seq.map (fun (m, endLine) -> String.Join (".", m |> Seq.map string), endLine) 
            |> Seq.sortBy (fun (m, _) -> -m.Length)
            |> Seq.toList
        fun (ident: Ident) (entityFullName: EntityFullName) ->
            res 
            |> Option.bind (fun (ns, pos) -> Entity.tryCreate ns ident entityFullName |> Option.map (fun e -> e, pos))
            |> Option.map (fun (entity, pos) ->
                entity,
                match modules 
                      |> List.filter (fun (m, _) -> 
                            entityFullName.StartsWith m 
                            && entityFullName.Length > m.Length && entityFullName.[m.Length] = '.') with
                | [] -> pos
                | (_, endLine) as _m :: _ ->
                    //printfn "All modules: %A, Win module: %A" modules m
                    { pos with Line = endLine + 1 })
