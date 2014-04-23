module FSharpVSPowerTools.Core.CodeGeneration.RecordStubGenerator

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.CodeDom.Compiler
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

#if INTERACTIVE
let debug x =
    Printf.ksprintf (printfn "[RecordStubGenerator] %s") x

let mutable debugObject: obj = null
let inline setDebugObject (o: 'a) = debugObject <- o
#else
let inline setDebugObject (_: 'a) = ()
#endif


[<NoEquality; NoComparison>]
type RecordBinding =
    | TypedRecordBinding of SynType * SynExpr * (RecordFieldName * SynExpr option * BlockSeparator option) list
    | QualifiedFieldRecordBinding of SynExpr * (RecordFieldName * SynExpr option * BlockSeparator option) list

[<NoComparison>]
type private Context = {
    Writer: ColumnIndentedTextWriter
    /// Indentation inside method bodies
    IndentValue: int
    /// A single-line skeleton for each field
    FieldDefaultValue: string
    DisplayContext: FSharpDisplayContext
}

// TODO: copy-pasted from InterfaceStubGeneration
let private (|IndexerArg|) = function
    | SynIndexerArg.Two(e1, e2) -> [e1; e2]
    | SynIndexerArg.One e -> [e]

// TODO: copy-pasted from InterfaceStubGeneration
let private (|IndexerArgList|) xs =
    List.collect (|IndexerArg|) xs

// TODO: copy-pasted from InterfaceStubGeneration
let private inRange range pos = 
    AstTraversal.rangeContainsPosLeftEdgeInclusive range pos

let private formatField (ctxt: Context) isFirstField (field: FSharpField) =
    let writer = ctxt.Writer

    if not isFirstField then
        writer.WriteLine("")
    
    writer.Write(" {0} = {1}", field.Name, ctxt.FieldDefaultValue)

let formatRecord startColumn indentValue (fieldDefaultValue: string)
                 (displayContext: FSharpDisplayContext) (entity: FSharpEntity)
                 (fieldsWritten: (RecordFieldName * _ * Option<_>) list) =
    assert entity.IsFSharpRecord

    use writer = new ColumnIndentedTextWriter()
    let ctxt: Context =
        { Writer = writer
          IndentValue = indentValue
          FieldDefaultValue = fieldDefaultValue
          DisplayContext = displayContext }

    let fieldsWritten =
        fieldsWritten
        |> List.collect (function
            ((fieldName, _), _, _) ->
                if fieldName.Lid.Length > 0 then
                    [(fieldName.Lid.Item (fieldName.Lid.Length - 1)).idText]
                else [])
        |> Set.ofList

    let fieldsToWrite =
        entity.FSharpFields
        |> Seq.filter (fun field -> not <| fieldsWritten.Contains field.Name)

    writer.Indent startColumn
    match List.ofSeq fieldsToWrite with
    | [] -> ()
    | firstField :: otherFields ->
        formatField ctxt true firstField
        otherFields
        |> List.iter (formatField ctxt false)

    // special case when fields are already written
    if not fieldsWritten.IsEmpty && fieldsWritten.Count < entity.FSharpFields.Count then
        writer.WriteLine("")
        writer.Write("")

    writer.Dump()


let tryFindRecordBinding (pos: pos) (parsedInput: ParsedInput) =
    let rec walkImplFileInput (ParsedImplFileInput(_name, _isScript, _fileName, _scopedPragmas, _hashDirectives, moduleOrNamespaceList, _)) = 
        List.tryPick walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace(SynModuleOrNamespace(_lid, _isModule, decls, _xmldoc, _attributes, _access, range)) =
        if not <| inRange range pos then
            None
        else
            List.tryPick walkSynModuleDecl decls

    and walkSynModuleDecl(decl: SynModuleDecl) =
        if not <| inRange decl.Range pos then
            None
        else
            match decl with
            | SynModuleDecl.Exception(ExceptionDefn(_repr, synMembers, _defnRange), _range) -> 
                List.tryPick walkSynMemberDefn synMembers
            | SynModuleDecl.Let(_isRecursive, bindings, _range) ->
                List.tryPick walkBinding bindings
            | SynModuleDecl.ModuleAbbrev(_lhs, _rhs, _range) ->
                None
            | SynModuleDecl.NamespaceFragment(fragment) ->
                walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(_componentInfo, modules, _isContinuing, _range) ->
                List.tryPick walkSynModuleDecl modules
            | SynModuleDecl.Types(typeDefs, _range) ->
                List.tryPick walkSynTypeDefn typeDefs
            | SynModuleDecl.DoExpr (_, expr, _) ->
                walkExpr expr
            | SynModuleDecl.Attributes _
            | SynModuleDecl.HashDirective _
            | SynModuleDecl.Open _ -> 
                None

    and walkSynTypeDefn(TypeDefn(_componentInfo, representation, members, range)) = 
        if not <| inRange range pos then
            None
        else
            walkSynTypeDefnRepr representation
            |> Option.orElse (List.tryPick walkSynMemberDefn members)        

    and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
        if not <| inRange typeDefnRepr.Range pos then
            None
        else
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
                List.tryPick walkSynMemberDefn members
            | SynTypeDefnRepr.Simple(_repr, _range) -> 
                None

    and walkSynMemberDefn (memberDefn: SynMemberDefn) =
        if not <| inRange memberDefn.Range pos then
            None
        else
            match memberDefn with
            | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range) ->
                None
            | SynMemberDefn.AutoProperty(_attributes, _isStatic, _id, _type, _memberKind, _memberFlags, _xmlDoc, _access, expr, _r1, _r2) ->
                walkExpr expr
            | SynMemberDefn.Interface(_, members, _range) ->
                Option.bind (List.tryPick walkSynMemberDefn) members
            | SynMemberDefn.Member(binding, _range) ->
                walkBinding binding
            | SynMemberDefn.NestedType(typeDef, _access, _range) -> 
                walkSynTypeDefn typeDef
            | SynMemberDefn.ValField(_field, _range) ->
                None
            | SynMemberDefn.LetBindings(bindings, _isStatic, _isRec, _range) ->
                List.tryPick walkBinding bindings
            | SynMemberDefn.Open _
            | SynMemberDefn.ImplicitInherit _
            | SynMemberDefn.Inherit _
            | SynMemberDefn.ImplicitCtor _ -> 
                None

    and walkBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, _headPat, retTy, expr, _bindingRange, _seqPoint) as binding) =
        debug "Walk Binding"
        if not <| inRange binding.RangeOfBindingAndRhs pos then
            debug "Not in range"
            None
        else
            debug "In range (%A)" binding.RangeOfBindingAndRhs
            debug "BindingReturnInfo: %A" retTy
            debug "Expr: %A" expr
            setDebugObject binding
            match retTy with
            | Some(SynBindingReturnInfo(ty, _range, _attributes)) ->
                debug "ReturnTypeInfo: %A" ty

                match expr with
                // Situation 1:
                // NOTE: 'buggy' parse tree when a type annotation is given before the '=' (but workable corner case)
                // Ex: let x: MyRecord = { f1 = e1; f2 = e2; ... }
                | SynExpr.Typed(SynExpr.Record(_inheritOpt, _copyOpt, fields, _range),
                                _,
                                __range) ->
                    // TODO: we'll possibly have to look further down the tree
                    Some(TypedRecordBinding(ty, expr, fields))
                | _ -> walkExpr expr
            | None ->
                match expr with
                // let x = { f1 = e1; f2 = e2; ... }
                | SynExpr.Record(_inheritOpt, _copyOpt, fields, _range) ->
                    match fields with
                    | ((fieldName, true), _, _) :: _ when fieldName.Lid.Length >= 2 ->
                        // TODO: we'll possibly have to look further down the tree
                        Some(QualifiedFieldRecordBinding(expr, fields))
                    | _ -> None
                | _ -> walkExpr expr

    and walkExpr expr =
        if not <| inRange expr.Range pos then 
            None
        else
            match expr with
            | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Const(_synConst, _range) -> 
                None

            | SynExpr.Paren(synExpr, _, _, _)
            | SynExpr.Typed(synExpr, _, _)
            | SynExpr.New(_, _, synExpr, _)
            | SynExpr.ArrayOrListOfSeqExpr(_, synExpr, _)
            | SynExpr.CompExpr(_, _, synExpr, _)
            | SynExpr.Lambda(_, _, _, synExpr, _)
            | SynExpr.Lazy(synExpr, _)
            | SynExpr.Do(synExpr, _)
            | SynExpr.Assert(synExpr, _) ->
                walkExpr synExpr

            | SynExpr.Tuple(synExprList, _, _range)
            | SynExpr.ArrayOrList(_, synExprList, _range) ->
                List.tryPick walkExpr synExprList

            | SynExpr.Record(_inheritOpt, _copyOpt, _fields, _range) ->
                // TODO: look in all expressions
                None

            | SynExpr.ObjExpr(_ty, baseCallOpt, binds, ifaces, _range1, _range2) -> 
                match baseCallOpt with
                | None ->
                    List.tryPick walkBinding binds
                    |> Option.orElse (List.tryPick walkSynInterfaceImpl ifaces)
                | Some _ -> 
                    // TODO: cover object expressions of normal objects
                    None

            | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]
            | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _isFromSource, _synPat, synExpr1, synExpr2, _range) -> 
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.For(_sequencePointInfoForForLoop, _ident, synExpr1, _, synExpr2, synExpr3, _range) -> 
                List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

            | SynExpr.MatchLambda(_isExnMatch, _argm, synMatchClauseList, _spBind, _wholem) -> 
                synMatchClauseList |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e)
            | SynExpr.Match(_sequencePointInfoForBinding, synExpr, synMatchClauseList, _, _range) ->
                walkExpr synExpr
                |> Option.orElse (synMatchClauseList |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e))

            | SynExpr.App(_exprAtomicFlag, _isInfix, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.TypeApp(synExpr, _, _synTypeList, _commas, _, _, _range) -> 
                walkExpr synExpr

            | SynExpr.LetOrUse(_, _, synBindingList, synExpr, _range) -> 
                Option.orElse (List.tryPick walkBinding synBindingList) (walkExpr synExpr)

            | SynExpr.TryWith(synExpr, _range, _synMatchClauseList, _range2, _range3, _sequencePointInfoForTry, _sequencePointInfoForWith) -> 
                walkExpr synExpr

            | SynExpr.TryFinally(synExpr1, synExpr2, _range, _sequencePointInfoForTry, _sequencePointInfoForFinally) -> 
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Sequential(_sequencePointInfoForSeq, _, synExpr1, synExpr2, _range) -> 
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.IfThenElse(synExpr1, synExpr2, synExprOpt, _sequencePointInfoForBinding, _isRecovery, _range, _range2) -> 
                match synExprOpt with
                | Some synExpr3 ->
                    List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]
                | None ->
                    List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Ident(_ident) ->
                None
            | SynExpr.LongIdent(_, _longIdent, _altNameRefCell, _range) -> 
                None

            | SynExpr.LongIdentSet(_longIdent, synExpr, _range) ->
                walkExpr synExpr
            | SynExpr.DotGet(synExpr, _dotm, _longIdent, _range) -> 
                walkExpr synExpr

            | SynExpr.DotSet(synExpr1, _longIdent, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.DotIndexedGet(synExpr, IndexerArgList synExprList, _range, _range2) -> 
                Option.orElse (walkExpr synExpr) (List.tryPick walkExpr synExprList) 

            | SynExpr.DotIndexedSet(synExpr1, IndexerArgList synExprList, synExpr2, _, _range, _range2) -> 
                [ yield synExpr1
                  yield! synExprList
                  yield synExpr2 ]
                |> List.tryPick walkExpr

            | SynExpr.JoinIn(synExpr1, _range, synExpr2, _range2) ->
                List.tryPick walkExpr [synExpr1; synExpr2]
            | SynExpr.NamedIndexedPropertySet(_longIdent, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.DotNamedIndexedPropertySet(synExpr1, _longIdent, synExpr2, synExpr3, _range) ->  
                List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

            | SynExpr.TypeTest(synExpr, _synType, _range)
            | SynExpr.Upcast(synExpr, _synType, _range)
            | SynExpr.Downcast(synExpr, _synType, _range) ->
                walkExpr synExpr
            | SynExpr.InferredUpcast(synExpr, _range)
            | SynExpr.InferredDowncast(synExpr, _range) ->
                walkExpr synExpr
            | SynExpr.AddressOf(_, synExpr, _range, _range2) ->
                walkExpr synExpr
            | SynExpr.TraitCall(_synTyparList, _synMemberSig, synExpr, _range) ->
                walkExpr synExpr

            | SynExpr.Null(_range)
            | SynExpr.ImplicitZero(_range) -> 
                None

            | SynExpr.YieldOrReturn(_, synExpr, _range)
            | SynExpr.YieldOrReturnFrom(_, synExpr, _range) 
            | SynExpr.DoBang(synExpr, _range) -> 
                walkExpr synExpr

            | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _, _synPat, synExpr1, synExpr2, _range) -> 
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.LibraryOnlyILAssembly _
            | SynExpr.LibraryOnlyStaticOptimization _ 
            | SynExpr.LibraryOnlyUnionCaseFieldGet _
            | SynExpr.LibraryOnlyUnionCaseFieldSet _ ->
                None
            | SynExpr.ArbitraryAfterError(_debugStr, _range) -> 
                None

            | SynExpr.FromParseError(synExpr, _range)
            | SynExpr.DiscardAfterMissingQualificationAfterDot(synExpr, _range) -> 
                walkExpr synExpr
    
    and walkSynInterfaceImpl (InterfaceImpl(_synType, synBindings, _range)) =
        List.tryPick walkBinding synBindings

    match parsedInput with
    | ParsedInput.SigFile _input -> None
    | ParsedInput.ImplFile input -> walkImplFileInput input