module FSharpVSPowerTools.CodeGeneration.RecordStubGenerator

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.CodeDom.Compiler
open FSharpVSPowerTools
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

type CopyExpressionOpt = (SynExpr * BlockSeparator) option
type FieldExpressionList = (RecordFieldName * SynExpr option * BlockSeparator option) list

[<NoEquality; NoComparison>]
type RecordBinding =
    | TypedRecordBinding of SynType * SynExpr * CopyExpressionOpt * FieldExpressionList
    | QualifiedFieldRecordBinding of SynExpr * CopyExpressionOpt * FieldExpressionList
    | NonQualifiedFieldRecordBinding of SynExpr * CopyExpressionOpt * FieldExpressionList

    member x.CopyExpressionOpt =
        match x with
        | TypedRecordBinding(_, _, copyExprOpt, _)
        | QualifiedFieldRecordBinding(_, copyExprOpt, _)
        | NonQualifiedFieldRecordBinding(_, copyExprOpt, _) -> copyExprOpt

    member x.Expression =
        match x with
        | TypedRecordBinding(_, expr, _, _)
        | QualifiedFieldRecordBinding(expr, _, _)
        | NonQualifiedFieldRecordBinding(expr, _, _) -> expr

    member x.FieldExpressionList =
        match x with
        | TypedRecordBinding(_, _, _, fieldExpressions)
        | QualifiedFieldRecordBinding(_, _, fieldExpressions)
        | NonQualifiedFieldRecordBinding(_, _, fieldExpressions) -> fieldExpressions

[<RequireQualifiedAccess>]
[<NoComparison>]
type RecordStubsInsertionPosition =
    /// let record = {<insert-here>}
    | AfterLeftBrace of pos

    /// let y = { x with<insert-here> }
    | AfterCopyExpression of pos

    /// let x = { <insert-here>Field1 = ... }
    | BeforeFirstField of pos

    member x.Position =
        match x with
        | AfterLeftBrace pos
        | AfterCopyExpression pos
        | BeforeFirstField pos -> pos

    static member FromRecordExpression (expr: RecordBinding) =
        match expr.FieldExpressionList with
        | [] ->
            match expr.CopyExpressionOpt with
            | None ->
                let exprRange = expr.Expression.Range
                AfterLeftBrace(Pos.fromZ (exprRange.StartLine - 1) (exprRange.StartColumn + 1))
            | Some(_toCopy, (withSeparator, _)) ->
                AfterCopyExpression(withSeparator.End)

        | fstFieldInfo :: _ ->
            let ((fstField, _), _, _) = fstFieldInfo
            BeforeFirstField(fstField.Range.Start)


[<NoComparison>]
type private Context = {
    Writer: ColumnIndentedTextWriter
    /// Indentation inside method bodies
    IndentValue: int
    /// A single-line skeleton for each field
    FieldDefaultValue: string
    RecordTypeName: string
    RequireQualifiedAccess: bool
    PrependExtraSpace: bool
}

let private formatField (ctxt: Context) isFirstField (field: FSharpField) =
    let writer = ctxt.Writer

    if not isFirstField then
        writer.WriteLine("")
    
    let name = 
        if ctxt.RequireQualifiedAccess then
            sprintf "%s.%s" ctxt.RecordTypeName field.Name
        else 
            field.Name
    
    let prependedSpace = if ctxt.PrependExtraSpace then " " else ""
    
    writer.Write("{0}{1} = {2}", prependedSpace, name, ctxt.FieldDefaultValue)

let formatRecord (insertionPos: RecordStubsInsertionPosition) indentValue (fieldDefaultValue: string)
                 (entity: FSharpEntity)
                 (fieldsWritten: (RecordFieldName * _ * Option<_>) list) =
    assert entity.IsFSharpRecord
    use writer = new ColumnIndentedTextWriter()
    let startColumn = insertionPos.Position.Column
    let ctxt =
        let prependExtraSpace =
            match insertionPos with
            | RecordStubsInsertionPosition.AfterCopyExpression _
            | RecordStubsInsertionPosition.AfterLeftBrace _ -> true
            | RecordStubsInsertionPosition.BeforeFirstField _ -> false

        { RecordTypeName = entity.DisplayName
          RequireQualifiedAccess = hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes 
          Writer = writer
          IndentValue = indentValue
          FieldDefaultValue = fieldDefaultValue
          PrependExtraSpace = prependExtraSpace }

    let fieldsWritten =
        fieldsWritten
        |> List.collect (function
            ((fieldName, _), _, _) ->
                // Extract <Field> in qualified identifiers: A.B.<Field> = ...
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
        if not <| rangeContainsPos range pos then
            None
        else
            List.tryPick walkSynModuleDecl decls

    and walkSynModuleDecl(decl: SynModuleDecl) =
        if not <| rangeContainsPos decl.Range pos then
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
        if not <| rangeContainsPos range pos then
            None
        else
            walkSynTypeDefnRepr representation
            |> Option.orElse (List.tryPick walkSynMemberDefn members)        

    and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
        if not <| rangeContainsPos typeDefnRepr.Range pos then
            None
        else
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
                List.tryPick walkSynMemberDefn members
            | SynTypeDefnRepr.Simple(_repr, _range) -> 
                None

    and walkSynMemberDefn (memberDefn: SynMemberDefn) =
        if not <| rangeContainsPos memberDefn.Range pos then
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
        //debug "Walk Binding"
        if not <| rangeContainsPos binding.RangeOfBindingAndRhs pos then
            //debug "Not in range"
            None
        else
            //debug "In range (%A)" binding.RangeOfBindingAndRhs
            //debug "BindingReturnInfo: %A" retTy
            //debug "Expr: %A" expr
            match retTy with
            | Some(SynBindingReturnInfo(ty, _range, _attributes)) ->
                //debug "ReturnTypeInfo: %A" ty
                match expr with
                // Situation 1:
                // NOTE: 'buggy' parse tree when a type annotation is given before the '=' (but workable corner case)
                // Ex: let x: MyRecord = { f1 = e1; f2 = e2; ... }
                | SynExpr.Typed(SynExpr.Record(_inheritOpt, copyOpt, fields, _range0), _, _range1) ->
                    fields 
                    |> List.tryPick walkRecordField
                    |> Option.orElse (Some(TypedRecordBinding(ty, expr, copyOpt, fields)))
                | _ -> walkExpr expr
            | None ->
                walkExpr expr

    and walkExpr expr =
        if not <| rangeContainsPos expr.Range pos then 
            None
        else
            match expr with
            | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Const(_synConst, _range) -> 
                None

            | SynExpr.Typed(synExpr, ty, _) ->
                match synExpr with
                // Situation 1:
                // NOTE: 'buggy' parse tree when a type annotation is given before the '=' (but workable corner case)
                // Ex: let x: MyRecord = { f1 = e1; f2 = e2; ... }
                | SynExpr.Record(_inheritOpt, copyOpt, fields, _range) ->
                    fields 
                    |> List.tryPick walkRecordField
                    |> Option.orElse (Some(TypedRecordBinding(ty, expr, copyOpt, fields)))
                | _ -> 
                walkExpr synExpr

            | SynExpr.Paren(synExpr, _, _, _)
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

            | SynExpr.Record(_inheritOpt, copyOpt, fields, _range) ->
                fields 
                |> List.tryPick walkRecordField
                |> Option.orElse (
                    match fields with
                    | ((fieldName, true), _, _) :: _ when fieldName.Lid.Length >= 2 ->
                            Some(QualifiedFieldRecordBinding(expr, copyOpt, fields))
                    | ((fieldName, true), _, _) :: _ when fieldName.Lid.Length = 1 ->
                        Some(NonQualifiedFieldRecordBinding(expr, copyOpt, fields))
                    | _ -> None)

            | SynExpr.ObjExpr(_ty, _baseCallOpt, binds, ifaces, _range1, _range2) -> 
                List.tryPick walkBinding binds
                |> Option.orElse (List.tryPick walkSynInterfaceImpl ifaces)

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

    and walkRecordField ((longIdents, _): RecordFieldName, synExprOpt, _) = 
        if rangeContainsPos longIdents.Range pos then
            None
        else
            Option.bind walkExpr synExprOpt
    
    and walkSynInterfaceImpl (InterfaceImpl(_synType, synBindings, _range)) =
        List.tryPick walkBinding synBindings

    match parsedInput with
    | ParsedInput.SigFile _input -> None
    | ParsedInput.ImplFile input -> walkImplFileInput input