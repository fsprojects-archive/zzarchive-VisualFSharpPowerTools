module FSharpVSPowerTools.CodeGeneration.UnionMatchCaseGenerator

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<NoEquality; NoComparison>]
type MatchExpr = {
    MatchWithRange: range
    Expr: SynExpr
    MatchedExpr: SynExpr
    Clauses: SynMatchClause list
}

type UnionMatchCasesInsertionParams = {
    InsertionPos: pos
    FirstClauseStartsWithPipe: bool
}


[<NoComparison>]
type private Context = {
    Writer: ColumnIndentedTextWriter
    /// A single-line skeleton for each case
    CaseDefaultValue: string
    UnionTypeName: string
    RequireQualifiedAccess: bool
}

let getWrittenCases (matchExpr: MatchExpr) =
    matchExpr.Clauses
    |> List.choose (function
        | Clause(SynPat.LongIdent(LongIdentWithDots(unionCaseLongIdent, _), _, _, _, _, _),
                 None, _, _, _) when unionCaseLongIdent.Length > 0 ->
            Some (unionCaseLongIdent.Item (unionCaseLongIdent.Length - 1)).idText
        | _ -> None)
    |> Set.ofList

let shouldGenerateUnionMatchCases (matchExpr: MatchExpr) (entity: FSharpEntity) =
    let caseCount = entity.UnionCases.Count
    let writtenCaseCount =
        getWrittenCases matchExpr
        |> Set.count
    caseCount > 0 && writtenCaseCount < caseCount

let private formatCase (ctxt: Context) writePipeBefore (case: FSharpUnionCase) =
    let writer = ctxt.Writer
    let name = 
        if ctxt.RequireQualifiedAccess then
            sprintf "%s.%s" ctxt.UnionTypeName case.Name
        else 
            case.Name

    let paramsPattern =
        let unionCaseFieldsCount = case.UnionCaseFields.Count
        if unionCaseFieldsCount <= 0 then
            ""
        else
            [|
                yield "(_"
                for _ in 1 .. unionCaseFieldsCount - 1 do
                    yield ", _"
                yield ")"
            |]
            |> String.Concat

    if writePipeBefore then
        writer.WriteLine("| {0}{1} -> {2}", name, paramsPattern, ctxt.CaseDefaultValue)
        writer.Write("")
    else
        writer.WriteLine("{0}{1} -> {2}", name, paramsPattern, ctxt.CaseDefaultValue)
        writer.Write("| ")

let formatMatchExpr insertionParams (caseDefaultValue: string)
                    (matchExpr: MatchExpr) (entity: FSharpEntity) =
    assert entity.IsFSharpUnion
    use writer = new ColumnIndentedTextWriter()
    let ctxt =
        { UnionTypeName = entity.DisplayName
          RequireQualifiedAccess = hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes 
          Writer = writer
          CaseDefaultValue = caseDefaultValue}

    let casesWritten = getWrittenCases matchExpr
    let casesToWrite =
        entity.UnionCases
        |> Seq.filter (fun case -> not <| casesWritten.Contains case.Name)

    let indentValue = insertionParams.InsertionPos.Column

    writer.Indent indentValue

    let writePipeBefore = insertionParams.FirstClauseStartsWithPipe
    match List.ofSeq casesToWrite with
    | [] -> ()
    | firstCase :: otherCases ->
        formatCase ctxt writePipeBefore firstCase
        otherCases
        |> List.iter (formatCase ctxt writePipeBefore)
    
    // Scenario when first case doesn't start with pipe
    // match x with Case3 -> ()
    //              ^
    //
    // match x with Case1 -> ()
    //              | Case3 -> () 
    //                ^
    //
    // match x with Case1 -> ()
    //              | Case2 -> ()
    //              | Case3 -> ()
    //                ^

    // TODO: remove comment if not necessary
    // special case when fields are already written
//    if not casesWritten.IsEmpty && casesWritten.Count < entity.UnionCases.Count then
//        writer.WriteLine("")
//        writer.Write("")

    writer.Dump()

let tryFindMatchExpression (pos: pos) (parsedInput: ParsedInput) =
    let inline getIfPosInRange range f =
        if rangeContainsPos range pos then f()
        else None
    
    let rec walkImplFileInput (ParsedImplFileInput(_name, _isScript, _fileName, _scopedPragmas, _hashDirectives, moduleOrNamespaceList, _)) = 
        List.tryPick walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace(SynModuleOrNamespace(_lid, _isModule, decls, _xmldoc, _attributes, _access, range)) =
        getIfPosInRange range (fun () ->
            List.tryPick walkSynModuleDecl decls
        )

    and walkSynModuleDecl(decl: SynModuleDecl) =
        getIfPosInRange decl.Range (fun () ->
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
        )

    and walkSynTypeDefn(TypeDefn(_componentInfo, representation, members, range)) = 
        getIfPosInRange range (fun () ->
            walkSynTypeDefnRepr representation
            |> Option.orElse (List.tryPick walkSynMemberDefn members)        
        )

    and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
        getIfPosInRange typeDefnRepr.Range (fun () ->
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
                List.tryPick walkSynMemberDefn members
            | SynTypeDefnRepr.Simple(_repr, _range) -> 
                None
        )

    and walkSynMemberDefn (memberDefn: SynMemberDefn) =
        getIfPosInRange memberDefn.Range (fun () ->
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
        )

    and walkBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, _headPat, retTy, expr, _bindingRange, _seqPoint) as binding) =
        getIfPosInRange binding.RangeOfBindingAndRhs (fun () -> walkExpr expr)

    and walkExpr expr =
        getIfPosInRange expr.Range (fun () ->
            match expr with
            | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Const(_synConst, _range) -> 
                None

            | SynExpr.Typed(synExpr, _, _)
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
                let fieldExprList =
                    fields
                    |> List.choose (fun (_, fieldExprOpt, _) -> fieldExprOpt)

                match copyOpt with
                | Some(copyExpr, _blockSeparator) ->
                    List.tryPick walkExpr (copyExpr :: fieldExprList)
                | None ->
                    List.tryPick walkExpr fieldExprList

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
            | SynExpr.Match(sequencePointInfoForBinding, synExpr, synMatchClauseList, _, _range) as matchExpr ->
                walkExpr synExpr
                |> Option.orElse (synMatchClauseList |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e))
                |> Option.orElse (
                    match sequencePointInfoForBinding with
                    | SequencePointAtBinding range ->
                        Some { MatchWithRange = range
                               Expr = matchExpr
                               MatchedExpr = synExpr
                               Clauses = synMatchClauseList }
                    | _ -> None
                )

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
        )
    
    and walkSynInterfaceImpl (InterfaceImpl(_synType, synBindings, _range)) =
        List.tryPick walkBinding synBindings

    match parsedInput with
    | ParsedInput.SigFile _input -> None
    | ParsedInput.ImplFile input -> walkImplFileInput input

let tryFindMatchExprInBufferAtPos (codeGenService: ICodeGenerationService<'Project, 'Pos, 'Range>) project (pos: 'Pos) document =
    async {
        let! parseResults =
            codeGenService.ParseFileInProject(document, project)
        
        return
            parseResults.ParseTree
            |> Option.bind (tryFindMatchExpression (codeGenService.ExtractFSharpPos(pos)))
    }

let tryFindTokenInRange
    (codeGenService: ICodeGenerationService<'Project, 'Pos, 'Range>) project
    (range: 'Range) (document: IDocument) (predicate: TokenInformation -> bool)  =
    let lines = seq {
        for line in (int range.StartLine) .. (int range.EndLine) do
            yield! codeGenService.TokenizeLine(project, document, (line * 1<Line1>))
                   |> List.map (fun tokenInfo -> line * 1<Line1>, tokenInfo)
    }

    lines
    |> Seq.tryFind (fun (line1, tokenInfo) ->
        if range.StartLine = range.EndLine then
            tokenInfo.LeftColumn >= range.StartColumn &&
            tokenInfo.RightColumn < range.EndColumn &&
            predicate tokenInfo
        elif range.StartLine = line1 then
            tokenInfo.LeftColumn >= range.StartColumn &&
            predicate tokenInfo
        elif line1 = range.EndLine then
            tokenInfo.RightColumn < range.EndColumn &&
            predicate tokenInfo
        else
            predicate tokenInfo
    )
    |> Option.map (fun (line1, tokenInfo) ->
        tokenInfo, codeGenService.CreateRange(startLine=line1,
                                              startColumn=tokenInfo.LeftColumn,
                                              endLine=line1,
                                              endColumn=tokenInfo.RightColumn + 1)
    )

let tryFindBarTokenInRange
    (codeGenService: ICodeGenerationService<'Project, 'Pos, 'Range>) project
    (range: 'Range) (document: IDocument) =
    tryFindTokenInRange codeGenService project range document
        (fun tokenInfo -> tokenInfo.TokenName = "BAR")

let tryGetRangeBetweenWithAndFirstClause (codeGenService: ICodeGenerationService<_, _, 'Range>) matchExpr =
    maybe {
        let! fstClause =
            match matchExpr.Clauses with
            | hd :: _ -> Some hd
            | _ -> None

        let range = unionRanges matchExpr.MatchWithRange.EndRange fstClause.Range.StartRange
        return codeGenService.CreateRange(range.StartLine * 1<Line1>,
                                          range.StartColumn,
                                          range.EndLine * 1<Line1>,
                                          range.EndColumn)
    }

let getInsertionParams codeGenService project document (matchExpr: MatchExpr) =
    let barTokenFindResult =
        maybe {
            let! rangeBetweenWithAndClause =
                tryGetRangeBetweenWithAndFirstClause codeGenService matchExpr
            return! tryFindBarTokenInRange codeGenService project rangeBetweenWithAndClause document
        }

    match barTokenFindResult, matchExpr.Clauses with
    | Some(_, tokenRange), _ ->
        // Before first clause's '|'
        { InsertionPos = Pos.fromZ (int tokenRange.StartLine - 1) tokenRange.StartColumn
          FirstClauseStartsWithPipe = true }
    | None, [] -> 
        // After 'with' keyword
        { InsertionPos = Pos.fromZ (matchExpr.MatchWithRange.EndLine - 1)
                                   (matchExpr.MatchWithRange.EndColumn)
          FirstClauseStartsWithPipe = true }
    | None, fstClause :: _ ->
        // Before first clause, which doesn't start by '|'
        { InsertionPos = Pos.fromZ (fstClause.Range.StartLine - 1)
                                   (fstClause.Range.StartColumn)
          FirstClauseStartsWithPipe = false }

let getIndentValue (matchExpr: MatchExpr) =
    matchExpr.MatchWithRange.StartColumn

let tryFindMatchCaseInsertionParamsAtPos (codeGenService: ICodeGenerationService<'Project, 'Pos, 'Range>) project (pos: 'Pos) document =
    asyncMaybe {
        let! matchExpr = tryFindMatchExprInBufferAtPos codeGenService project pos document
        let insertionParams = getInsertionParams codeGenService project document matchExpr

        return matchExpr, insertionParams
    }

let tryFindUnionTypeDefinitionFromPos (codeGenService: ICodeGenerationService<'Project, 'Pos, 'Range>) project (pos: 'Pos) document =
    asyncMaybe {
        let! matchExpr, insertionParams = tryFindMatchCaseInsertionParamsAtPos codeGenService project pos document
        let! symbolRange, _symbol, symbolUse = codeGenService.GetSymbolAndUseAtPositionOfKind(project, document, pos, SymbolKind.Ident)

        match symbolUse.Symbol with
        | :? FSharpUnionCase as case when case.ReturnType.TypeDefinition.IsFSharpUnion ->
            return! Some (symbolRange, matchExpr, case.ReturnType.TypeDefinition, insertionParams) |> liftMaybe
        | :? FSharpEntity as entity when entity.IsFSharpUnion ->
            return! Some (symbolRange, matchExpr, entity, insertionParams) |> liftMaybe
        | _ ->
            return! None |> liftMaybe
    }