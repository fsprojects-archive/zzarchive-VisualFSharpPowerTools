module FSharpVSPowerTools.SourceCodeClassifier

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

type Category =
    | ReferenceType
    | ValueType
    | PatternCase
    | Function
    | MutableVar
    | Quotation
    | Module
    | Other
    override x.ToString() = sprintf "%A" x

let internal getCategory (symbolUse: FSharpSymbolUse) =
    let symbol = symbolUse.Symbol
    
    let isOperator (name: string) =
        if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4 then
            name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
        else false

    let rec getEntityAbbreviatedType (entity: FSharpEntity) =
        match entity with
        | AbbreviatedType (TypeWithDefinition def) -> getEntityAbbreviatedType def
        | AbbreviatedType typ -> entity, Some typ
        | _ -> entity, None

    let rec getAbbreviatedType (fsharpType: FSharpType) =
        if fsharpType.IsAbbreviation then
            let typ = fsharpType.AbbreviatedType
            if typ.HasTypeDefinition then getAbbreviatedType typ
            else fsharpType
        else fsharpType

    let isReferenceCell (fsharpType: FSharpType) = 
        let ty = getAbbreviatedType fsharpType
        ty.HasTypeDefinition 
        && ty.TypeDefinition.IsFSharpRecord
        && ty.TypeDefinition.FullName = "Microsoft.FSharp.Core.FSharpRef`1"

    match symbol with
    | :? FSharpGenericParameter
    | :? FSharpUnionCase
    | :? FSharpActivePatternCase -> 
        PatternCase

    | :? FSharpField as f ->
        if f.IsMutable || isReferenceCell f.FieldType then MutableVar
        else Other

    | :? FSharpEntity as entity ->
        //debug "%A (type: %s)" e (e.GetType().Name)
        let e, ty = getEntityAbbreviatedType entity
        if e.IsEnum || e.IsValueType 
           || hasAttribute<_, MeasureAnnotatedAbbreviationAttribute> e then ValueType
        elif 
            
           (e.IsClass && (not e.IsStaticInstantiation 
                          // here we must use "entity", not "e" because "entity" could be an alias, but "e" is certainly cannot
                          || entity.IsFSharpAbbreviation)) 
           || e.IsDelegate || e.IsFSharpExceptionDeclaration
           || e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsMeasure 
           || ((e.IsProvided || e.IsProvidedAndErased || e.IsProvidedAndGenerated) && e.CompiledName = e.DisplayName)
           || (e.IsFSharp && e.IsOpaque && not e.IsFSharpModule && not e.IsNamespace)
           || e.IsByRef || e.IsArrayType then
            ReferenceType
        elif e.IsFSharpModule then Module
        else 
            match ty with
            | Some t when t.IsTupleType -> ReferenceType
            | _ -> Other
    
    | :? FSharpMemberFunctionOrValue as func ->
        //debug "%A (type: %s)" mfov (mfov.GetType().Name)
        if func.CompiledName = ".ctor" then 
            if func.EnclosingEntity.IsValueType || func.EnclosingEntity.IsEnum then ValueType
            else ReferenceType
        elif func.FullType.IsFunctionType && not func.IsPropertyGetterMethod && not func.IsPropertySetterMethod
             && not symbolUse.IsFromComputationExpression then 
            if isOperator func.DisplayName then Other
            else Function
        elif func.IsMutable || isReferenceCell func.FullType then MutableVar
        else Other
    
    | _ ->
        debug "Unknown symbol: %A (type: %s)" symbol (symbol.GetType().Name)
        Other

type CategorizedColumnSpan =
    { Category: Category
      WordSpan: WordSpan }

// If "what" span is entirely included in "from" span, then truncate "from" to the end of "what".
// Example: for ReferenceType symbol "System.Diagnostics.DebuggerDisplay" there are "System", "Diagnostics" and "DebuggerDisplay"
// plane symbols. After excluding "System", we get "Diagnostics.DebuggerDisplay",
// after excluding "Diagnostics", we get "DebuggerDisplay" and we are done.
let excludeWordSpan from what =
    if what.EndCol < from.EndCol && what.EndCol > from.StartCol then
        { from with StartCol = what.EndCol + 1 } // the dot between parts
    else from
 
let getCategoriesAndLocations (allSymbolsUses: FSharpSymbolUse[], untypedAst: ParsedInput option, lexer: ILexer) =
    let allSymbolsUses' =
        allSymbolsUses
        |> Array.choose (fun su ->
            let r = su.RangeAlternate
            lexer.GetSymbolAtLocation (r.End.Line - 1) (r.End.Column - 1)
            |> Option.bind (fun sym -> 
                match sym.Kind with
                | SymbolKind.Ident ->
                    // FCS returns inaccurate ranges for multiline method chains
                    // Specifically, only the End is right. So we use the lexer to find Start for such symbols.
                    if r.StartLine < r.EndLine then
                        Some (su, { Line = r.End.Line; StartCol = r.End.Column - sym.Text.Length; EndCol = r.End.Column })
                    else 
                        Some (su, { Line = r.End.Line; StartCol = r.Start.Column; EndCol = r.End.Column })
                | _ -> None))
      
    // index all symbol usages by LineNumber 
    let wordSpans = 
        allSymbolsUses'
        |> Seq.map snd
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
                if (span.EndCol - span.StartCol) - symbolUse.Symbol.DisplayName.Length > 0 then
                    // The span is wider that the simbol's display name.
                    // This means that we have not managed to extract last part of a long ident accurately.
                    // Particulary, it happens for chained method calls like Guid.NewGuid().ToString("N").Substring(1).
                    // So we get ident from the lexer.
                    match lexer.GetSymbolAtLocation (span.Line - 1) (span.EndCol - 1) with
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
                else Some { Category = getCategory symbolUse; WordSpan = span' }
        
            categorizedSpan)
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

    untypedAst |> Option.iter (fun ast ->
        match ast with
        | ParsedInput.ImplFile(implFile) ->
            let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
            visitModulesAndNamespaces modules
        | _ -> ()
    )

    //printfn "AST: %A" untypedAst
    
    let trimWhitespaces = 
        Seq.skipWhile (fun t -> t.CharClass = TokenCharKind.WhiteSpace) >> Seq.toList

    let quotations =
        !quotationRanges 
        |> Seq.map (fun (r: Range.range) -> 
            if r.EndLine = r.StartLine then
                seq [ { Category = Quotation
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
                            |> Seq.skipWhile (fun t -> t <> rquote)
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

                        Some { Category = Quotation
                               WordSpan = { Line = line
                                            StartCol = minCol
                                            EndCol = maxCol }}))
        |> Seq.concat
        |> Seq.toArray

    let allSpans = spansBasedOnSymbolsUses |> Array.append quotations
    
    //for span in allSpans do
       //debug "-=O=- %A" span

    allSpans
