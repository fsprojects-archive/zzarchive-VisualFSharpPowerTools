namespace FSharpVSPowerTools.Core

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.CodeDom.Compiler
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
[<NoEquality; NoComparison>]
type InterfaceData =
    | Interface of SynType * SynMemberDefns option
    | ObjExpr of SynType * SynBinding list

module InterfaceStubGenerator =
    type internal ColumnIndentedTextWriter() =
        let stringWriter = new StringWriter()
        let indentWriter = new IndentedTextWriter(stringWriter, " ")

        member x.Write(s: string, [<ParamArray>] objs: obj []) =
            indentWriter.Write(s, objs)

        member x.WriteLine(s: string, [<ParamArray>] objs: obj []) =
            indentWriter.WriteLine(s, objs)

        member x.Indent i = 
            indentWriter.Indent <- indentWriter.Indent + i

        member x.Unindent i = 
            indentWriter.Indent <- max 0 (indentWriter.Indent - i)

        member x.Writer = 
            indentWriter :> TextWriter

        member x.Dump() =
            indentWriter.InnerWriter.ToString()

        interface IDisposable with
            member x.Dispose() =
                stringWriter.Dispose()
                indentWriter.Dispose()  

    [<NoComparison>]
    type internal Context =
        {
            Writer: ColumnIndentedTextWriter
            /// Indentation inside method bodies
            Indentation: int
            /// Object identifier of the interface e.g. 'x', 'this', '__', etc.
            ObjectIdent: string
            /// A list of lines represents skeleton of each member
            MethodBody: string []
        }

    // Adapt from MetadataFormat module in FSharp.Formatting 

    let internal (|AllAndLast|_|) (list:'T list) = 
        if list.IsEmpty then 
            None
        else 
            let revd = List.rev list
            Some(List.rev revd.Tail, revd.Head)

    let internal isAttrib<'T> (attrib: FSharpAttribute)  =
        attrib.AttributeType.CompiledName = typeof<'T>.Name

    let internal hasAttrib<'T> (attribs: IList<FSharpAttribute>) = 
        attribs |> Seq.exists (fun a -> isAttrib<'T>(a))

    let internal (|MeasureProd|_|) (typ : FSharpType) = 
        if typ.HasTypeDefinition && typ.TypeDefinition.LogicalName = "*" && typ.GenericArguments.Count = 2 then
            Some (typ.GenericArguments.[0], typ.GenericArguments.[1])
        else None

    let internal (|MeasureInv|_|) (typ : FSharpType) = 
        if typ.HasTypeDefinition && typ.TypeDefinition.LogicalName = "/" && typ.GenericArguments.Count = 1 then 
            Some typ.GenericArguments.[0]
        else None

    let internal (|MeasureOne|_|) (typ: FSharpType) = 
        if typ.HasTypeDefinition && typ.TypeDefinition.LogicalName = "1" && typ.GenericArguments.Count = 0 then 
            Some ()
        else None

    let internal formatTypeArgument (typar: FSharpGenericParameter) =
        (if typar.IsSolveAtCompileTime then "^" else "'") + typar.Name

    let internal formatTypeArguments (typars:seq<FSharpGenericParameter>) =
        Seq.map formatTypeArgument typars |> List.ofSeq

    let internal bracket (str: string) = 
        if str.Contains(" ") then "(" + str + ")" else str

    let internal bracketIf cond str = 
        if cond then "(" + str + ")" else str

    let internal formatTyconRef (tcref:FSharpEntity) = 
        tcref.DisplayName

    let rec internal formatTypeApplication typeName prec prefix args =
        if prefix then 
            match args with
            | [] -> typeName
            | [arg] -> typeName + "<" + (formatTypeWithPrec 4 arg) + ">"
            | args -> bracketIf (prec <= 1) (typeName + "<" + (formatTypesWithPrec 2 "," args) + ">")
        else
            match args with
            | [] -> typeName
            | [arg] -> (formatTypeWithPrec 2 arg) + " " + typeName 
            | args -> bracketIf (prec <= 1) ((bracket (formatTypesWithPrec 2 "," args)) + typeName)

    and formatTypesWithPrec prec sep typs = 
        String.concat sep (typs |> Seq.map (formatTypeWithPrec prec))

    and formatTypeWithPrec prec (typ:FSharpType) =
        // Measure types are stored as named types with 'fake' constructors for products, "1" and inverses
        // of measures in a normalized form (see Andrew Kennedy technical reports). Here we detect this 
        // embedding and use an approximate set of rules for layout out normalized measures in a nice way. 
        match typ with 
        | MeasureProd (ty, MeasureOne) 
        | MeasureProd (MeasureOne, ty) -> formatTypeWithPrec prec ty
        | MeasureProd (ty1, MeasureInv ty2) 
        | MeasureProd (ty1, MeasureProd (MeasureInv ty2, MeasureOne)) -> 
            (formatTypeWithPrec 2 ty1) + "/" + (formatTypeWithPrec 2 ty2)
        | MeasureProd (ty1, MeasureProd(ty2,MeasureOne)) 
        | MeasureProd (ty1, ty2) -> 
            (formatTypeWithPrec 2 ty1) + "*" + (formatTypeWithPrec 2 ty2)
        | MeasureInv ty -> "/" + (formatTypeWithPrec 1 ty)
        | MeasureOne  -> "1" 
        | _ when typ.HasTypeDefinition -> 
            let tcref = typ.TypeDefinition 
            let tyargs = typ.GenericArguments |> Seq.toList
            // layout postfix array types
            formatTypeApplication (formatTyconRef tcref) prec tcref.UsesPrefixDisplay tyargs 
        | _ when typ.IsTupleType ->
            let tyargs = typ.GenericArguments |> Seq.toList
            bracketIf (prec <= 2) (formatTypesWithPrec 2 " * " tyargs)
        | _ when typ.IsFunctionType ->
            let rec loop soFar (typ:FSharpType) = 
                if typ.IsFunctionType then 
                    let _domainTyp, retType = typ.GenericArguments.[0], typ.GenericArguments.[1]
                    loop (soFar + (formatTypeWithPrec 4 typ.GenericArguments.[0]) + " -> ") retType
                else 
                    soFar + formatTypeWithPrec 5 typ
            bracketIf (prec <= 4) (loop "" typ)
        | _ when typ.IsGenericParameter ->
            formatTypeArgument typ.GenericParameter
        | _ -> failwith "Can't format type annotation" 

    let internal formatType typ = 
        formatTypeWithPrec 5 typ

    // Format each argument, including its name and type 
    let internal formatArgUsage hasTypeAnnotation i (arg: FSharpParameter) = 
        let nm = 
            match arg.Name with 
            | None -> 
                if arg.Type.HasTypeDefinition && arg.Type.TypeDefinition.XmlDocSig = "T:Microsoft.FSharp.Core.unit" then "()" 
                else "arg" + string i 
            | Some nm -> nm
        // Detect an optional argument 
        let isOptionalArg = hasAttrib<OptionalArgumentAttribute> arg.Attributes
        let argName = if isOptionalArg then "?" + nm else nm
        if hasTypeAnnotation && argName <> "()" then 
            argName + ": " + formatTypeWithPrec 2 arg.Type
        else argName

    let internal formatArgsUsage hasTypeAnnotation (v: FSharpMemberFunctionOrValue) args =
        let isItemIndexer = (v.IsInstanceMember && v.DisplayName = "Item")
        let counter = let n = ref 0 in fun () -> incr n; !n
        let unit, argSep, tupSep = "()", " ", ", "
        args
        |> List.map (List.map (fun x -> formatArgUsage hasTypeAnnotation (counter()) x))
        |> List.map (function 
            | [] -> unit 
            | [arg] when arg = unit -> unit
            | [arg] when not v.IsMember || isItemIndexer -> arg 
            | args when isItemIndexer -> String.concat tupSep args
            | args -> bracket (String.concat tupSep args))
        |> String.concat argSep
  
    let internal formatMember (ctx: Context) (v: FSharpMemberFunctionOrValue) = 
        let buildUsage (argInfos: FSharpParameter list list) = 
            let args =
                match argInfos with
                | [[x]] when v.IsGetterMethod && x.Name.IsNone 
                            && x.Type.TypeDefinition.XmlDocSig = "T:Microsoft.FSharp.Core.unit" -> ""
                | _  -> formatArgsUsage true v argInfos
            let parArgs = 
                if String.IsNullOrWhiteSpace(args) then "" 
                elif args.StartsWith("(") then args
                else sprintf "(%s)" args
            match v.IsMember, v.IsInstanceMember, v.LogicalName, v.DisplayName with
            // Constructors and indexers
            | _, _, ".ctor", _ -> "new" + parArgs 
            | _, true, _, "Item" -> "[" + parArgs + "]"
            // Ordinary instance members
            | _, true, _, name -> name + parArgs
            // Ordinary functions or values
            | false, _, _, name when 
                not (hasAttrib<RequireQualifiedAccessAttribute> v.LogicalEnclosingEntity.Attributes) -> 
                name + " " + parArgs
            // Ordinary static members or things (?) that require fully qualified access
            | _, _, _, name -> name + parArgs

        let modifiers =
            [ if v.InlineAnnotation = FSharpInlineAnnotation.AlwaysInline then yield "inline"
              // Skip dispatch slot because we generate stub implementation
              if v.IsDispatchSlot then () ]

        let argInfos = 
            // It might be a bug in FCS
            try v.CurriedParameterGroups |> Seq.map Seq.toList |> Seq.toList 
            with _ -> 
                Debug.WriteLine("FSharpMemberFunctionOrValue.CurriedParameterGroups throws an exception.")
                [[]]

        let retType =
            try Some v.ReturnParameter.Type 
            with _ -> 
                Debug.WriteLine("FSharpMemberFunctionOrValue.ReturnParameter throws an exception.")
                None

        let argInfos, retType = 
            match argInfos, v.IsGetterMethod, v.IsSetterMethod with
            | [ AllAndLast(args, last) ], _, true -> [ args ], Some last.Type
            | _, _, true -> argInfos, None
            | [[]], true, _ -> [], retType
            | _, _, _ -> argInfos, retType

        let _typars = formatTypeArguments v.GenericParameters 

        let retType = defaultArg (retType |> Option.map formatType) "unit"
        let usage = buildUsage argInfos
    
        ctx.Writer.WriteLine("")
        ctx.Writer.Write("member ")
        for modifier in modifiers do
            ctx.Writer.Write("{0} ", modifier)
        ctx.Writer.Write("{0}.", ctx.ObjectIdent)
        ctx.Writer.Write(usage)
        ctx.Writer.WriteLine(": {0} = ", retType)
        ctx.Writer.Indent ctx.Indentation
        for line in ctx.MethodBody do
            ctx.Writer.WriteLine(line)
        ctx.Writer.Unindent ctx.Indentation

    /// Get members in the decreasing order of inheritance chain
    let rec internal getMembers (e: FSharpEntity) = 
        seq {
            match e.BaseType with
            | Some baseType ->
                yield! getMembers baseType.TypeDefinition
            | None -> ()
            yield! e.MembersFunctionsAndValues
         }

    /// Generate stub implementation of an interface at a start column
    let formatInterface startColumn indentation objectIdent (methodBody: string) (e: FSharpEntity) =
        assert e.IsInterface
        use writer = new ColumnIndentedTextWriter()
        let lines = methodBody.Replace("\r\n", "\n").Split('\n')
        let ctx = { Writer = writer; Indentation = indentation; ObjectIdent = objectIdent; MethodBody = lines }
        writer.Indent startColumn
        for v in getMembers e do
            formatMember ctx v
        writer.Dump()

    let internal (|IndexerArg|) = function
        | SynIndexerArg.Two(e1, e2) -> [e1; e2]
        | SynIndexerArg.One e -> [e]

    let internal (|IndexerArgList|) xs =
        List.collect (|IndexerArg|) xs

    let internal inRange range pos = 
        AstTraversal.rangeContainsPosLeftEdgeInclusive range pos

    let tryFindInterfaceDeclaration (pos: pos) (parsedInput: ParsedInput) =
        let rec walkImplFileInput (ParsedImplFileInput(_name, _isScript, _fileName, _scopedPragmas, _hashDirectives, moduleOrNamespaceList, _)) = 
            List.tryPick walkSynModuleOrNamespace moduleOrNamespaceList

        and walkSynModuleOrNamespace(SynModuleOrNamespace(_lid, _isModule, decls, _xmldoc, _attributes, _access, _range)) =
            List.tryPick walkSynModuleDecl decls

        and walkSynModuleDecl(decl: SynModuleDecl) =
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
            | SynModuleDecl.DoExpr _
            | SynModuleDecl.Attributes _
            | SynModuleDecl.HashDirective _
            | SynModuleDecl.Open _ -> 
                None

        and walkSynTypeDefn(TypeDefn(_componentInfo, representation, members, _range)) = 
            walkSynTypeDefnRepr representation
            |> Option.orElse (List.tryPick walkSynMemberDefn members)        

        and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
                List.tryPick walkSynMemberDefn members
            | SynTypeDefnRepr.Simple(_repr, _range) -> 
                None

        and walkSynMemberDefn (memberDefn: SynMemberDefn) =
            match memberDefn with
            | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range) ->
                None
            | SynMemberDefn.AutoProperty(_attributes, _isStatic, id, _type, _memberKind, _memberFlags, _xmlDoc, _access, rhs, _r1, _r2) ->
                walkExpr rhs
            | SynMemberDefn.Interface(interfaceType, members, _range) ->
                if inRange interfaceType.Range pos then
                    Some(InterfaceData.Interface(interfaceType, members))
                else
                    members |> Option.bind (List.tryPick walkSynMemberDefn)
            | SynMemberDefn.Member(binding, _range) ->
                walkBinding binding
            | SynMemberDefn.NestedType(typeDef, _access, _range) -> 
                walkSynTypeDefn typeDef
            | SynMemberDefn.ValField(_field, _range) ->
                None
            | SynMemberDefn.LetBindings _
            | SynMemberDefn.Open _
            | SynMemberDefn.ImplicitInherit _
            | SynMemberDefn.Inherit _
            | SynMemberDefn.ImplicitCtor _ -> None

        and walkBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, _headPat, _retTy, expr, _bindingRange, _seqPoint)) =
            walkExpr expr

        and walkExpr e =
            if not <| inRange e.Range pos then 
                None
            else
                match e with
                | SynExpr.Quote(_synExpr1, _, _synExpr2, _, _range) ->
                    None
                | SynExpr.Const(_synConst, _range) -> 
                    None

                | SynExpr.Paren(synExpr, _, _, _parenRange) ->
                    walkExpr synExpr
                | SynExpr.Typed(synExpr, _synType, _range) -> 
                    walkExpr synExpr

                | SynExpr.Tuple(synExprList, _, _range)
                | SynExpr.ArrayOrList(_, synExprList, _range) ->
                    List.tryPick walkExpr synExprList

                | SynExpr.Record(_inheritOpt, _copyOpt, fields, _range) -> 
                    List.tryPick (fun (_, e, _) -> Option.bind walkExpr e) fields

                | SynExpr.New(_, _synType, synExpr, _range) -> 
                    walkExpr synExpr

                | SynExpr.ObjExpr(ty, baseCallOpt, binds, ifaces, _range1, _range2) -> 
                    match baseCallOpt, ifaces with
                    | None, [] -> 
                        if inRange ty.Range pos then
                            Some (InterfaceData.ObjExpr(ty, binds))
                        else
                            None
                    | _ -> 
                        // Ignore object expressions of normal objects
                        None

                | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
                    List.tryPick walkExpr [synExpr1; synExpr2]
                | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _isFromSource, _synPat, synExpr1, synExpr2, _range) -> 
                    List.tryPick walkExpr [synExpr1; synExpr2]

                | SynExpr.For(_sequencePointInfoForForLoop, _ident, synExpr1, _, synExpr2, synExpr3, _range) -> 
                    List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

                | SynExpr.ArrayOrListOfSeqExpr(_, synExpr, _range) ->
                    walkExpr synExpr
                | SynExpr.CompExpr(_, _, synExpr, _range) ->
                    walkExpr synExpr
                | SynExpr.Lambda(_, _, _synSimplePats, synExpr, _range) ->
                     walkExpr synExpr

                | SynExpr.MatchLambda(_isExnMatch,_argm,_synMatchClauseList,_spBind,_wholem) -> 
                    None

                | SynExpr.Match(_sequencePointInfoForBinding, synExpr, _synMatchClauseList, _, _range) ->
                    walkExpr synExpr
                | SynExpr.Lazy(synExpr, _range) ->
                    walkExpr synExpr
                | SynExpr.Do(synExpr, _range) ->
                    walkExpr synExpr
                | SynExpr.Assert(synExpr, _range) -> 
                    walkExpr synExpr

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
                | SynExpr.ImplicitZero(_range) -> None

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

        match parsedInput with
        | ParsedInput.SigFile _input ->
            None
        | ParsedInput.ImplFile input -> 
            walkImplFileInput input




