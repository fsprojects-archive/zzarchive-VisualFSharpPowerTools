module FSharpVSPowerTools.CodeGeneration.SignatureGenerator

open System
open System.Diagnostics
open System.Collections.Generic
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.Lexhelp.Keywords
open System.IO
open System.Text.RegularExpressions

[<NoComparison; NoEquality>]
type internal Context = 
    {
        Writer: ColumnIndentedTextWriter
        Indentation: int
        DisplayContext: FSharpDisplayContext
        OpenDeclarations: string list
        GetXmlDocBySignature: string -> string list
    }

let private hasUnitOnlyParameter (mem: FSharpMemberFunctionOrValue) =
    mem.CurriedParameterGroups.Count = 1 && mem.CurriedParameterGroups.[0].Count = 0

let private mustAppearAsAbstractMember (mem: FSharpMemberFunctionOrValue) =
    let enclosingEntityIsFSharpClass = mem.EnclosingEntity.IsClass && mem.EnclosingEntity.IsFSharp

    if mem.IsDispatchSlot then
        match mem.EnclosingEntity with
        | Interface | AbstractClass -> true
        | _ -> enclosingEntityIsFSharpClass 
    else
        false

let private needsInlineAnnotation (mem: FSharpMemberFunctionOrValue) =
    match mem.InlineAnnotation with
    | FSharpInlineAnnotation.AlwaysInline
    | FSharpInlineAnnotation.PseudoValue -> true
    | _ -> false

[<NoComparison>]
type private MembersPartition = 
    {
        Constructors: FSharpMemberFunctionOrValue[]
        AbstractMembers: FSharpMemberFunctionOrValue[]
        ConcreteInstanceMembers: FSharpMemberFunctionOrValue[]
        StaticMembers: FSharpMemberFunctionOrValue[]
    }
    static member Create(members: seq<FSharpMemberFunctionOrValue>) =
        // NOTE: If we want to handle EventHandler<'T> types, we can test
        //       match mem.ReturnParameter.Type with
        //       | TypeWithDefinition -> mem.ReturnParameter.Type.TypeWithDefinition.FullName = "System.EventHandler`1"
        //       | _ -> false
        let filteredMembers = members
                              |> Seq.filter (fun mem ->
                                  not mem.IsPropertyGetterMethod &&
                                  not mem.IsPropertySetterMethod &&
                                  not mem.IsEvent)

        let constructors = ResizeArray<_>()
        let abstractMembers = ResizeArray<_>()
        let concreteInstanceMembers = ResizeArray<_>()
        let staticMembers = ResizeArray<_>()

        for mem in filteredMembers do
            match mem with
            | Constructor _ -> constructors.Add(mem)
            | _ ->
                if not mem.IsInstanceMember then staticMembers.Add(mem)
                // Is abstract
                elif mustAppearAsAbstractMember mem then 
                    abstractMembers.Add(mem)
                else
                    concreteInstanceMembers.Add(mem)

        let sortByNameAndArgCount =
            Array.sortInPlaceBy (fun (mem: FSharpMemberFunctionOrValue) ->
                let paramCount =
                    if hasUnitOnlyParameter mem
                    then 0 
                    else mem.CurriedParameterGroups.Count

                mem.DisplayName.ToUpperInvariant(), paramCount)

        let res = 
            { Constructors = constructors.ToArray()
              AbstractMembers = abstractMembers.ToArray()
              ConcreteInstanceMembers = concreteInstanceMembers.ToArray()
              StaticMembers = staticMembers.ToArray() }

        sortByNameAndArgCount res.AbstractMembers
        sortByNameAndArgCount res.ConcreteInstanceMembers
        sortByNameAndArgCount res.StaticMembers

        res

let private isStaticallyResolved (param: FSharpGenericParameter) =
    param.Constraints
    |> Seq.exists (fun c -> c.IsMemberConstraint)

let private formatValueOrMemberName name =
    let newName = QuoteIdentifierIfNeeded name
    if newName = name then
        DemangleOperatorName name
    else
        newName

let private formatGenericParam (param: FSharpGenericParameter) =
    if isStaticallyResolved param then "^" + param.Name
    else "'" + param.Name

let private formatMemberConstraint ctx (c: FSharpGenericParameterMemberConstraint) =
    let hasPropertyShape =
        (c.MemberIsStatic && c.MemberArgumentTypes.Count = 0) ||
        (not c.MemberIsStatic && c.MemberArgumentTypes.Count = 1)
    let formattedMemberName, isProperty =
        match hasPropertyShape, TryChopPropertyName c.MemberName with
        | true, Some(chopped) when chopped <> c.MemberName ->
            formatValueOrMemberName chopped, true
        | _, _ -> formatValueOrMemberName c.MemberName, false
        
    [|
        yield "("
        yield
            sprintf "%smember %s"
                (if c.MemberIsStatic then "static " else "")
                formattedMemberName
        yield " : "
        
        if isProperty then
            yield (c.MemberReturnType.Format(ctx))
        else
            if c.MemberArgumentTypes.Count <= 1
            then yield "unit"
            else
                let startIdx = if c.MemberIsStatic then 0 else 1
                yield
                    [| for i in startIdx .. c.MemberArgumentTypes.Count - 1 ->
                        c.MemberArgumentTypes.[i].Format(ctx) |]
                    |> String.concat " * "
            yield sprintf " -> %s" (c.MemberReturnType.Format(ctx))
        yield ")"
    |]
    |> String.concat ""

let private getConstraints (ctx: FSharpDisplayContext) (genParams: IList<FSharpGenericParameter>) =
    let supportedConstraints =
        [|
            for param in genParams do
                let paramName = formatGenericParam param

                for c in param.Constraints do
                    if c.IsSupportsNullConstraint then
                        yield sprintf "%s : null" paramName
                    elif c.IsRequiresDefaultConstructorConstraint then
                        yield sprintf "%s : (new : unit -> %s)" paramName paramName
                    elif c.IsCoercesToConstraint then
                        yield sprintf "%s :> %s" paramName (c.CoercesToTarget.Format(ctx))
                    elif c.IsNonNullableValueTypeConstraint then
                        yield sprintf "%s : struct" paramName
                    elif c.IsReferenceTypeConstraint then
                        yield sprintf "%s : not struct" paramName
                    elif c.IsComparisonConstraint then
                        yield sprintf "%s : comparison" paramName
                    elif c.IsEqualityConstraint then
                        yield sprintf "%s : equality" paramName
                    elif c.IsUnmanagedConstraint then
                        yield sprintf "%s : unmanaged" paramName
                    elif c.IsEnumConstraint then
                        yield sprintf "%s : enum<%s>" paramName (c.EnumConstraintTarget.Format(ctx))
                    elif c.IsDelegateConstraint then
                        yield sprintf "%s : delegate<%s, %s>"
                                paramName
                                (c.DelegateConstraintData.DelegateTupledArgumentType.Format(ctx))
                                (c.DelegateConstraintData.DelegateReturnType.Format(ctx))
                    elif c.IsMemberConstraint then
                        yield sprintf "%s : %s" paramName (formatMemberConstraint ctx c.MemberConstraintData)
        |]

    if supportedConstraints.Length = 0 then ""
    else
        supportedConstraints
        |> String.concat " and "
        |> sprintf " when %s"

let private getTypeNameWithGenericParams ctx (typ: FSharpEntity) isInTypeName =
    [|
        yield QuoteIdentifierIfNeeded typ.DisplayName
        if typ.GenericParameters.Count > 0 then
            yield "<"
            let genericParamsRepr =
                [|
                    for i in 0 .. typ.GenericParameters.Count - 1 do
                        let p = typ.GenericParameters.[i]
                        if i = 0 && isStaticallyResolved p then
                            yield " " + formatGenericParam p
                        else
                            yield formatGenericParam p
                |]
                |> String.concat ", "
            yield genericParamsRepr
            if isInTypeName then
                yield getConstraints ctx.DisplayContext typ.GenericParameters
            yield ">"
    |]
    |> String.concat ""

let private generateSignature ctx (mem: FSharpMemberFunctionOrValue) =
    let generateInputParamsPart (mem: FSharpMemberFunctionOrValue) =
        let formatParamTypeName (param: FSharpParameter) =
            let formattedTypeName = param.Type.Format(ctx.DisplayContext)
            if param.IsOptionalArg then
                let result = formattedTypeName
                if mem.EnclosingEntity.IsFSharp && result.EndsWith(" option") then
                    // result has the 'XXXX option' format: we remove the trailing ' option'
                    result.Substring(0, result.Length - (" option").Length)
                else
                    result
            elif param.Type.IsFunctionType || param.Type.IsTupleType then
                sprintf "(%s)" formattedTypeName
            else
                formattedTypeName

        if not (hasUnitOnlyParameter mem) then
            [|
                for paramGroup in mem.CurriedParameterGroups do
                    yield [|
                        for (param: FSharpParameter) in paramGroup do
                            let formattedTypeName = formatParamTypeName param

                            match param.Name with
                            | Some paramName ->
                                let paramName = QuoteIdentifierIfNeeded paramName
                                if param.IsOptionalArg then
                                    yield sprintf "?%s:%s" paramName formattedTypeName
                                else
                                    yield sprintf "%s:%s" paramName formattedTypeName
                            | None -> yield formattedTypeName
                    |]
                    |> String.concat " * "
            |]
            |> String.concat " -> "
        else
            "unit"

    match mem with
    | Constructor entity ->
        let signatureInputParamsPart = generateInputParamsPart mem
        let signatureReturnTypePart = getTypeNameWithGenericParams ctx entity false

        sprintf "%s -> %s" signatureInputParamsPart signatureReturnTypePart

    | Event ->
        let returnParameterType = mem.ReturnParameter.Type
        let invokeMember = returnParameterType.TypeDefinition.MembersFunctionsAndValues
                           |> Seq.find (fun m -> m.DisplayName = "Invoke")
        sprintf "IEvent<%s, %s>" (returnParameterType.Format(ctx.DisplayContext)) (invokeMember.CurriedParameterGroups.[0].[1].Type.Format(ctx.DisplayContext))

    | _ when not mem.IsPropertyGetterMethod && not mem.IsPropertySetterMethod ->
        let signatureReturnTypePart = mem.ReturnParameter.Type.Format(ctx.DisplayContext)

        if mem.IsProperty then
            signatureReturnTypePart
        else
            let signatureInputParamsPart = generateInputParamsPart mem
            sprintf "%s -> %s" signatureInputParamsPart signatureReturnTypePart

    | _ -> ""

let private tryGetNeededTypeDefSyntaxDelimiter (typ: FSharpEntity) =
    let isStruct = typ.IsValueType && not typ.IsEnum
    let hasMembers = typ.MembersFunctionsAndValues.Count > 0

    if not hasMembers && not typ.IsFSharpModule then
        if typ.IsClass then Some "class"
        elif isStruct then Some "struct"
        elif typ.IsInterface then Some "interface"
        else None
    else None

let private tryRemoveModuleSuffix (modul: FSharpEntity) (moduleName: string) =
    let fullModuleNameWithoutModule =
        if modul.IsFSharpModule && hasModuleSuffixAttribute modul then
            if moduleName.EndsWith "Module" then
                moduleName.Substring(0, moduleName.Length - "Module".Length)
            else moduleName
        else moduleName

    let prefixToConsider =
        match modul.Namespace with
        | Some namespacePrefix -> namespacePrefix
        // TODO: it's not because there's no namespace that the module is not inside a module
        // Ex: module Microsoft.FSharp.Compiler.Range.Pos
        // -> Pos is in the Range module
        | None -> if modul.AccessPath = "global" then "" else modul.AccessPath

    let moduleNameOnly =
        let prefixLength = prefixToConsider.Length + 1
        if prefixLength >= fullModuleNameWithoutModule.Length || prefixToConsider = ""
        then fullModuleNameWithoutModule
        else fullModuleNameWithoutModule.Remove(0, prefixLength)

    if prefixToConsider = "" then
        QuoteIdentifierIfNeeded moduleNameOnly
    else
        prefixToConsider + "." + (QuoteIdentifierIfNeeded moduleNameOnly)

let rec internal writeModule isTopLevel ctx (modul: FSharpEntity) =
    Debug.Assert(modul.IsFSharpModule, "The entity should be a valid F# module.")
    writeDocs ctx modul.XmlDoc (fun _ -> modul.XmlDocSig)
    writeAttributes ctx (Some modul) modul.Attributes
    if isTopLevel then
        let qualifiedModuleName = tryRemoveModuleSuffix modul modul.FullName
        ctx.Writer.WriteLine("module {0}", qualifiedModuleName)
        ctx.OpenDeclarations
        |> Seq.filter ((<>) qualifiedModuleName)
        |> Seq.iteri (fun i decl ->
            if i = 0 then 
                ctx.Writer.WriteLine("")
            ctx.Writer.WriteLine("open {0}", decl))          
    else
        ctx.Writer.WriteLine("module {0} = ", QuoteIdentifierIfNeeded modul.LogicalName)

    if not isTopLevel then
        ctx.Writer.Indent ctx.Indentation
    if modul.MembersFunctionsAndValues.Count > 0 then
        ctx.Writer.WriteLine("")
    for value in modul.MembersFunctionsAndValues do
        writeFunctionOrValue ctx value

    if modul.NestedEntities.Count > 0 then
        ctx.Writer.WriteLine("")

    for entity in modul.NestedEntities do
        match entity with
        | FSharpModule -> writeModule false ctx entity
        | AbbreviatedType abbreviatedType -> writeTypeAbbrev true ctx entity abbreviatedType
        | FSharpException -> writeFSharpExceptionType true ctx entity
        | Delegate when entity.IsFSharp -> writeDelegateType true ctx entity
        | _ -> writeType true ctx entity

        ctx.Writer.WriteLine("")
    if not isTopLevel then
        ctx.Writer.Unindent ctx.Indentation

and internal getParentPath (entity: FSharpEntity) =
    match entity.Namespace with
    | Some ns -> sprintf "namespace %s" ns
    | None -> sprintf "module %s" entity.AccessPath

and internal writeType isNestedEntity ctx (typ: FSharpEntity) =
    Debug.Assert(not typ.IsFSharpModule, "The entity should be a type.")

    if not isNestedEntity then
        let parent = getParentPath typ
        ctx.Writer.WriteLine(parent)
        ctx.OpenDeclarations
        |> Seq.filter (not << parent.EndsWith)
        |> Seq.iteri (fun i decl ->
            if i = 0 then 
                ctx.Writer.WriteLine("")
            ctx.Writer.WriteLine("open {0}", decl))
        ctx.Writer.WriteLine("")

    writeDocs ctx typ.XmlDoc (fun _ -> typ.XmlDocSig)
    writeAttributes ctx (Some typ) typ.Attributes

    let neededTypeDefSyntaxDelimiter = tryGetNeededTypeDefSyntaxDelimiter typ
    let classAttributeHasToBeAdded =
        let hasVisibleConstructor =
            typ.MembersFunctionsAndValues
            |> Seq.exists (function
                | Constructor _ -> true
                | _             -> false)

        (not typ.IsInterface)
        && (not typ.IsValueType)
        && (typ.IsOpaque || not hasVisibleConstructor)
        && not (typ.IsFSharpRecord || typ.IsFSharpUnion)
        && not (hasAttribute<AbstractClassAttribute> typ.Attributes)
        && not (hasAttribute<ClassAttribute> typ.Attributes)
        && neededTypeDefSyntaxDelimiter <> Some "class"

    if classAttributeHasToBeAdded then
        ctx.Writer.WriteLine("[<Class>]")
    elif typ.IsInterface && neededTypeDefSyntaxDelimiter = None then
        ctx.Writer.WriteLine("[<Interface>]")

    ctx.Writer.WriteLine("type {0} =", getTypeNameWithGenericParams ctx typ true)
    ctx.Writer.Indent ctx.Indentation
    if typ.IsFSharpRecord then
        ctx.Writer.WriteLine("{")
        ctx.Writer.Indent ctx.Indentation
        for field in typ.FSharpFields do
            writeField true ctx field
        ctx.Writer.Unindent ctx.Indentation
        ctx.Writer.WriteLine("}")
    elif typ.IsFSharpUnion then
        for case in typ.UnionCases do
            writeUnionCase ctx case
    elif typ.IsEnum then
        for field in typ.FSharpFields do
            writeEnumValue ctx field

    let isStruct = typ.IsValueType && not typ.IsEnum
    let hasMembers = typ.MembersFunctionsAndValues.Count > 0

    if not hasMembers then
        match neededTypeDefSyntaxDelimiter with
        | Some startDelimiter ->
            ctx.Writer.WriteLine(startDelimiter)
            ctx.Writer.Indent ctx.Indentation
        | None -> ()

    // Inherited class
    try
        match typ.BaseType with
        | Some(TypeWithDefinition(baseTypDef) as baseTyp) when baseTypDef.DisplayName <> "obj" ->
            if not (typ.IsValueType || typ.IsEnum || typ.IsDelegate || typ.IsArrayType) then
                ctx.Writer.WriteLine("inherit {0}", baseTyp.Format(ctx.DisplayContext))
        | _ -> ()
    with _ -> ()

    // Interfaces
    [
        for inter in typ.DeclaredInterfaces do
            if inter.TypeDefinition <> typ then
                yield inter, inter.Format(ctx.DisplayContext)
    ]
    |> List.sortBy (fun (inter, _name) ->
        // Sort by name without the namespace qualifier
        if inter.HasTypeDefinition
        then inter.TypeDefinition.DisplayName.ToUpperInvariant()
        else String.Empty)
    |> List.iter (fun (_, name) ->
        if typ.IsInterface then
            ctx.Writer.WriteLine("inherit {0}", name)
        else
            ctx.Writer.WriteLine("interface {0}", name))

    let membersPartition = MembersPartition.Create(typ.MembersFunctionsAndValues)

    // Constructors
    for constr in membersPartition.Constructors do
        writeMember ctx constr

    // Fields
    if typ.IsClass || isStruct then
        for field in typ.FSharpFields do
            if field.Accessibility.IsPublic || field.Accessibility.IsInternal then
                writeClassOrStructField ctx field

    // Abstract members
    for mem in membersPartition.AbstractMembers do
        writeMember ctx mem

    // Concrete instance members
    for mem in membersPartition.ConcreteInstanceMembers do
        writeMember ctx mem

    // Static members
    for mem in membersPartition.StaticMembers do
        writeMember ctx mem

    // Nested entities
    // Deactivate nested types display for the moment (C# doesn't do it)
//        for entity in typ.NestedEntities do
//            Debug.Assert(not entity.IsFSharpModule, "F# type should not contain modules.")
//            // Nested types only happen due to C# interoperability
//            ctx.Writer.WriteLine("")
//            writeType ctx entity

    if not hasMembers && neededTypeDefSyntaxDelimiter <> None then
        ctx.Writer.Unindent ctx.Indentation
        ctx.Writer.WriteLine("end")

    ctx.Writer.Unindent ctx.Indentation

and internal writeTypeAbbrev isNestedEntity ctx (abbreviatingType: FSharpEntity) (abbreviatedType: FSharpType) =
    if not isNestedEntity then
        let parent = getParentPath abbreviatingType
        ctx.Writer.WriteLine(parent)
        ctx.OpenDeclarations
        |> Seq.filter (not << parent.EndsWith)
        |> Seq.iteri (fun i decl ->
            if i = 0 then 
                ctx.Writer.WriteLine("")
            ctx.Writer.WriteLine("open {0}", decl))
        ctx.Writer.WriteLine("")

    writeDocs ctx abbreviatingType.XmlDoc (fun _ -> abbreviatingType.XmlDocSig)
    ctx.Writer.WriteLine("type {0} = {1}",
        getTypeNameWithGenericParams ctx abbreviatingType true,
        abbreviatedType.Format(ctx.DisplayContext))

and internal writeFSharpExceptionType isNestedEntity ctx (exn: FSharpEntity) =
    if not isNestedEntity then
        let parent = getParentPath exn
        ctx.Writer.WriteLine(parent)
        ctx.OpenDeclarations
        |> Seq.filter (not << parent.EndsWith)
        |> Seq.iteri (fun i decl ->
            if i = 0 then 
                ctx.Writer.WriteLine("")
            ctx.Writer.WriteLine("open {0}", decl))
        ctx.Writer.WriteLine("")

    writeDocs ctx exn.XmlDoc (fun _ -> exn.XmlDocSig)

    if exn.FSharpFields.Count > 0 then
        let fields =
            [|
                for field in exn.FSharpFields ->
                    if field.FieldType.IsFunctionType || field.FieldType.IsTupleType then
                        sprintf "(%s)" (field.FieldType.Format(ctx.DisplayContext))
                    else
                        field.FieldType.Format(ctx.DisplayContext)
            |]
            |> String.concat " * "

        ctx.Writer.WriteLine("exception {0} of {1}", QuoteIdentifierIfNeeded exn.LogicalName, fields)
    else
        ctx.Writer.WriteLine("exception {0}", QuoteIdentifierIfNeeded exn.LogicalName)

and internal writeDelegateType isNestedEntity ctx (del: FSharpEntity) =
    if not isNestedEntity then
        let parent = getParentPath del
        ctx.Writer.WriteLine(parent)        
        ctx.OpenDeclarations
        |> Seq.filter (not << parent.EndsWith)
        |> Seq.iteri (fun i decl ->
            if i = 0 then 
                ctx.Writer.WriteLine("")
            ctx.Writer.WriteLine("open {0}", decl))
        ctx.Writer.WriteLine("")
    writeDocs ctx del.XmlDoc (fun _ -> del.XmlDocSig)

    let argsPart =
        [|
            for arg in del.FSharpDelegateSignature.DelegateArguments do
                match arg with
                | Some name, typ ->
                    if typ.IsFunctionType || typ.IsTupleType then
                        yield sprintf "%s:(%s)" name (typ.Format(ctx.DisplayContext))
                    else
                        yield sprintf "%s:%s" name (typ.Format(ctx.DisplayContext))
                | None, typ ->
                    if typ.IsFunctionType || typ.IsTupleType then
                        yield sprintf "(%s)" (typ.Format(ctx.DisplayContext))
                    else
                        yield sprintf "%s" (typ.Format(ctx.DisplayContext))
        |]
        |> String.concat " * "

    ctx.Writer.WriteLine("type {0} =", QuoteIdentifierIfNeeded del.LogicalName)
    ctx.Writer.Indent ctx.Indentation
    ctx.Writer.WriteLine("delegate of {0} -> {1}", argsPart, del.FSharpDelegateSignature.DelegateReturnType.Format(ctx.DisplayContext))
    ctx.Writer.Unindent ctx.Indentation

and internal writeUnionCase ctx (case: FSharpUnionCase) =
    writeDocs ctx case.XmlDoc (fun _ -> case.XmlDocSig)

    if case.Attributes.Count > 0 then
        ctx.Writer.Write("| ")
        ctx.Writer.Indent(2)
        writeAttributes ctx None case.Attributes
        ctx.Writer.Write(formatValueOrMemberName case.Name)
        ctx.Writer.Unindent(2)
    else
        ctx.Writer.Write("| {0}", formatValueOrMemberName case.Name)

    if case.UnionCaseFields.Count > 0 then
        case.UnionCaseFields
        |> Seq.iteri (fun i field ->
            if i = 0 then 
                ctx.Writer.Write(" of ")
            else 
                ctx.Writer.Write(" * ")
            writeUnionCaseField ctx field)

    ctx.Writer.WriteLine("")

and internal writeAttributes ctx (typ: option<FSharpEntity>) (attributes: IList<FSharpAttribute>) =
    let typeDefSyntaxDelimOpt = Option.bind tryGetNeededTypeDefSyntaxDelimiter typ
    let bypassAttribute (attrib: FSharpAttribute) =
        typeDefSyntaxDelimOpt = Some "struct" && isAttribute<StructAttribute> attrib

    for (attr: FSharpAttribute) in attributes do
        if not (bypassAttribute attr) then
            let name = 
                let displayName = attr.AttributeType.DisplayName

                // We only remove the "Attribute" suffix when the identifier need not to be quoted
                if displayName.EndsWith "Attribute" && displayName.Length > "Attribute".Length then
                    displayName.Substring(0, displayName.Length - "Attribute".Length)
                else QuoteIdentifierIfNeeded attr.AttributeType.LogicalName
            if attr.ConstructorArguments.Count = 0 then
                ctx.Writer.WriteLine("[<{0}>]", name)
            else
                let argumentsStringRepr = [| for arg in attr.ConstructorArguments -> sprintf "%A" arg |]
                                          |> String.concat ", "

                ctx.Writer.Write("[<{0}(", name)
                ctx.Writer.Write(argumentsStringRepr)
                ctx.Writer.WriteLine(")>]")

and internal writeField hasNewLine ctx (field: FSharpField) =
    writeDocs ctx field.XmlDoc (fun _ -> field.XmlDocSig)
    writeAttributes ctx None field.FieldAttributes
    writeAttributes ctx None field.PropertyAttributes

    let fieldName = QuoteIdentifierIfNeeded field.Name

    if hasNewLine then
        ctx.Writer.WriteLine("{0}: {1}", fieldName, field.FieldType.Format(ctx.DisplayContext))
    else
        ctx.Writer.Write("{0}: {1}", fieldName, field.FieldType.Format(ctx.DisplayContext))

and internal writeEnumValue ctx (field: FSharpField) =
    // NOTE: for enum values, the compiler generates a "value__" field which 
    //       is not visible from outside. That's why we don't display it
    if not field.IsCompilerGenerated then
        match field.LiteralValue with
        | Some value ->
            writeDocs ctx field.XmlDoc (fun _ -> field.XmlDocSig)
            ctx.Writer.WriteLine("| {0} = {1}", field.Name, value)
        | None -> Debug.Fail("There should be a literal value for the enum field: " + field.FullName)

and internal writeUnionCaseField ctx (field: FSharpField) =
    if isUnnamedUnionCaseField field then
        ctx.Writer.Write(field.FieldType.Format(ctx.DisplayContext))
    else
        ctx.Writer.Write("{0}: {1}", QuoteIdentifierIfNeeded field.Name, field.FieldType.Format(ctx.DisplayContext))

and internal writeFunctionOrValue ctx (value: FSharpMemberFunctionOrValue) =
    Debug.Assert(value.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a valid F# module.")
    writeDocs ctx value.XmlDoc (fun _ -> value.XmlDocSig)

    let constraints = getConstraints ctx.DisplayContext value.GenericParameters
    let valueName = formatValueOrMemberName value.LogicalName

    if value.FullType.IsFunctionType then
        let inlineSpecifier = if needsInlineAnnotation value then "inline " else ""
        ctx.Writer.WriteLine("val {0}{1} : {2}{3}", inlineSpecifier, valueName, generateSignature ctx value, constraints)
    else
        ctx.Writer.WriteLine("val {0} : {1}{2}", valueName, value.FullType.Format(ctx.DisplayContext), constraints)

and internal writeClassOrStructField ctx (field: FSharpField) =
    Debug.Assert(field.DeclaringEntity.IsClass ||
                 (field.DeclaringEntity.IsValueType && not field.DeclaringEntity.IsEnum),
                 "The declaring entity should be a class or a struct.")

    writeDocs ctx field.XmlDoc (fun _ -> field.XmlDocSig)
    ctx.Writer.WriteLine("val {0} : {1}", QuoteIdentifierIfNeeded field.DisplayName, field.FieldType.Format(ctx.DisplayContext))

and internal writeMember ctx (mem: FSharpMemberFunctionOrValue) =
    Debug.Assert(not mem.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a type.")

    match mem with
    | Constructor _entity ->
        writeDocs ctx mem.XmlDoc (fun _ -> mem.XmlDocSig)
        ctx.Writer.WriteLine("new : {0}", generateSignature ctx mem)
    | Event -> ()
    | _ when not mem.IsPropertyGetterMethod && not mem.IsPropertySetterMethod ->
        // Discard explicit getter/setter methods
        writeDocs ctx mem.XmlDoc (fun _ -> mem.XmlDocSig)
        writeAttributes ctx None mem.Attributes

        let memberType =
            // Is static?
            if not mem.IsInstanceMember then
                "static member"
            // Is abstract && enclosing type is interface/abstract?
            elif mustAppearAsAbstractMember mem then
                "abstract member"
            elif mem.IsOverrideOrExplicitMember then
                "override"
            else
                "member"

        let propertyType =
            if mem.HasSetterMethod && mem.HasGetterMethod then " with get, set"
            elif mem.HasSetterMethod then " with set"
            else ""

        let inlineAnnotation =
            if needsInlineAnnotation mem then "inline " else ""

        let constraints = getConstraints ctx.DisplayContext mem.GenericParameters

        ctx.Writer.WriteLine("{0} {1}{2} : {3}{4}{5}",
                             memberType,
                             inlineAnnotation,
                             formatValueOrMemberName mem.LogicalName,
                             generateSignature ctx mem,
                             propertyType,
                             constraints)
    | _ -> ()

and internal writeDocs ctx docs getXmlDocSig =
    let xmlDocs =
        if Seq.isEmpty docs then            
            getXmlDocSig()
            |> ctx.GetXmlDocBySignature
            :> seq<_>
        else docs :> seq<_>

    xmlDocs
    |> Seq.collect (fun line -> line.Replace("\r\n", "\n").Split('\r', '\n'))
    |> Seq.iter (fun line -> ctx.Writer.WriteLine("/// {0}", line.TrimStart()))

and internal writeActivePattern ctx (case: FSharpActivePatternCase) =
    let group = case.Group
    writeDocs ctx case.XmlDoc (fun _ -> case.XmlDocSig)
    ctx.Writer.Write("val |")
    for name in group.Names do
        ctx.Writer.Write("{0}|", name)
    if not group.IsTotal then
        ctx.Writer.Write("_|")
    ctx.Writer.Write(" : ")
    ctx.Writer.WriteLine("{0}", group.OverallType.Format(ctx.DisplayContext))

let formatSymbol getXmlDocBySignature indentation displayContext openDeclarations (symbol: FSharpSymbol) =
    use writer = new ColumnIndentedTextWriter()
    let ctx = { Writer = writer; Indentation = indentation;
                DisplayContext = displayContext; OpenDeclarations = openDeclarations;
                GetXmlDocBySignature = getXmlDocBySignature }

    let rec writeSymbol (symbol: FSharpSymbol) =
        match symbol with
        | Entity(entity, _, _) ->
            match entity with
            | FSharpModule -> writeModule true ctx entity
            | AbbreviatedType abbreviatedType -> writeTypeAbbrev false ctx entity abbreviatedType
            | FSharpException -> writeFSharpExceptionType false ctx entity
            | Delegate when entity.IsFSharp -> writeDelegateType false ctx entity
            | _ -> writeType false ctx entity
            |> Some
        | MemberFunctionOrValue mem ->
            writeSymbol mem.LogicalEnclosingEntity
        | ActivePatternCase case ->
            Some (writeActivePattern ctx case)
        | UnionCase uc ->
            match uc.ReturnType with
            | TypeWithDefinition entity ->
                writeSymbol entity
            | _ -> None
        | Field(field, _) ->
            writeSymbol field.DeclaringEntity
        | _ ->
            debug "Invalid symbol in this context: %O" (symbol.GetType().Name)
            None
    writeSymbol symbol
    |> Option.map (fun _ -> writer.Dump())

let [<Literal>] private tempFileName = "tmp"

/// Get file name from symbol's full name and escape illegal characters
let getFileNameFromSymbol (symbol: FSharpSymbol) =    
    let fileName =        
        match symbol with
        | MemberFunctionOrValue mem ->
            mem.LogicalEnclosingEntity.TryGetFullName()
        | UnionCase uc ->
            match uc.ReturnType with
            | TypeWithDefinition entity ->
                entity.TryGetFullName()
            | _ -> None
        | Field (field, _) -> field.DeclaringEntity.TryGetFullName()
        | _ -> Option.attempt(fun _ -> symbol.FullName)
        |> Option.getOrElse tempFileName

    let regexSearch = String(Path.GetInvalidFileNameChars()) + String(Path.GetInvalidPathChars())
    let r = Regex(String.Format("[{0}]", Regex.Escape(regexSearch)))
    r.Replace(fileName + ".fsi", "")
