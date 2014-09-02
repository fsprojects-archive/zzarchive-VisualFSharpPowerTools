module FSharpVSPowerTools.CodeGeneration.SignatureGenerator

open System.Diagnostics
open System.Collections.Generic
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.PrettyNaming

[<NoComparison>]
type internal Context = {
    Writer: ColumnIndentedTextWriter
    Indentation: int
    DisplayContext: FSharpDisplayContext
}

let hasUnitOnlyParameter (mem: FSharpMemberFunctionOrValue) =
    mem.CurriedParameterGroups.Count = 1 && mem.CurriedParameterGroups.[0].Count = 0

[<NoComparison>]
type private MembersPartition = {
    Constructors: FSharpMemberFunctionOrValue[]
    AbstractMembers: FSharpMemberFunctionOrValue[]
    ConcreteInstanceMembers: FSharpMemberFunctionOrValue[]
    StaticMembers: FSharpMemberFunctionOrValue[]
}
with
    static member Create(members: seq<FSharpMemberFunctionOrValue>) =
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
                if not mem.IsInstanceMember then
                    staticMembers.Add(mem)
                // Is abstract && enclosing type is interface/abstract?
                elif mem.IsDispatchSlot && isInterfaceOrAbstractClass mem.EnclosingEntity then
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

let internal (|Module|_|) (entity: FSharpEntity) = 
    if entity.IsFSharpModule then Some() else None

let private getTypeNameWithGenericParams (typ: FSharpEntity) =
    [|
        yield typ.DisplayName
        if typ.GenericParameters.Count > 0 then
            yield "<"
            let genericParamsRepr =
                [| for p in typ.GenericParameters -> "'" + p.DisplayName |]
                |> String.concat ", "
            yield genericParamsRepr
            yield ">"
    |]
    |> String.concat ""

let private generateSignature ctx (mem: FSharpMemberFunctionOrValue) =
    let generateInputParamsPart (mem: FSharpMemberFunctionOrValue) =
        let formatParamTypeName (typ: FSharpType) isOptional =
            if isOptional then
                // result has the 'XXXX option' format: we remove the trailing ' option'
                let result = typ.Format(ctx.DisplayContext)
                result.Substring(0, result.Length - (" option").Length)
            elif typ.IsFunctionType || typ.IsTupleType then
                sprintf "(%s)" (typ.Format(ctx.DisplayContext))
            else
                typ.Format(ctx.DisplayContext)

        if not (hasUnitOnlyParameter mem) then
            [
                for paramGroup in mem.CurriedParameterGroups do
                    yield [
                        for (param: FSharpParameter) in paramGroup do
                            let isOptional = hasAttribute<OptionalArgumentAttribute> param.Attributes
                            let formattedTypeName = formatParamTypeName param.Type isOptional

                            match param.Name with
                            | Some paramName when not isOptional ->
                                yield sprintf "%s:%s" paramName formattedTypeName
                            | Some paramName ->
                                yield sprintf "?%s:%s" paramName formattedTypeName
                            | None -> yield formattedTypeName
                    ]
                    |> String.concat " * "
            ]
            |> String.concat " -> "
        else
            "unit"

    match mem with
    | Constructor entity ->
        let signatureInputParamsPart = generateInputParamsPart mem
        let signatureReturnTypePart = getTypeNameWithGenericParams entity

        sprintf "%s -> %s" signatureInputParamsPart signatureReturnTypePart

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

let rec internal writeModule ctx (modul: FSharpEntity) =
    Debug.Assert(modul.IsFSharpModule, "The entity should be a valid F# module.")
    writeDocs ctx modul.XmlDoc
    writeAttributes ctx modul modul.Attributes
    ctx.Writer.WriteLine("module {0} = ", modul.FullName)
    ctx.Writer.Indent ctx.Indentation
    for value in modul.MembersFunctionsAndValues do
        writeFunctionOrValue ctx value

    if modul.MembersFunctionsAndValues.Count > 0 && modul.NestedEntities.Count > 0 then
        ctx.Writer.WriteLine("")

    for entity in modul.NestedEntities do
        match entity with
        | Module _ -> writeModule ctx entity
        | AbbreviatedType abbreviatedType -> writeTypeAbbrev ctx entity abbreviatedType
        | _ when entity.IsFSharpExceptionDeclaration -> writeFSharpExceptionType ctx entity
        | _ -> writeType ctx entity

        ctx.Writer.WriteLine("")

    ctx.Writer.Unindent ctx.Indentation

and internal writeType ctx (typ: FSharpEntity) =
    Debug.Assert(not typ.IsFSharpModule, "The entity should be a type.")
    match typ.Namespace with
    | Some ns -> 
        ctx.Writer.WriteLine("namespace {0}", ns)
        ctx.Writer.WriteLine("")
    | None ->
        // TODO: print modules or not?
        ()

    writeDocs ctx typ.XmlDoc
    writeAttributes ctx typ typ.Attributes

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

    ctx.Writer.WriteLine("type {0} =", getTypeNameWithGenericParams typ)
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

    // Interfaces
    [
        for inter in typ.DeclaredInterfaces do
            if inter.TypeDefinition <> typ then
                yield inter, inter.Format(ctx.DisplayContext)
    ]
    |> List.sortBy (fun (inter, _name) ->
        // Sort by name without the namespace qualifier
        inter.TypeDefinition.DisplayName)
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

and internal writeTypeAbbrev ctx (abbreviatingType: FSharpEntity) (abbreviatedType: FSharpType) =
    writeDocs ctx abbreviatingType.XmlDoc
    ctx.Writer.WriteLine("type {0} = {1}", abbreviatingType.DisplayName, abbreviatedType.Format(ctx.DisplayContext))

and internal writeFSharpExceptionType ctx (exn: FSharpEntity) =
    writeDocs ctx exn.XmlDoc

    if exn.FSharpFields.Count > 0 then
        let fields =
            [
                for field in exn.FSharpFields ->
                    if field.FieldType.IsFunctionType || field.FieldType.IsTupleType then
                        sprintf "(%s)" (field.FieldType.Format(ctx.DisplayContext))
                    else
                        field.FieldType.Format(ctx.DisplayContext)
            ]
            |> String.concat " * "

        ctx.Writer.WriteLine("exception {0} of {1}", exn.DisplayName, fields)
    else
        ctx.Writer.WriteLine("exception {0}", exn.DisplayName)

and internal writeUnionCase ctx (case: FSharpUnionCase) =
    writeDocs ctx case.XmlDoc
    ctx.Writer.Write("| {0}", DemangleOperatorName case.Name)
    let mutable isFirst = true
    for field in case.UnionCaseFields do
        if isFirst then
            ctx.Writer.Write(" of ")
            isFirst <- false
        else
            ctx.Writer.Write(" * ")

        writeUnionCaseField ctx field
    ctx.Writer.WriteLine("")

and internal writeAttributes ctx (typ: FSharpEntity) (attributes: IList<FSharpAttribute>) =
    let typeDefSyntaxDelimOpt = tryGetNeededTypeDefSyntaxDelimiter typ
    let bypassAttribute (attrib: FSharpAttribute) =
        typeDefSyntaxDelimOpt = Some "struct" &&
        attrib.AttributeType.CompiledName = typeof<StructAttribute>.Name

    for attr in attributes do
        if not (bypassAttribute attr) then
            let name = 
                let displayName = attr.AttributeType.DisplayName
                if displayName.EndsWith "Attribute" && displayName.Length > "Attribute".Length then
                    displayName.Substring(0, displayName.Length - "Attribute".Length)
                else displayName
            if attr.ConstructorArguments.Count = 0 then
                ctx.Writer.WriteLine("[<{0}>]", name)
            else
                ctx.Writer.Write("[<{0}(", name)
                let mutable isFirst = true
                for arg in attr.ConstructorArguments do
                    if isFirst then
                        ctx.Writer.Write("{0}", sprintf "%A" arg)
                        isFirst <- false
                    else
                        ctx.Writer.Write(", {0}", sprintf "%A" arg)
                ctx.Writer.WriteLine(")>]")

and internal writeField hasNewLine ctx (field: FSharpField) =
    writeDocs ctx field.XmlDoc

    if hasNewLine then
        ctx.Writer.WriteLine("{0}: {1}", field.Name, field.FieldType.Format(ctx.DisplayContext))
    else
        ctx.Writer.Write("{0}: {1}", field.Name, field.FieldType.Format(ctx.DisplayContext))

and internal writeEnumValue ctx (field: FSharpField) =
    // NOTE: for enum values, the compiler generates a "value__" field which 
    //       is not visible from outside. That's why we don't display it
    if not field.IsCompilerGenerated then
        match field.LiteralValue with
        | Some value ->
            writeDocs ctx field.XmlDoc
            ctx.Writer.WriteLine("| {0} = {1}", field.Name, value)
        | None -> Debug.Fail("There should be a literal value for the enum field: " + field.FullName)

and internal writeUnionCaseField ctx (field: FSharpField) =
    if isUnnamedUnionCaseField field then
        ctx.Writer.Write(field.FieldType.Format(ctx.DisplayContext))
    else
        ctx.Writer.Write("{0}: {1}", field.Name, field.FieldType.Format(ctx.DisplayContext))

and internal writeFunctionOrValue ctx (value: FSharpMemberFunctionOrValue) =
    Debug.Assert(value.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a valid F# module.")
    writeDocs ctx value.XmlDoc
    if value.FullType.IsFunctionType then
        ctx.Writer.WriteLine("val {0} : {1}", value.DisplayName, generateSignature ctx value)
    else
        ctx.Writer.WriteLine("val {0} : {1}", value.DisplayName, value.FullType.Format(ctx.DisplayContext))

and internal writeClassOrStructField ctx (field: FSharpField) =
    Debug.Assert(field.DeclaringEntity.IsClass ||
                 (field.DeclaringEntity.IsValueType && not field.DeclaringEntity.IsEnum),
                 "The declaring entity should be a class or a struct.")

    writeDocs ctx field.XmlDoc
    ctx.Writer.WriteLine("val {0} : {1}", field.DisplayName, field.FieldType.Format(ctx.DisplayContext))

and internal writeMember ctx (mem: FSharpMemberFunctionOrValue) =
    Debug.Assert(not mem.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a type.")

    match mem with
    | Constructor _entity ->
        writeDocs ctx mem.XmlDoc
        ctx.Writer.WriteLine("new : {0}", generateSignature ctx mem)

    | _ when not mem.IsPropertyGetterMethod && not mem.IsPropertySetterMethod ->
        // Discard explicit getter/setter methods
        writeDocs ctx mem.XmlDoc

        let memberType =
            // Is static?
            if not mem.IsInstanceMember then
                "static member"
            // Is abstract && enclosing type is interface/abstract?
            elif mem.IsDispatchSlot && isInterfaceOrAbstractClass mem.EnclosingEntity then
                "abstract member"
            elif mem.IsOverrideOrExplicitMember then
                "override"
            else
                "member"

        ctx.Writer.WriteLine("{0} {1} : {2}", memberType, DemangleOperatorName mem.DisplayName, generateSignature ctx mem)
    | _ -> ()

and internal writeDocs ctx docs =
    for doc in docs do
        ctx.Writer.WriteLine("/// {0}", doc)

let formatSymbol displayContext (symbol: FSharpSymbol) =
    use writer = new ColumnIndentedTextWriter()
    let ctx = { Writer = writer; Indentation = 4; DisplayContext = displayContext }

    let rec writeSymbol (symbol: FSharpSymbol) =
        match symbol with
        | :? FSharpEntity as entity ->
            match entity with
            | Module _ -> writeModule ctx entity
            | AbbreviatedType abbreviatedType -> writeTypeAbbrev ctx entity abbreviatedType
            | _ when entity.IsFSharpExceptionDeclaration -> writeFSharpExceptionType ctx entity
            | _ -> writeType ctx entity
        | :? FSharpMemberFunctionOrValue as mem ->
            writeSymbol mem.LogicalEnclosingEntity
        | _ ->
            fail "Invalid symbol in this context: %O" symbol

    writeSymbol symbol
    writer.Dump()