namespace FSharpVSPowerTools.CodeGeneration

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.CodeDom.Compiler
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

module SignatureGenerator =
    open Microsoft.FSharp.Compiler.PrettyNaming

    [<NoComparison>]
    type internal Context =
        {
            Writer: ColumnIndentedTextWriter
            Indentation: int
            DisplayContext: FSharpDisplayContext
        }

    let internal (|Module|_|) (entity: FSharpEntity) = 
        if entity.IsFSharpModule then Some() else None

    let getTypeNameWithGenericParams (typ: FSharpEntity) =
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

    let internal generateSignature ctx (mem: FSharpMemberFunctionOrValue) =
        let generateInputParamsPart (mem: FSharpMemberFunctionOrValue) =
            let takesInputParameters =
                not (mem.CurriedParameterGroups.Count = 1 && mem.CurriedParameterGroups.[0].Count = 0)

            let formatParamTypeName (typ: FSharpType) =
                if typ.IsFunctionType || typ.IsTupleType then
                    sprintf "(%s)" (typ.Format(ctx.DisplayContext))
                else
                    typ.Format(ctx.DisplayContext)

            if takesInputParameters then
                [
                    for pGroup in mem.CurriedParameterGroups do
                        yield [
                            for p in pGroup do
                                let formattedTypeName = formatParamTypeName p.Type

                                match p.Name with
                                | Some paramName -> yield sprintf "%s:%s" paramName formattedTypeName
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

    let rec internal writeModule ctx (modul: FSharpEntity) =
        Debug.Assert(modul.IsFSharpModule, "The entity should be a valid F# module.")
        writeDocs ctx modul.XmlDoc
        writeAttributes ctx modul.Attributes
        ctx.Writer.WriteLine("module {0} = ", modul.FullName)
        ctx.Writer.Indent ctx.Indentation
        for value in modul.MembersFunctionsAndValues do
            writeFunctionOrValue ctx value

        if modul.MembersFunctionsAndValues.Count > 0 && modul.NestedEntities.Count > 0 then
            ctx.Writer.WriteLine("")

        for entity in modul.NestedEntities do
            match entity with
            | Module _ -> writeModule ctx entity
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

        writeAttributes ctx typ.Attributes

        printfn "IsClass: %b" typ.IsClass
        printfn "IsOpaque: %b" typ.IsOpaque
        printfn "IsFSharp: %b" typ.IsFSharp
        printfn "IsFSharpModule: %b" typ.IsFSharpModule
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

        if classAttributeHasToBeAdded then
            ctx.Writer.WriteLine("[<Class>]")
        elif typ.IsInterface then
            ctx.Writer.WriteLine("[<Interface>]")
        elif typ.IsValueType then
             ctx.Writer.WriteLine("[<Struct>]")

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
                ctx.Writer.Write("| ")
                writeField true ctx field
        else ()

        // Interfaces
        [
            for inter in typ.DeclaredInterfaces do
                if inter.TypeDefinition <> typ then
                    yield inter, inter.Format(ctx.DisplayContext)
        ]
        |> List.sortBy (fun (inter, _name) ->
            // Sort by name without the namespace qualifier
            inter.TypeDefinition.DisplayName)
        |> List.iter (fun (_, name) -> ctx.Writer.WriteLine("interface {0}", name))

        // Members
        for value in typ.MembersFunctionsAndValues do
            Debug.Assert(not value.LogicalEnclosingEntity.IsFSharpModule, "F# type should not contain module functions or values.")
            writeMember ctx value
        // Nested entities
        // Deactivate nested types display for the moment (C# doesn't do it)
//        for entity in typ.NestedEntities do
//            Debug.Assert(not entity.IsFSharpModule, "F# type should not contain modules.")
//            // Nested types only happen due to C# interoperability
//            ctx.Writer.WriteLine("")
//            writeType ctx entity
        ctx.Writer.Unindent ctx.Indentation

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
            writeField false ctx field
        ctx.Writer.WriteLine("")

    and internal writeAttributes ctx (attributes: IList<FSharpAttribute>) =
        for attr in attributes do
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

    and internal writeFunctionOrValue ctx (value: FSharpMemberFunctionOrValue) =
        Debug.Assert(value.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a valid F# module.")
        writeDocs ctx value.XmlDoc
        ctx.Writer.WriteLine("val {0} : {1}", value.DisplayName, value.FullType.Format(ctx.DisplayContext))

    and internal writeMember ctx (mem: FSharpMemberFunctionOrValue) =
        Debug.Assert(not mem.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a type.")

        match mem with
        | Constructor _entity ->
            writeDocs ctx mem.XmlDoc
            ctx.Writer.WriteLine("new : {0}", generateSignature ctx mem)

        | _ when not mem.IsPropertyGetterMethod && not mem.IsPropertySetterMethod ->
            // Discard explicit getter/setter methods
            writeDocs ctx mem.XmlDoc

            let prefix =
                // Is static?
                if not mem.IsInstanceMember then "static "
                // Is abstract && enclosing type is interface/abstract?
                elif mem.IsDispatchSlot && mem.EnclosingEntity.IsInterface then "abstract "
                else ""

            ctx.Writer.WriteLine("{0}member {1} : {2}", prefix, mem.DisplayName, generateSignature ctx mem)
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
                | _ -> writeType ctx entity
            | :? FSharpMemberFunctionOrValue as mem ->
                if mem.LogicalEnclosingEntity.IsFSharpModule then
                    writeFunctionOrValue ctx mem
                else
                    writeSymbol mem.LogicalEnclosingEntity
            | _ ->
                fail "Invalid symbol in this context: %O" symbol

        writeSymbol symbol
        writer.Dump()