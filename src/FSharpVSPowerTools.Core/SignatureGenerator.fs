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
    [<NoComparison>]
    type internal Context =
        {
            Writer: ColumnIndentedTextWriter
            Indentation: int
            DisplayContext: FSharpDisplayContext
        }

    let internal (|Module|_|) (entity: FSharpEntity) = 
        if entity.IsFSharpModule then Some() else None

    let rec internal writeModule ctx (modul: FSharpEntity) =
        Debug.Assert(modul.IsFSharpModule, "The entity should be a valid F# module.")
        writeDocs ctx modul.XmlDoc
        ctx.Writer.WriteLine("module {0} = ", modul.FullName)
        ctx.Writer.Indent ctx.Indentation
        for value in modul.MembersFunctionsAndValues do
            writeFunctionOrValue ctx value
        for entity in modul.NestedEntities do
            match entity with
            | Module _ ->
                writeModule ctx entity
            | _ ->
                writeType ctx entity
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
        ctx.Writer.WriteLine("type {0} = ", typ.DisplayName)
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

        for value in typ.MembersFunctionsAndValues do
            Debug.Assert(not value.LogicalEnclosingEntity.IsFSharpModule, "F# type should not contain module functions or values.")
            writeMember ctx value
        for entity in typ.NestedEntities do
            Debug.Assert(not entity.IsFSharpModule, "F# type should not contain modules.")
            // Nested types only happen due to C# interoperability
            writeType ctx entity
        ctx.Writer.Unindent ctx.Indentation

    and internal writeUnionCase ctx (case: FSharpUnionCase) =
        writeDocs ctx case.XmlDoc
        ctx.Writer.Write("| {0}", case.Name)
        let mutable isFirst = true
        for field in case.UnionCaseFields do
            if not isFirst then
                ctx.Writer.Write(" * ")
            else
                isFirst <- false
            writeField false ctx field
        ctx.Writer.WriteLine("")

    and internal writeField hasNewLine ctx (field: FSharpField) =
        writeDocs ctx field.XmlDoc
        if hasNewLine then
            ctx.Writer.WriteLine("{0} : {1}", field.Name, field.FieldType.Format(ctx.DisplayContext))
        else
            ctx.Writer.Write("{0} : {1}", field.Name, field.FieldType.Format(ctx.DisplayContext))

    and internal writeFunctionOrValue ctx (value: FSharpMemberFunctionOrValue) =
        Debug.Assert(value.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a valid F# module.")
        writeDocs ctx value.XmlDoc
        ctx.Writer.WriteLine("val {0} : {1}", value.DisplayName, value.FullType.Format(ctx.DisplayContext))

    and internal writeMember ctx (mem: FSharpMemberFunctionOrValue) =
        Debug.Assert(not mem.LogicalEnclosingEntity.IsFSharpModule, "The enclosing entity should be a type.")
        if mem.IsPropertyGetterMethod || mem.IsPropertySetterMethod then ()
        else
            writeDocs ctx mem.XmlDoc
            ctx.Writer.WriteLine("member {0} : {1}", mem.DisplayName, mem.FullType.Format(ctx.DisplayContext))

    and internal writeDocs ctx docs =
        for doc in docs do
            ctx.Writer.WriteLine("/// {0}", doc)

    let formatSymbol displayContext (symbol: FSharpSymbol) =
        use writer = new ColumnIndentedTextWriter()
        let ctx = { Writer = writer; Indentation = 4; DisplayContext = displayContext }
        match symbol with
        | :? FSharpEntity as entity ->
            match entity with
            | Module _ -> writeModule ctx entity
            | _ -> writeType ctx entity
        | :? FSharpMemberFunctionOrValue as mem ->
            if mem.LogicalEnclosingEntity.IsFSharpModule then
                writeFunctionOrValue ctx mem
            else
                writeMember ctx mem
        | _ ->
            fail "Invalid symbol in this context: %O" symbol
        writer.Dump()