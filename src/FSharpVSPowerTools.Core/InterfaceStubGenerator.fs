module FSharpVSPowerTools.Core.InterfaceStubGenerator

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.CodeDom.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

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

    member x.Writer = indentWriter :> TextWriter

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
        Indentation: int
        ObjectIdent: string
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

    // Extension members can have apparent parents which are not F# types.
    // Hence getting the generic argument count if this is a little trickier
    let numGenericParamsOfApparentParent = 
        let pty = v.LogicalEnclosingEntity 
        pty.GenericParameters.Count
    let tps = v.GenericParameters |> Seq.skip numGenericParamsOfApparentParent
    let _typars = formatTypeArguments tps 

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

let formatInterface startColumn indentation objectIdent (methodBody: string) (e: FSharpEntity) =
    assert e.IsInterface
    use writer = new ColumnIndentedTextWriter()
    let lines = methodBody.Replace("\r\n", "\n").Split('\n')
    let ctx = { Writer = writer; Indentation = indentation; ObjectIdent = objectIdent; MethodBody = lines }
    writer.Indent startColumn
    for v in getMembers e do
        formatMember ctx v
    writer.Dump()



