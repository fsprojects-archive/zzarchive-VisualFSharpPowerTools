namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler.Ast

type Namespace = LongIdent

[<NoComparison; NoEquality>]
type Entity = 
    { Namespace: Namespace
      Name: string } 
    override x.ToString() = sprintf "%A" x
       
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
    let tryCreate (ns: Namespace option) (ident: string) (fullName: string) =
        fullName
        |> Option.ofNull
        |> Option.bind (fun fullName ->
            match fullName.LastIndexOf '.' with
            | -1 -> None
            | lastDotIndex ->
                let lastIdent = fullName.Substring (lastDotIndex + 1)
                if lastIdent = ident then
                    match ns with
                    | Some ns when fullName.StartsWith ns ->
                        let rest = fullName.Substring ns.Length
                        if rest.Length = 0 || rest.[0] <> '.' then None
                        else Some (rest.Substring 1)
                    | _ -> Some fullName
                    |> Option.bind (fun name ->
                        match name.LastIndexOf '.' with
                        | -1 -> None
                        | lastDotIndex ->
                            Some { Namespace = name.Substring (0, lastDotIndex)
                                   Name = name.Substring (lastDotIndex + 1) })
                else None)

type Pos = 
    { Line: int
      Col: int }
       
module Ast =
    open System

    let findNearestOpenStatementBlock (currentLine: int) (ast: ParsedInput) : (LongIdent option * Pos) option = 
        let result = ref None
        
        let doRange (ns: LongIdent option) line col = 
            if line < currentLine then 
                match !result with
                | None -> result := Some (ns, { Line = line; Col = col})
                | Some (oldNs, { Line = oldLine}) when oldLine < line -> 
                    result := Some (ns |> Option.orElse oldNs, { Line = line; Col = col }) 
                | _ -> ()

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.iter (walkSynModuleOrNamespace []) moduleOrNamespaceList

        and walkSynModuleOrNamespace (parent: LongIdent) (SynModuleOrNamespace(ident, _, decls, _, _, _, range)) =
            if range.EndLine >= currentLine then
                let fullIdent = parent @ ident
                doRange (Some fullIdent) range.StartLine range.StartColumn
                List.iter (walkSynModuleDecl fullIdent) decls

        and walkSynModuleDecl (parent: LongIdent) (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace parent fragment
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, ident, _, _, _, _), modules, _, range) ->
                if range.EndLine >= currentLine then
                    let fullIdent = parent @ ident
                    doRange (Some fullIdent) range.StartLine (range.StartColumn + 4)
                    List.iter (walkSynModuleDecl fullIdent) modules
            | SynModuleDecl.Open (_, range) -> doRange None range.EndLine (range.StartColumn - 5)
            | _ -> ()

        match ast with 
        | ParsedInput.SigFile _input -> ()
        | ParsedInput.ImplFile input -> walkImplFileInput input

        let res = !result |> Option.map (fun (ns, pos) -> ns, { pos with Line = pos.Line + 1 }) 
        //debug "[ResolveUnopenedNamespaceSmartTagger] Ident, line, col = %A, AST = %A" (!result) ast
        res 