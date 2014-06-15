namespace FSharpVSPowerTools

open System
open Microsoft.FSharp.Compiler.Ast

type Namespace = string[]

type Entity = 
    { Namespace: string
      Name: string } 
    override x.ToString() = sprintf "%A" x
       
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
    let rec private getRelativeName (ns: Namespace) (fullName: string[]) =
        match ns, fullName with
        | [||], _ 
        | _, [||] -> fullName
        | _ when ns.[0] = fullName.[0] ->
            getRelativeName ns.[1..] fullName.[1..]
        | _ -> fullName

    let tryCreate (ns: Namespace) (ident: string) (fullName: string) =
        fullName
        |> Option.ofNull
        |> Option.bind (fun fullName ->
            let fullName = fullName.Split '.'
            if fullName.Length = 0 then None
            elif fullName.[fullName.Length - 1] <> ident then None
            else
                match getRelativeName ns fullName with
                | [||] | [|_|] -> None
                | relativeName ->
                    Some { Namespace = String.Join (".", relativeName.[0..relativeName.Length - 2])
                           Name = relativeName.[relativeName.Length - 1] })

type Pos = 
    { Line: int
      Col: int }
       
module Ast =
    open System

    let findNearestOpenStatementBlock (currentLine: int) (ast: ParsedInput) : (Namespace * Pos) option = 
        let result = ref None
        
        let doRange (ns: LongIdent) line col = 
            if line < currentLine then 
                match !result with
                | None -> result := Some (ns, { Line = line; Col = col})
                | Some (oldNs, { Line = oldLine}) when oldLine < line -> 
                    result := Some ((match ns with [] -> oldNs | _ -> ns), { Line = line; Col = col }) 
                | _ -> ()

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.iter (walkSynModuleOrNamespace []) moduleOrNamespaceList

        and walkSynModuleOrNamespace (parent: LongIdent) (SynModuleOrNamespace(ident, _, decls, _, _, _, range)) =
            if range.EndLine >= currentLine then
                let fullIdent = parent @ ident
                doRange fullIdent range.StartLine range.StartColumn
                List.iter (walkSynModuleDecl fullIdent) decls

        and walkSynModuleDecl (parent: LongIdent) (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace parent fragment
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, ident, _, _, _, _), modules, _, range) ->
                if range.EndLine >= currentLine then
                    let fullIdent = parent @ ident
                    doRange fullIdent range.StartLine (range.StartColumn + 4)
                    List.iter (walkSynModuleDecl fullIdent) modules
            | SynModuleDecl.Open (_, range) -> doRange [] range.EndLine (range.StartColumn - 5)
            | _ -> ()

        match ast with 
        | ParsedInput.SigFile _input -> ()
        | ParsedInput.ImplFile input -> walkImplFileInput input

        let res = !result |> Option.map (fun (ns, pos) -> 
            ns |> List.map (fun x -> string x) |> List.toArray, { pos with Line = pos.Line + 1 }) 
        //debug "[ResolveUnopenedNamespaceSmartTagger] Ident, line, col = %A, AST = %A" (!result) ast
        res 