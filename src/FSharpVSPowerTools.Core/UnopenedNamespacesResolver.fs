namespace FSharpVSPowerTools

type Namespace = string

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
