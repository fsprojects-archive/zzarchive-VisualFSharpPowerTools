module FSharpVSPowerTools.Rename.Checks

open Microsoft.FSharp.Compiler.PrettyNaming

let private delimiter = "``"

let private isDoubleBacktickIdent (s: string) =
    if s.StartsWith(delimiter) && s.EndsWith(delimiter) && s.Length > 4 then
        let inner = s.Substring(delimiter.Length, s.Length - (2 * delimiter.Length))
        not (inner.Contains(delimiter))
    else false

let isIdentifier (s: string) =
    if isDoubleBacktickIdent s then
        true
    else
        s |> Seq.mapi (fun i c -> i, c)
          |> Seq.forall (fun (i, c) -> 
                if i = 0 then IsIdentifierFirstCharacter c else IsIdentifierPartCharacter c) 

/// Encapsulates identifiers for rename operations if needed
let encapsulateIdentifier name =
    let isKeyWord = List.exists ((=) name) Microsoft.FSharp.Compiler.Lexhelp.Keywords.keywordNames    

    if name.StartsWith delimiter && name.EndsWith delimiter then name // already encapsulated
    elif isKeyWord || not (isIdentifier name) then delimiter + name + delimiter
    else name