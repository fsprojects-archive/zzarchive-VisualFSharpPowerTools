module FSharpVSPowerTools.Rename.Checks

open Microsoft.FSharp.Compiler.PrettyNaming

let DoubleBackTickDelimiter = "``"

let isDoubleBacktickIdent (s: string) =
    let doubledDelimiter = 2 * DoubleBackTickDelimiter.Length
    if s.StartsWith(DoubleBackTickDelimiter) && s.EndsWith(DoubleBackTickDelimiter) && s.Length > doubledDelimiter then
        let inner = s.Substring(DoubleBackTickDelimiter.Length, s.Length - doubledDelimiter)
        not (inner.Contains(DoubleBackTickDelimiter))
    else false

let isOperator (s: string) = 
    let allowedChars = Set.ofList ['!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~']
    (IsPrefixOperator s || IsInfixOperator s || IsTernaryOperator s)
    && (s.ToCharArray() |> Array.forall (fun c -> Set.contains c allowedChars))

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
    let isAlreadyEncapsulated = name.StartsWith DoubleBackTickDelimiter && name.EndsWith DoubleBackTickDelimiter

    if isAlreadyEncapsulated then name
    elif isOperator name then name
    elif isKeyWord || not (isIdentifier name) then DoubleBackTickDelimiter + name + DoubleBackTickDelimiter
    else name