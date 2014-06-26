module FSharpVSPowerTools.Rename.Checks

/// Encapsulates identifiers for rename operations if needed
let encapsulateIdentifier name = 
    let delimiter = "``"
    let specialChars = [" "; "!"]
    let isKeyWord = List.exists ((=) name) Microsoft.FSharp.Compiler.Lexhelp.Keywords.keywordNames
    let containsSpecialChar = List.exists (fun char -> name.Contains char) specialChars

    if name.StartsWith delimiter && name.EndsWith delimiter then name // already encapsulated
    elif isKeyWord || containsSpecialChar then delimiter + name + delimiter
    else name