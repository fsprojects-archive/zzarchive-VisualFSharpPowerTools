namespace FSharpVSPowerTools

open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices

type SymbolKind =
    | Ident
    | Operator
    | GenericTypeParameter
    | StaticallyResolvedTypeParameter
    | Other

type Symbol =
    { Kind: SymbolKind
      Line: int
      LeftColumn: int
      RightColumn: int
      Text: string }
    member x.Range = x.Line, x.LeftColumn, x.Line, x.RightColumn

[<RequireQualifiedAccess>]
type SymbolLookupKind =
    | Fuzzy
    | ByRightColumn
    | ByLongIdent

type internal DraftToken =
    { Kind: SymbolKind
      LeftColumn: int
      RightColumn: int
      Tag: int }

module Lexer =
    /// Get the array of all lex states in current source
    let internal getLexStates defines (source: string) =
        [|
            /// Iterate through the whole line to get the final lex state
            let rec loop (lineTokenizer: FSharpLineTokenizer) lexState =
                match lineTokenizer.ScanToken lexState with
                | None, newLexState -> newLexState
                | Some _, newLexState ->
                    loop lineTokenizer newLexState

            let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")
            let lines = String.getLines source
            let lexState = ref 0L
            for line in lines do 
                yield !lexState
                let lineTokenizer = sourceTokenizer.CreateLineTokenizer line
                lexState := loop lineTokenizer !lexState
        |]

    // Provide a default implementation where we cache lex states of the current document.
    // Assume that current document will be queried repeatedly
    let queryLexState =
        let currentDocumentState = ref None
        fun source defines line ->
            let lexStates = 
                match !currentDocumentState with
                | Some (lexStates, s, d) when s = source && d = defines ->
                    lexStates
                // OPTIMIZE: if the new document has the current document as a prefix, 
                // we can reuse lexing results and process only the added part.
                | _ ->
                    debug "queryLexState: lexing current document"
                    let lexStates = getLexStates defines source
                    currentDocumentState := Some (lexStates, source, defines) 
                    lexStates
            Debug.Assert(line >= 0 && line < Array.length lexStates, "Should have lex states for every line.")
            lexStates.[line]

    /// Return all tokens of current line
    let tokenizeLine source (args: string[]) line lineStr queryLexState =
        let defines =
            args |> Seq.choose (fun s -> if s.StartsWith "--define:" then Some s.[9..] else None)
                 |> Seq.toList
        let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")
        let lineTokenizer = sourceTokenizer.CreateLineTokenizer lineStr
        let rec loop lexState acc =
            match lineTokenizer.ScanToken lexState with
            | Some tok, state -> loop state (tok :: acc)
            | _ -> List.rev acc
        loop (queryLexState source defines line) []

    let (|IdentToken|_|) (t: FSharpTokenInfo) = if t.CharClass = FSharpTokenCharKind.Identifier then Some() else None
    let (|OperatorToken|_|) (t: FSharpTokenInfo) = if t.ColorClass = FSharpTokenColorKind.Operator then Some() else None
    let (|GenericTypeParameterPrefix|_|) (t: FSharpTokenInfo) = if t.Tag = FSharpTokenTag.QUOTE then Some() else None
    let (|LeftParenToken|_|) (t: FSharpTokenInfo) = if t.Tag = FSharpTokenTag.LPAREN then Some() else None
    let (|RightParenToken|_|) (t: FSharpTokenInfo) = if t.Tag = FSharpTokenTag.RPAREN then Some() else None
    let (|BarToken|_|) (t: FSharpTokenInfo) = if t.Tag = 56 then Some() else None
    let (|StaticallyResolvedTypeParameterPrefix|_|) (lineStr: string) (t: FSharpTokenInfo) =
        if t.Tag = FSharpTokenTag.INFIX_AT_HAT_OP then
             // The lexer return INFIX_AT_HAT_OP token for both "^" and "@" symbols.
             // We have to check the char itself to distinguish one from another.
             if t.FullMatchedLength = 1 && lineStr.[t.LeftColumn] = '^' then Some()
             else None
        else None

    // Returns symbol at a given position.
    let getSymbolFromTokens (tokens: FSharpTokenInfo list) line col (lineStr: string) lookupKind: Symbol option =
        // Operators: Filter out overlapped operators (>>= operator is tokenized as three distinct tokens: GREATER, GREATER, EQUALS. 
        // Each of them has FullMatchedLength = 3. So, we take the first GREATER and skip the other two).
        //
        // Generic type parameters: we convert QUOTE + IDENT tokens into single IDENT token, altering its LeftColumn 
        // and FullMathedLength (for "'type" which is tokenized as (QUOTE, left=2) + (IDENT, left=3, length=4) 
        // we'll get (IDENT, left=2, length=5).
        //
        // Statically resolved type parameters: we convert INFIX_AT_HAT_OP + IDENT tokens into single IDENT token, altering its LeftColumn 
        // and FullMathedLength (for "^type" which is tokenized as (INFIX_AT_HAT_OP, left=2) + (IDENT, left=3, length=4) 
        // we'll get (IDENT, left=2, length=5).
        let tokens2 = 
            tokens
            |> List.fold (fun (acc, lastTokens: FSharpTokenInfo list) token ->
                match lastTokens with
                | t :: _ when token.LeftColumn <= t.RightColumn -> acc, lastTokens
                | _ ->
                    match token, lastTokens with
                    | GenericTypeParameterPrefix, _ -> acc, [ token ]
                    | StaticallyResolvedTypeParameterPrefix lineStr, _ -> acc, [ token ]
                    | LeftParenToken, _ -> acc, [ token ]
                    | BarToken, LeftParenToken :: _ -> acc, token :: lastTokens
                    | RightParenToken, BarToken :: (LeftParenToken as start) :: _ ->
                        // do not include `(` and `)` since `FSharpSymbolUse` does not include them either.
                        let draftToken =
                            { Kind = Ident 
                              LeftColumn = start.LeftColumn - 1
                              RightColumn = token.RightColumn - 1
                              Tag = FSharpTokenTag.IDENT }
                        draftToken :: acc, []
                    | _, _ ->
                        let draftToken, lastTokens =
                            match token, lastTokens with
                            | IdentToken, GenericTypeParameterPrefix :: _ ->
                                { Kind = GenericTypeParameter
                                  LeftColumn = token.LeftColumn - 1
                                  RightColumn = token.LeftColumn + token.FullMatchedLength - 1
                                  Tag = token.Tag }, []
                            | IdentToken, StaticallyResolvedTypeParameterPrefix lineStr :: _ ->
                                { Kind = StaticallyResolvedTypeParameter 
                                  LeftColumn = token.LeftColumn - 1
                                  RightColumn = token.LeftColumn + token.FullMatchedLength - 1
                                  Tag = token.Tag }, []
                            | IdentToken, _ -> 
                                { Kind = Ident 
                                  LeftColumn = token.LeftColumn
                                  RightColumn = token.LeftColumn + token.FullMatchedLength - 1
                                  Tag = token.Tag }, lastTokens
                            | OperatorToken, _ -> 
                                { Kind = Operator
                                  LeftColumn = token.LeftColumn
                                  RightColumn = token.LeftColumn + token.FullMatchedLength - 1
                                  Tag = token.Tag }, lastTokens
                            | _, _ -> 
                                { Kind = Other 
                                  LeftColumn = token.LeftColumn
                                  RightColumn = token.LeftColumn + token.FullMatchedLength - 1
                                  Tag = token.Tag }, lastTokens
                        draftToken :: acc, lastTokens
                ) ([], [])
            |> fst
           
        // One or two tokens that in touch with the cursor (for "let x|(g) = ()" the tokens will be "x" and "(")
        let tokensUnderCursor = 
            match lookupKind with
            | SymbolLookupKind.Fuzzy ->
                tokens2 |> List.filter (fun x -> x.LeftColumn <= col && x.RightColumn + 1 >= col)
            | SymbolLookupKind.ByRightColumn ->
                tokens2 |> List.filter (fun x -> x.RightColumn = col)
            | SymbolLookupKind.ByLongIdent ->
                tokens2 |> List.filter (fun x -> x.LeftColumn <= col)
                
        //printfn "Filtered tokens: %+A" tokensUnderCursor
        match lookupKind with
        | SymbolLookupKind.ByLongIdent ->
            // Try to find start column of the long identifiers
            // Assume that tokens are ordered in an decreasing order of start columns
            let rec tryFindStartColumn tokens =
               match tokens with
               | ({ DraftToken.Kind = Ident } as token1) :: ({ DraftToken.Kind = Operator } as token2) :: remainingTokens ->
                    if token2.Tag = FSharpTokenTag.DOT then
                        tryFindStartColumn remainingTokens
                    else
                        Some token1.LeftColumn
               | ({ DraftToken.Kind = Ident } as token) :: _ -> Some token.LeftColumn
               | _ :: _ | [] ->
                   None
            let decreasingTokens =
                match tokensUnderCursor |> List.sortBy (fun token -> - token.LeftColumn) with
                // Skip the first dot if it is the start of the identifier
                | ({ DraftToken.Kind = Operator } as token) :: remainingTokens when token.Tag = FSharpTokenTag.DOT ->
                    remainingTokens
                | newTokens -> newTokens
            
            match decreasingTokens with
            | [] -> None
            | first :: _ ->
                tryFindStartColumn decreasingTokens
                |> Option.map (fun leftCol ->
                    { Kind = Ident
                      Line = line
                      LeftColumn = leftCol
                      RightColumn = first.RightColumn + 1
                      Text = lineStr.[leftCol..first.RightColumn] })
        | SymbolLookupKind.Fuzzy 
        | SymbolLookupKind.ByRightColumn ->
            // Select IDENT token. If failed, select OPERATOR token.
            tokensUnderCursor
            |> List.tryFind (fun { DraftToken.Kind = k } -> 
                match k with 
                | Ident | GenericTypeParameter | StaticallyResolvedTypeParameter -> true 
                | _ -> false) 
            |> Option.orTry (fun _ -> tokensUnderCursor |> List.tryFind (fun { DraftToken.Kind = k } -> k = Operator))
            |> Option.map (fun token ->
                { Kind = token.Kind
                  Line = line
                  LeftColumn = token.LeftColumn
                  RightColumn = token.RightColumn + 1
                  Text = lineStr.[token.LeftColumn..token.RightColumn] })
    
    let getSymbol source line col lineStr lookupKind (args: string[]) queryLexState =
        let tokens = tokenizeLine source args line lineStr queryLexState
        try
            getSymbolFromTokens tokens line col lineStr lookupKind
        with e ->
            debug "Getting lex symbols failed with %O" e
            None 