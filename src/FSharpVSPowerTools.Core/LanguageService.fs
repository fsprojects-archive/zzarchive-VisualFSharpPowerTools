﻿namespace FSharp.CompilerBinding

open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

// --------------------------------------------------------------------------------------
/// Wraps the result of type-checking and provides methods for implementing
/// various IntelliSense functions (such as completion & tool tips)
type TypedParseResult(info:CheckFileResults, untyped: ParseFileResults) =
    member x.CheckFileResults = info        
    member x.ParseFileResults = untyped 

// --------------------------------------------------------------------------------------
/// Represents request send to the background worker
/// We need information about the current file and project (options)
[<NoEquality; NoComparison>]
type internal ParseRequest (file:string, source:string, options:ProjectOptions, fullCompile:bool, afterCompleteTypeCheckCallback: (string * ErrorInfo [] -> unit) option) =
  member x.File  = file
  member x.Source = source
  member x.Options = options
  member x.StartFullCompile = fullCompile
  /// A callback that gets called asynchronously on a background thread after a full, complete and accurate typecheck of a file has finally completed.
  member x.AfterCompleteTypeCheckCallback = afterCompleteTypeCheckCallback
  
// --------------------------------------------------------------------------------------
// Language service - is a mailbox processor that deals with requests from the user
// interface - mainly to trigger background parsing or get current parsing results
// All processing in the mailbox is quick - however, if we don't have required info
// we post ourselves a message that will be handled when the info becomes available

[<NoEquality; NoComparison>]
type internal LanguageServiceMessage = 
  // Trigger parse request in ParserWorker
  | TriggerRequest of ParseRequest
  // Request for information - when we receive this, we parse and reply when information become available
  | UpdateAndGetTypedInfo of ParseRequest * AsyncReplyChannel<TypedParseResult>
  | GetTypedInfoDone of AsyncReplyChannel<TypedParseResult>

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

type DraftToken =
    { Kind: SymbolKind
      Token: TokenInformation 
      RightColumn: int }
    static member Create kind token = 
        { Kind = kind; Token = token; RightColumn = token.LeftColumn + token.FullMatchedLength - 1 }

type LanguageServiceRequest =
    { FileName: string
      ProjFileName: string
      Source: string
      Files: string array
      CompilerOptions: string array
      TargetFramework: FSharpTargetFramework }

/// Provides functionality for working with the F# interactive checker running in background
type LanguageService(dirtyNotify) =
  let debug msg = Printf.kprintf Debug.WriteLine msg

  let tryGetSymbolRange (range: Range.range option) = 
        range |> Option.map (fun dec -> dec.FileName, ((dec.StartLine-1, dec.StartColumn), (dec.EndLine-1, dec.EndColumn)))

  /// Load times used to reset type checking properly on script/project load/unload. It just has to be unique for each project load/reload.
  /// Not yet sure if this works for scripts.
  let fakeDateTimeRepresentingTimeLoaded proj = DateTime(abs (int64 (match proj with null -> 0 | _ -> proj.GetHashCode())) % 103231L)

  // Create an instance of interactive checker. The callback is called by the F# compiler service
  // when its view of the prior-typechecking-state of the start of a file has changed, for example
  // when the background typechecker has "caught up" after some other file has been changed, 
  // and its time to re-typecheck the current file.
  let checker = 
    let checker = InteractiveChecker.Create()
    checker.BeforeBackgroundFileCheck.Add dirtyNotify
    checker

  /// When creating new script file on Mac, the filename we get sometimes 
  /// has a name //foo.fsx, and as a result 'Path.GetFullPath' throws in the F#
  /// language service - this fixes the issue by inventing nicer file name.
  let fixFileName path = 
    if (try Path.GetFullPath(path) |> ignore; true
        with _ -> false) then path
    else 
      let dir = 
        if Environment.OSVersion.Platform = PlatformID.Unix ||  
           Environment.OSVersion.Platform = PlatformID.MacOSX then
          Environment.GetEnvironmentVariable("HOME") 
        else
          Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%")
      Path.Combine(dir, Path.GetFileName(path))
   
  // Mailbox of this 'LanguageService'
  let mbox = MailboxProcessor.Start(fun mbox ->
    
    // Tail-recursive loop that remembers the current state
    // (untyped and typed parse results)
    let rec loop typedInfo =
      mbox.Scan(fun msg ->
        debug "Worker: Checking message %s" (msg.GetType().Name)
        match msg, typedInfo with 
        | TriggerRequest(info), _ -> Some <| async {
          let newTypedInfo = 
           try
            debug "Worker: TriggerRequest"
            let fileName = info.File        
            debug "Worker: Request parse received"
            // Run the untyped parsing of the file and report result...
            debug "Worker: Untyped parse..."
            let untypedInfo = try checker.ParseFileInProject(fileName, info.Source, info.Options) 
                              with e -> debug "Worker: Error in UntypedParse: %O" e
                                        reraise ()
              
            // Now run the type-checking
            let fileName = fixFileName(fileName)
            debug "Worker: Typecheck source..."
            let updatedTyped = try checker.CheckFileInProjectIfReady( untypedInfo, fileName, 0, info.Source,info.Options, IsResultObsolete(fun () -> false), null )
                               with e -> debug "Worker: Error in TypeCheckSource: %O" e
                                         reraise ()
              
            // If this is 'full' request, then start background compilations too
            if info.StartFullCompile then
                debug "Worker: Starting background compilations"
                checker.StartBackgroundCompile(info.Options)
            
            debug "Worker: Parse completed"
            let file = info.File

            // Construct new typed parse result if the task succeeded
            let newTypedInfo =
              match updatedTyped with
              | Some(CheckFileAnswer.Succeeded(results)) ->
                  // Handle errors on the GUI thread
                  debug "LanguageService: Update typed info - HasFullTypeCheckInfo? %b" results.HasFullTypeCheckInfo
                  match info.AfterCompleteTypeCheckCallback with 
                  | None -> ()
                  | Some cb -> 
                      debug "Errors: Got update for: %s" (Path.GetFileName file)
                      cb(file, results.Errors)

                  match results.HasFullTypeCheckInfo with
                  | true -> Some(TypedParseResult(results, untypedInfo))
                  | _ -> typedInfo
              | _ -> 
                  debug "LanguageService: Update typed info - failed"
                  typedInfo
            newTypedInfo
           with e -> 
            debug "Errors: Got unexpected background error: %O" e
            typedInfo
          return! loop newTypedInfo }
        
        // When we receive request for information and we don't have it we trigger a 
        // parse request and then send ourselves a message, so that we can reply later
        | UpdateAndGetTypedInfo(req, reply), _ -> Some <| async { 
            debug "LanguageService: UpdateAndGetTypedInfo"
            mbox.Post(TriggerRequest(req))
            mbox.Post(GetTypedInfoDone(reply)) 
            return! loop typedInfo }
                    
        | GetTypedInfoDone(reply), (Some typedRes) -> Some <| async {
            debug "LanguageService: GetTypedInfoDone"
            reply.Reply(typedRes)
            return! loop typedInfo }

        // We didn't have information to reply to a request - keep waiting for results!
        // The caller will probably timeout.
        | GetTypedInfoDone _, None -> 
            debug "Worker: No match found for the message, leaving in queue until info is available"
            None )
        
    // Start looping with no initial information        
    loop None)

  /// Get the array of all lex states in current source
  let getLexStates defines (source: string) =
    [|
        /// Iterate through the whole line to get the final lex state
        let rec loop (lineTokenizer: LineTokenizer) lexState =
            match lineTokenizer.ScanToken lexState with
            | None, newLexState -> newLexState
            | Some _, newLexState ->
                loop lineTokenizer newLexState

        let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")
        let lines = source.Replace("\r\n","\n").Split('\r', '\n')
        let lexState = ref 0L
        for line in lines do 
            yield !lexState
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer line
            lexState := loop lineTokenizer !lexState
    |]

  // Until F.C.S returns lex states of current file, we cache lex states of the current document.
  // We assume that current document will be queried repeatedly
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

  //let tap msg x = debug "[%s] %A" msg x; x

  // Returns symbol at a given position.
  let getSymbol source line col lineStr (args: string array): Symbol option =
      let defines =
          args |> Seq.choose (fun s -> if s.StartsWith "--define:" then Some s.[9..] else None)
               |> Seq.toList

      let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")

      // get all tokens
      let tokens =
        let lineTokenizer = sourceTokenizer.CreateLineTokenizer lineStr
        let rec loop lexState acc =
            match lineTokenizer.ScanToken lexState with
            | Some tok, state -> loop state (tok :: acc)
            | _ -> List.rev acc
        loop (queryLexState source defines line) []

      let isIdentifier t = t.CharClass = TokenCharKind.Identifier
      let isOperator t = t.ColorClass = TokenColorKind.Operator

      let (|GenericTypeParameterPrefix|StaticallyResolvedTypeParameterPrefix|Other|) token =
          match token.TokenName with
          | "QUOTE" -> GenericTypeParameterPrefix
          | "INFIX_AT_HAT_OP" -> StaticallyResolvedTypeParameterPrefix
          | _ -> Other

      // Operators: Filter out overlapped oparators (>>= operator is tokenized as three distinct tokens: GREATER, GREATER, EQUALS. 
      // Each of them has FullMatchedLength = 3. So, we take the first GREATER and skip the other two).
      //
      // Generic type parameters: we convert QUOTE + IDENT tokens into single IDENT token, altering its LeftColumn 
      // and FullMathedLength (for "'type" which is tokenized as (QUOTE, left=2) + (IDENT, left=3, length=4) 
      // we'll get (IDENT, left=2, lenght=5).
      //
      // Statically resolved type parameters: we convert INFIX_AT_HAT_OP + IDENT tokens into single IDENT token, altering its LeftColumn 
      // and FullMathedLength (for "^type" which is tokenized as (INFIX_AT_HAT_OP, left=2) + (IDENT, left=3, length=4) 
      // we'll get (IDENT, left=2, lenght=5).
      let tokens = 
        tokens
        |> List.fold (fun (acc, lastToken) token ->
            match lastToken with
            | Some t when token.LeftColumn <= t.RightColumn -> acc, lastToken
            | _ ->
                match token with
                | GenericTypeParameterPrefix -> acc, Some (DraftToken.Create GenericTypeParameter token)
                | StaticallyResolvedTypeParameterPrefix -> acc, Some (DraftToken.Create StaticallyResolvedTypeParameter token)
                | Other ->
                    let draftToken =
                        match lastToken with
                        | Some { Kind = GenericTypeParameter | StaticallyResolvedTypeParameter as kind } when isIdentifier token ->
                            DraftToken.Create kind { token with LeftColumn = token.LeftColumn - 1
                                                                FullMatchedLength = token.FullMatchedLength + 1 }
                        | _ -> 
                            let kind = if isOperator token then Operator elif isIdentifier token then Ident else Other
                            DraftToken.Create kind token
                    draftToken :: acc, Some draftToken
           ) ([], None)
        |> fst
         
      // One or two tokens that in touch with the cursor (for "let x|(g) = ()" the tokens will be "x" and "(")
      let tokensUnderCursor = tokens |> List.filter (fun x -> x.Token.LeftColumn <= col && x.RightColumn + 1 >= col)

      // Select IDENT token. If failes, select OPERATOR token.
      tokensUnderCursor
      |> List.tryFind (fun (x: DraftToken) -> 
          match x.Kind with 
          | Ident | GenericTypeParameter | StaticallyResolvedTypeParameter -> true 
          | _ -> false) 
      |> Option.orElse (List.tryFind (fun (x: DraftToken) -> 
          match x.Kind with 
          | Operator -> true 
          | _ -> false) tokensUnderCursor)
      |> Option.map (fun token ->
          { Kind = token.Kind
            Line = line
            LeftColumn = token.Token.LeftColumn
            RightColumn = token.RightColumn + 1
            Text = lineStr.Substring(token.Token.LeftColumn, token.Token.FullMatchedLength) })

  /// Constructs options for the interactive checker for the given file in the project under the given configuration.
  let getCheckerOptions req = 
      let ext = Path.GetExtension(req.FileName)
  
      let opts = 
          if (ext = ".fsx" || ext = ".fsscript") then 
              // We are in a stand-alone file or we are in a project, but currently editing a script file
              try 
                  let fileName = fixFileName (req.FileName)
                  debug "CheckOptions: Creating for stand-alone file or script: '%s'" fileName
                  let opts = 
                      checker.GetProjectOptionsFromScript
                          (fileName, req.Source, fakeDateTimeRepresentingTimeLoaded req.ProjFileName)
                  // The InteractiveChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
                  if opts.ProjectOptions |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then opts
                  else 
                      // Add assemblies that may be missing in the standard assembly resolution
                      debug "CheckOptions: Adding missing core assemblies."
                      let dirs = 
                          FSharpEnvironment.getDefaultDirectories (FSharpCompilerVersion.LatestKnown, req.TargetFramework)
                      { opts with ProjectOptions = 
                                      [| yield! opts.ProjectOptions
                                         match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                                         | Some fn -> yield sprintf "-r:%s" fn
                                         | None -> debug "Resolution: FSharp.Core assembly resolution failed!"
                                     
                                         match FSharpEnvironment.resolveAssembly dirs 
                                                   "FSharp.Compiler.Interactive.Settings" with
                                         | Some fn -> yield sprintf "-r:%s" fn
                                         | None -> 
                                             debug 
                                                 "Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!" |] }
              with e -> failwithf "Exception when getting check options for '%s'\n.Details: %A" req.FileName e
          // We are in a project - construct options using current properties
          else 
              debug "GetCheckerOptions: Creating for file '%s' in project '%s'" req.FileName req.ProjFileName
              { ProjectFileName = req.ProjFileName
                ProjectFileNames = req.Files
                ProjectOptions = req.CompilerOptions
                IsIncompleteTypeCheckEnvironment = false
                UseScriptResolutionRules = false
                LoadTime = fakeDateTimeRepresentingTimeLoaded req.ProjFileName
                UnresolvedReferences = None }
      // Print contents of check option for debugging purposes
      debug 
          "GetCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, ProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
          opts.ProjectFileName opts.ProjectFileNames opts.ProjectOptions opts.IsIncompleteTypeCheckEnvironment 
          opts.UseScriptResolutionRules
      opts

  let getUsesOfSymbolAtLocation createSymbolUsesGetter req (line: int) col lineStr = 
      async { 
          let projectOptions = getCheckerOptions req
          // Parse and retrieve Checked Project results, this has the entity graph and errors etc
          let! usesGetter = createSymbolUsesGetter projectOptions
          // Get the parse results for the current file
          let! _, checkFileResults = 
              // Note this operates on the file system so the file needs to be current
              checker.GetBackgroundCheckResultsForFileInProject(req.FileName, projectOptions)
          return maybe { 
                     let! symbol = getSymbol req.Source line col lineStr req.CompilerOptions
                     // We only look up identifiers and operators, everything else isn't of interest       
                     debug "Parsing: Passed in the following symbol '%s'" symbol.Text
                     // Note we advance the caret to 'rightCol' ** due to GetSymbolAtLocation only working at the beginning/end **
                     let! fsharpSymbol = checkFileResults.GetSymbolAtLocationAlternate
                                             (line + 1, symbol.RightColumn, lineStr, [ symbol.Text ])
                     let symRangeOpt = tryGetSymbolRange fsharpSymbol.DeclarationLocation
                     let refs = usesGetter checkFileResults fsharpSymbol
                     return fsharpSymbol, symbol.Text, symRangeOpt, refs
                 }
      }

  member x.GetCheckerOptions req = getCheckerOptions req

  member x.ProcessParseTrees(req, openDocuments, parseTreeHandler, ct: System.Threading.CancellationToken) = 
    let rec loop i options = 
        if not ct.IsCancellationRequested && i < req.Files.Length then
            let file = req.Files.[i]
            let source = 
                match Map.tryFind file openDocuments with
                | None -> try Some(File.ReadAllText file) with _ -> None 
                | x -> x
            let options = 
                match source with
                | Some source ->
                    let opts = 
                        match options with
                        | None -> getCheckerOptions req
                        | Some opts -> opts
                    let parseResults = checker.ParseFileInProject(file, source, opts)
                    match parseResults.ParseTree with
                    | Some tree -> parseTreeHandler tree
                    | None -> ()
                    Some opts
                | None -> 
                    options
            loop (i + 1) options
    loop 0 None

  /// Parses and type-checks the given file in the given project under the given configuration. The callback
  /// is called after the complete typecheck has been performed.
  member x.TriggerParse (req, afterCompleteTypeCheckCallback) = 
      let opts = getCheckerOptions req
      debug "Parsing: Trigger parse (fileName=%s)" req.FileName
      mbox.Post(TriggerRequest(ParseRequest(req.FileName, req.Source, opts, true, Some afterCompleteTypeCheckCallback)))

  member x.GetUntypedParseResult req = 
      let opts = getCheckerOptions req
      debug "Parsing: Get untyped parse result (fileName=%s)" req.FileName
      let _req = ParseRequest (req.FileName, req.Source, opts, false, None)
      checker.ParseFileInProject (req.FileName, req.Source, opts)

  member x.GetTypedParseResult (req, allowRecentTypeCheckResults, timeout) : TypedParseResult = 
    let opts = getCheckerOptions req
    debug "Parsing: Get typed parse result, fileName=%s" req.FileName
    let parseReq = ParseRequest(req.FileName, req.Source, opts, false, None)
    // Try to get recent results from the F# service
    match checker.TryGetRecentTypeCheckResultsForFile(req.FileName, parseReq.Options) with
    | Some(untyped, typed, _) when typed.HasFullTypeCheckInfo && allowRecentTypeCheckResults ->
        debug "Worker: Quick parse completed - success"
        TypedParseResult(typed, untyped)
    | _ -> debug "Worker: No TryGetRecentTypeCheckResultsForFile - trying typecheck with timeout"
           // If we didn't get a recent set of type checking results, we put in a request and wait for at most 'timeout' for a response
           mbox.PostAndReply((fun repl -> UpdateAndGetTypedInfo(parseReq, repl)), timeout = timeout)

  /// Returns symbol at a given position.
  member x.GetSymbol (source, line, col, lineStr, args) = getSymbol source line col lineStr args

  member x.GetUsesOfSymbolAtLocation (req, line: int, col, lineStr) = 
      let symbolUsesGetter projectOptions = async {
          let! projectResults = checker.ParseAndCheckProject projectOptions
          debug "There are %i error(s)." projectResults.Errors.Length
          Debug.Assert(not projectResults.HasCriticalErrors, "Should abort on critical errors.") 
          return fun _ symbol -> projectResults.GetUsesOfSymbol symbol }
            
      getUsesOfSymbolAtLocation symbolUsesGetter req line col lineStr

  member x.GetUsesOfSymbolAtLocationInFile (req, line: int, col, lineStr) = 
      let symbolUsesGetter _ = async { return fun (checkFileResults: CheckFileResults) -> checkFileResults.GetUsesOfSymbolInFile }
      getUsesOfSymbolAtLocation symbolUsesGetter req line col lineStr

  member x.GetUsesOfSymbol (req, symbol: FSharpSymbol) =
      async { 
          let projectOptions = getCheckerOptions req
          //parse and retrieve Checked Project results, this has the entity graph and errors etc
          let! projectResults = checker.ParseAndCheckProject(projectOptions) 
          let symDeclRangeOpt = tryGetSymbolRange symbol.DeclarationLocation
          let refs = projectResults.GetUsesOfSymbol(symbol)
          return (symDeclRangeOpt, refs) }

  member x.Checker = checker
