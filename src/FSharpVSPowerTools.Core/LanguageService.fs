namespace FSharpVSPowerTools

open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools
open AsyncMaybe

// --------------------------------------------------------------------------------------
/// Wraps the result of type-checking and provides methods for implementing
/// various IntelliSense functions (such as completion & tool tips). Provides default
/// empty/negative results if information is missing.
type ParseAndCheckResults private (infoOpt: (CheckFileResults * ParseFileResults) option) =

    new (checkResults, parseResults) = ParseAndCheckResults(Some (checkResults, parseResults))

    static member Empty = ParseAndCheckResults(None)

    member x.GetSymbolUseAtLocation(line, colAtEndOfNames, lineStr, identIsland) =
        async {
            match infoOpt with 
            | None -> 
                return None
            | Some (checkResults, _parseResults) -> 
                return! checkResults.GetSymbolUseAtLocation(line, colAtEndOfNames, lineStr, identIsland)
        }

    member x.GetUsesOfSymbolInFile(symbol) =
        async {
            match infoOpt with 
            | None -> 
                return [| |]
            | Some (checkResults, _parseResults) -> 
                return! checkResults.GetUsesOfSymbolInFile(symbol)
        }

    member x.GetAllUsesOfAllSymbolsInFile() =
        async {
            match infoOpt with
            | None -> 
                return [||]
            | Some (checkResults, _) -> 
                return! checkResults.GetAllUsesOfAllSymbolsInFile()
        }

    member x.GetUntypedAst() =
        match infoOpt with 
        | None -> None
        | Some (_, parseResults) -> parseResults.ParseTree

    member x.GetErrors() =
        match infoOpt with 
        | None -> None
        | Some (checkResults, _parseResults) -> Some checkResults.Errors

    member x.GetNavigationItems() =
        match infoOpt with 
        | None -> [| |]
        | Some (_checkResults, parseResults) -> 
           // GetNavigationItems is not 100% solid and throws occasional exceptions
            try parseResults.GetNavigationItems().Declarations
            with _ -> 
                Debug.Assert(false, "couldn't update navigation items, ignoring")  
                [| |]

    member x.GetPartialAssemblySignature() =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.PartialAssemblySignature)

    member x.ProjectContext =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.ProjectContext)
            
    member x.GetIdentTooltip (line, colAtEndOfNames, lineText, names) =
        asyncMaybe {
            match infoOpt with
            | Some (checkResults, _) -> 
                let tokenTag = Parser.tagOfToken (Parser.token.IDENT "")
                return! checkResults.GetToolTipTextAlternate(line, colAtEndOfNames, lineText, names, tokenTag) |> liftAsync
            | None -> return! (liftMaybe None)
        }

[<RequireQualifiedAccess>]
type AllowStaleResults = 
    /// Allow checker results where the source doesn't even match
    | MatchingFileName
    /// Allow checker results where the source matches but where the background builder may not have caught up yet after some other change
    /// (such as a saved change in an earlier file in the compilation order, or a saved change in a project or DLL this project depends on).
    ///
    /// This gives good, fast, accurate results for repeated requests to the same file text. Semantic responsiveness will be degraded
    /// during edition of the file.
    | MatchingSource
    /// Don't allow stale results. This waits for all background changes relevant to the file to propagate, and forces a recheck of the file text
    /// regardless of whether if has been recently checked or not.
    | No

type ParseAndCheckResults with
    member x.GetUsesOfSymbolInFileAtLocation (line, col, lineStr, ident) =
        async {
            let! result = x.GetSymbolUseAtLocation(line+1, col, lineStr, [ident]) 
            match result with
            | Some symbolUse ->
                let! refs = x.GetUsesOfSymbolInFile(symbolUse.Symbol)
                return Some(symbolUse.Symbol, ident, refs)
            | None -> 
                return None
        }

type WordSpan = 
    { Line: int
      StartCol: int
      EndCol: int }
    static member FromRange (r: Range.range) = 
        { Line = r.StartLine
          StartCol = r.StartColumn 
          EndCol = r.EndColumn }
    member x.ToRange() = x.Line, x.StartCol, x.Line, x.EndCol

[<AbstractClass>]
type LexerBase() = 
    abstract GetSymbolFromTokensAtLocation: TokenInformation list * line: int * col: int -> Symbol option
    abstract TokenizeLine: line: int -> TokenInformation list
    member x.GetSymbolAtLocation (line: int, col: int) =
           x.GetSymbolFromTokensAtLocation (x.TokenizeLine line, line, col)

[<NoComparison>]
type SymbolUse =
    { SymbolUse: FSharpSymbolUse 
      IsUsed: bool
      FullNames: Idents[] Lazy }

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

// --------------------------------------------------------------------------------------
// Language service 

/// Provides functionality for working with the F# interactive checker running in background
type LanguageService (dirtyNotify, ?fileSystem: IFileSystem) =

  do Option.iter (fun fs -> Shim.FileSystem <- fs) fileSystem

  // Create an instance of interactive checker. The callback is called by the F# compiler service
  // when its view of the prior-typechecking-state of the start of a file has changed, for example
  // when the background typechecker has "caught up" after some other file has been changed, 
  // and its time to re-typecheck the current file.
  let checker = 
    let checker = InteractiveChecker.Create(200)
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
   
  // We use an additional mailbox processor here so we can get work off the GUI 
  // thread and timeout on synchronous requests. Note, there is already a background thread in F.C.S.
  //
  // Every request to the mailbox is 'PostAndReply' or 'PostAndAsyncReply'.  This means the requests are
  // a lot like a function call, except 
  //   (a) they may be asynchronous (reply is interleaved on the UI thread)
  //   (b) they may be on on a timeout (to prevent blocking the UI thread)
  //   (c) only one request is active at a time, the rest are in the queue

  let mbox = MailboxProcessor.Start(fun mbox -> 
      async { 
          while true do
              debug "Worker: Awaiting request"
              let! (fileName, source, options, reply: AsyncReplyChannel<_>) = mbox.Receive()
              let fileName = fixFileName (fileName)
              debug "Worker: Parse and typecheck source..."
              let! res = 
                  Async.Catch (checker.ParseAndCheckFileInProject (fileName, 0, source, options, IsResultObsolete(fun () -> false), null))
              debug "Worker: Parse completed"
              // Construct new typed parse result if the task succeeded
              let results = 
                  match res with
                  | Choice1Of2 (parseResults, CheckFileAnswer.Succeeded(checkResults)) ->
                      // Handle errors on the GUI thread
                      debug "[LanguageService] Update typed info - HasFullTypeCheckInfo? %b" checkResults.HasFullTypeCheckInfo
                      ParseAndCheckResults(checkResults, parseResults)
                  | Choice1Of2 _ ->
                      debug "[LanguageService] Update typed info - failed"
                      ParseAndCheckResults.Empty
                  | Choice2Of2 e -> 
                      debug "[LanguageService] Calling checker.ParseAndCheckFileInProject failed: %A" e
                      fail "ParseAndCheckFileInProject fails. There is probably a bug in the language service."
                      ParseAndCheckResults.Empty  
              reply.Reply results
      })

  /// Constructs options for the interactive checker for the given file in the project under the given configuration.
  member x.GetCheckerOptions(fileName, projFilename, source, files, args, referencedProjects, targetFramework) =
    let ext = Path.GetExtension(fileName)
    let opts = 
      if (ext = ".fsx" || ext = ".fsscript") then
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        x.GetScriptCheckerOptions(fileName, projFilename, source, targetFramework)
          
      // We are in a project - construct options using current properties
      else async { return x.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) }
    opts

  /// Constructs options for the interactive checker for the given script file in the project under the given configuration. 
  member x.GetScriptCheckerOptions(fileName, projFilename, source, targetFramework) =
      async {
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        try 
            let fileName = fixFileName(fileName)
            Debug.WriteLine (sprintf "GetScriptCheckerOptions: Creating for stand-alone file or script: '%s'" fileName )
            let! opts = checker.GetProjectOptionsFromScript(fileName, source, fakeDateTimeRepresentingTimeLoaded projFilename)
                
            let results =         
                // The InteractiveChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
                if opts.ProjectOptions |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then opts
                else 
                // Add assemblies that may be missing in the standard assembly resolution
                Debug.WriteLine("GetScriptCheckerOptions: Adding missing core assemblies.")
                let dirs = FSharpEnvironment.getDefaultDirectories (FSharpCompilerVersion.LatestKnown, targetFramework )
                {opts with ProjectOptions = [|  yield! opts.ProjectOptions
                                                match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                                                | Some fn -> yield sprintf "-r:%s" fn
                                                | None -> Debug.WriteLine("Resolution: FSharp.Core assembly resolution failed!")
                                                match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
                                                | Some fn -> yield sprintf "-r:%s" fn
                                                | None -> Debug.WriteLine("Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!") |]}
              
            // Print contents of check option for debugging purposes
            Debug.WriteLine(sprintf "GetScriptCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, ProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
                                    opts.ProjectFileName opts.ProjectFileNames opts.ProjectOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules)
        
            return results
        with e -> 
            return failwithf "Exception when getting check options for '%s'\n.Details: %A" fileName e
      }
   
  /// Constructs options for the interactive checker for a project under the given configuration. 
  member x.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) =
    { ProjectFileName = projFilename
      ProjectFileNames = files
      ProjectOptions = args
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = fakeDateTimeRepresentingTimeLoaded projFilename
      UnresolvedReferences = None
      ReferencedProjects = referencedProjects }

  member x.ParseFileInProject(projectOptions, fileName: string, src) = 
    async {
        Debug.WriteLine(sprintf "Parsing: Get untyped parse result (fileName=%s)" fileName)
        return! checker.ParseFileInProject(fileName, src, projectOptions)
    }

  member internal x.TryGetStaleTypedParseResult(fileName:string, options, src, stale)  = 
    // Try to get recent results from the F# service
    let res = 
        match stale with 
        | AllowStaleResults.MatchingFileName -> checker.TryGetRecentTypeCheckResultsForFile(fileName, options) 
        | AllowStaleResults.MatchingSource -> checker.TryGetRecentTypeCheckResultsForFile(fileName, options, source=src) 
        | AllowStaleResults.No -> None
    match res with 
    | Some (untyped,typed,_) when typed.HasFullTypeCheckInfo  -> Some (ParseAndCheckResults(typed, untyped))
    | _ -> None

(*
 // This is currently unused, but could be used in the future to ensure reactivity and analysis-responsiveness in some situations.
 // The method in the corresponding fsharpbinding code is used by Xamarin Studio.
  member x.GetTypedParseResultWithTimeout(projectFilename, fileName:string, src, files, args, stale, timeout, targetFramework)  : ParseAndCheckResults = 
    let opts = x.GetCheckerOptions(fileName, projectFilename, src, files, args, targetFramework)
    Debug.WriteLine("Parsing: Get typed parse result, fileName={0}", [|fileName|])
    // Try to get recent results from the F# service
    match x.TryGetStaleTypedParseResult(fileName, opts, src, stale)  with
    | Some results ->
        Debug.WriteLine(sprintf "Parsing: using stale results")
        results
    | None -> 
        Debug.WriteLine(sprintf "Worker: Not using stale results - trying typecheck with timeout")
        // If we didn't get a recent set of type checking results, we put in a request and wait for at most 'timeout' for a response
        mbox.PostAndReply((fun reply -> (fileName, src, opts, reply)), timeout = timeout)
*)

  /// Parses and checks the given file in the given project under the given configuration. Asynchronously
  /// returns the results of checking the file.
  member x.ParseAndCheckFileInProject(opts, fileName: string, src, stale) = 
      async { 
          match x.TryGetStaleTypedParseResult(fileName, opts, src, stale) with
          | Some results -> return results
          | None -> 
              Debug.WriteLine(sprintf "Parsing: Trigger parse (fileName=%s)" fileName)
              let! results = mbox.PostAndAsyncReply(fun r -> fileName, src, opts, r)
              Debug.WriteLine(sprintf "Worker: Starting background compilations")
              checker.StartBackgroundCompile(opts)
              return results
      }

  /// Get all the uses of a symbol in the given file (using 'source' as the source for the file)
  member x.GetUsesOfSymbolAtLocationInFile(projectOptions, fileName, source, line, col, lineStr, args, stale, queryLexState) = 
      async { 
          match Lexer.getSymbol source line col lineStr args queryLexState with
          | Some sym ->
                let! checkResults = x.ParseAndCheckFileInProject(projectOptions, fileName, source, stale)
                return! checkResults.GetUsesOfSymbolInFileAtLocation(line, sym.RightColumn, lineStr, sym.Text)
          | _ -> return None
      }

  member x.GetUsesOfSymbolAtLocationInFile(projectOptions, fileName, source, symbol: Symbol, line, lineStr, stale) = 
      async { 
          let! checkResults = x.ParseAndCheckFileInProject(projectOptions, fileName, source, stale)
          return! checkResults.GetUsesOfSymbolInFileAtLocation(line, symbol.RightColumn, lineStr, symbol.Text)
      }

  /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
  member x.GetUsesOfSymbolInProjectAtLocationInFile(currentProjectOptions: ProjectOptions, dependentProjectsOptions: ProjectOptions seq, 
                                                    fileName, source, line:int, col, lineStr, args, queryLexState, reportProgress : (string -> int -> int -> unit) option) =     
     async { 
         match Lexer.getSymbol source line col lineStr args queryLexState with
         | Some symbol ->
             let! projectCheckResults = x.ParseAndCheckFileInProject(currentProjectOptions, fileName, source, AllowStaleResults.MatchingSource)
             let! result = projectCheckResults.GetSymbolUseAtLocation(line + 1, symbol.RightColumn, lineStr, [symbol.Text])
             match result with
             | Some fsSymbolUse ->
                 let! refs =                    
                    let dependentProjects = dependentProjectsOptions |> Seq.toArray
                    
                    dependentProjects |> Async.Array.mapi (fun index opts ->
                          async {                            
                            let projectName = System.IO.Path.GetFileNameWithoutExtension(opts.ProjectFileName)
                            reportProgress |> Option.iter (fun progress -> progress projectName index dependentProjects.Length)
                            let! projectResults = checker.ParseAndCheckProject opts
                            let! refs = projectResults.GetUsesOfSymbol fsSymbolUse.Symbol
                            return refs })
                 let refs = Array.concat refs
                 return Some(fsSymbolUse.Symbol, symbol.Text, refs)
             | None -> return None
         | _ -> return None 
     }

  /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
  member x.IsSymbolUsedInProjects(symbol: FSharpSymbol, currentProjectName: string, projectsOptions: ProjectOptions seq) =
     async { 
        return 
            projectsOptions
            |> Seq.exists (fun opts ->
                async {
                    let! projectResults = checker.ParseAndCheckProject opts
                    let! refs = projectResults.GetUsesOfSymbol symbol
                    return
                        if opts.ProjectFileName = currentProjectName then
                            refs.Length > 1
                        else refs.Length > 0 }
                |> Async.RunSynchronously)
     }

  member x.InvalidateConfiguration(options) = checker.InvalidateConfiguration(options)

  // additions

  member x.Checker = checker

  member x.ProcessParseTrees(projectFilename, openDocuments, files: string[], args, targetFramework, parseTreeHandler, ct: System.Threading.CancellationToken) = 
      let rec loop i options = 
        async {
          if not ct.IsCancellationRequested && i < files.Length then
              let file = files.[i]
              let source = 
                  match Map.tryFind file openDocuments with
                  | None -> try Some(File.ReadAllText file) with _ -> None 
                  | x -> x
              let! options = 
                async {
                  match source with
                  | Some source ->
                      let! opts = 
                        async {
                          match options with
                          | None -> 
                              return! x.GetCheckerOptions(file, projectFilename, source, files, args, [||], targetFramework)
                          | Some opts -> 
                              return opts
                        }
                      let! parseResults = checker.ParseFileInProject(file, source, opts)
                      match parseResults.ParseTree with
                      | Some tree -> parseTreeHandler tree
                      | None -> ()
                      return Some opts
                  | None -> 
                      return options
                }
              return! loop (i + 1) options
          }
      loop 0 None

    member x.GetAllUsesOfAllSymbolsInFile (projectOptions, fileName, source: string[], stale, checkForUnusedDeclarations, 
                                           getSymbolDeclProjects, lexer: LexerBase) : SymbolUse[] Async =

        let stringArrayToString (arr: string[]) = String.Join (Environment.NewLine, arr)

        async {
            let! results = x.ParseAndCheckFileInProject (projectOptions, fileName, stringArrayToString source, stale)
            let! allSymbolsUses = results.GetAllUsesOfAllSymbolsInFile()

            let allSymbolsUses =
                allSymbolsUses
                |> Array.map (fun symbolUse ->
                    let fullNames = lazy (
                        let fullName =
                            match symbolUse.Symbol with
                            | MemberFunctionOrValue func when func.IsExtensionMember && func.IsProperty ->
                                let range = symbolUse.RangeAlternate
                                let line = range.StartLine - 1
                                lexer.GetSymbolAtLocation (line, range.EndColumn) 
                                |> Option.bind (fun sym ->
                                    let origLineStr = source.[line]
                                    let adjustedLineStr = 
                                        origLineStr.Insert (sym.LeftColumn, 
                                            // we use IsGetterMethod instead of IsPropertyGetterMethod because the latter 
                                            // returns False.
                                            (if func.IsGetterMethod then "get" else "set") + "_")
                                    source.[line] <- adjustedLineStr
                                    let adjustedSym = { sym with RightColumn = sym.RightColumn + 4 }
                                    let res = x.GetUsesOfSymbolAtLocationInFile (
                                                    projectOptions, fileName, stringArrayToString source, adjustedSym, line,
                                                    adjustedLineStr, AllowStaleResults.No) |> Async.RunSynchronously
                                    source.[line] <- origLineStr
                                    res)
                                |> Option.map (fun (symbol, _, _) ->  
                                     debug "[LanguageService] Getting real FullName for %s: %s" symbolUse.Symbol.FullName symbol.FullName
                                     symbol.FullName)
                            | _ -> None
                            |> Option.getOrElse symbolUse.Symbol.FullName
                        let isAttribute = 
                            match symbolUse.Symbol with 
                            | Entity (_, entity, _) when AssemblyContentProvider.isAttribute entity -> true
                            | _ -> false 
                        [| yield fullName.Split '.'
                           if isAttribute then
                               yield fullName.Substring(0, fullName.Length - 9).Split '.' |])
                    { SymbolUse = symbolUse
                      IsUsed = true
                      FullNames = fullNames })

            let singleDefs = 
                if checkForUnusedDeclarations then
                    allSymbolsUses
                    |> Seq.groupBy (fun su -> su.SymbolUse.Symbol)
                    |> Seq.choose (fun (symbol, uses) ->
                        match symbol with
                        | UnionCase when isSymbolLocalForProject symbol -> Some symbol
                        // determining that a record, DU or module is used anywhere requires
                        // inspecting all their inclosed entities (fields, cases and func / vals)
                        // for useness, which is too expensive to do. Hence we never gray them out.
                        | Entity ((Record | UnionType | Interface | FSharpModule), _, _) -> None
                        | _ ->
                            match Seq.toList uses with
                            | [symbolUse] when symbolUse.SymbolUse.IsFromDefinition && isSymbolLocalForProject symbol ->
                                Some symbol 
                            | _ -> None)
                    |> Seq.toList
                else []

            let! notUsedSymbols =
                singleDefs 
                |> Async.List.map (fun sym ->
                    async {
                        let! opts = getSymbolDeclProjects sym
                        match opts with
                        | Some projects ->
                            let! isSymbolUsed = x.IsSymbolUsedInProjects (sym, projectOptions.ProjectFileName, projects) 
                            if isSymbolUsed then return None
                            else return Some sym
                        | None -> return None 
                    })
                |> Async.map (List.choose id)
                                    
            return
                match notUsedSymbols with
                | [] -> allSymbolsUses
                | _ ->
                    allSymbolsUses
                    |> Array.map (fun su -> 
                        { su with IsUsed = notUsedSymbols |> List.forall (fun s -> s <> su.SymbolUse.Symbol) })
        }

    member x.GetAllEntitiesInProjectAndReferencedAssemblies (projectOptions: ProjectOptions, fileName, source) =
        async {
            let! checkResults = x.ParseAndCheckFileInProject (projectOptions, fileName, source, AllowStaleResults.No)
            return 
                Some [ match checkResults.GetPartialAssemblySignature() with
                       | Some signature -> yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full signature
                       | None -> ()

                       match checkResults.ProjectContext with
                       | Some ctx ->
                           for asm in ctx.GetReferencedAssemblies() do
                               let contentType = Public // it's always Public for now since we don't support InternalsVisibleTo attribute yet
                               yield! AssemblyContentProvider.getAssemblyContent contentType asm
                       | None -> () ]
        }

    member x.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, project: ProjectOptions, file, source) =
        async {
            let! checkResults = x.ParseAndCheckFileInProject (project, file, source, AllowStaleResults.No)
            return! checkResults.GetIdentTooltip (line, colAtEndOfNames, lineStr, names)
        }