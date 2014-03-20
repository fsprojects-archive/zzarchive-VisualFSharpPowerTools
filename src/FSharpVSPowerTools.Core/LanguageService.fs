namespace FSharp.CompilerBinding

open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools

// --------------------------------------------------------------------------------------
/// Wraps the result of type-checking and provides methods for implementing
/// various IntelliSense functions (such as completion & tool tips). Provides default
/// empty/negative results if information is missing.
type ParseAndCheckResults private (infoOpt: (CheckFileResults * ParseFileResults) option) =

    new (checkResults, parseResults) = ParseAndCheckResults(Some (checkResults, parseResults))

    static member Empty = ParseAndCheckResults(None)

    member x.GetSymbolAtLocation(line, col, lineStr, identIsland) =
        match infoOpt with 
        | None -> None
        | Some (checkResults, _parseResults) -> 
            checkResults.GetSymbolAtLocationAlternate(line, col, lineStr, identIsland)

    member x.GetUsesOfSymbolInFile(symbol) =
        match infoOpt with 
        | None -> [| |]
        | Some (checkResults, _parseResults) -> checkResults.GetUsesOfSymbolInFile(symbol)

    member x.GetAllUsesOfAllSymbolsInFile() =
        match infoOpt with
        | None -> [| |]
        | Some (checkResults, _) -> checkResults.GetAllUsesOfAllSymbolsInFile()

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
            

[<RequireQualifiedAccess>]
type AllowStaleResults = 
    /// Allow checker results where the source doesn't even match
    | MatchingFileName
    /// Allow checker results where the source matches but where the background builder may not have caught up yet after some other change
    /// (such as a saved change in an earlier file in the compilation order, or a saved change in a project or DLL this project depends on).
    ///
    /// This gives good, fast, accurate results for repeated requests to the same file text. Semantic responsiveness will be degraded
    /// during eiting of the file.
    | MatchingSource
    /// Don't allow stale results. This waits for all background changes relevant to the file to propagate, and forces a recheck of the file text
    /// regardless of whether if has been recently checked or not.
    | No

type ParseAndCheckResults with
    member x.GetUsesOfSymbolInFileAtLocation (line, col, lineStr, ident) =
        match x.GetSymbolAtLocation(line+1, col, lineStr, [ident]) with
        | Some symbol ->
            let refs = x.GetUsesOfSymbolInFile(symbol)
            Some(symbol, ident, refs)
        | None -> None
  
// --------------------------------------------------------------------------------------
// Language service 

/// Provides functionality for working with the F# interactive checker running in background
type LanguageService (dirtyNotify) =
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
            Debug.WriteLine("Worker: Awaiting request") 
            let! (fileName, source, options, reply: AsyncReplyChannel<_> ) = mbox.Receive()
            
            let fileName = fixFileName(fileName)            
            
            Debug.WriteLine("Worker: Parse and typecheck source...")
            let! parseResults, checkAnswer = checker.ParseAndCheckFileInProject(fileName, 0, source,options, IsResultObsolete(fun () -> false), null )
              
            Debug.WriteLine(sprintf "Worker: Parse completed")

            // Construct new typed parse result if the task succeeded
            let results =
              match checkAnswer with
              | CheckFileAnswer.Succeeded(checkResults) ->
                  // Handle errors on the GUI thread
                  Debug.WriteLine(sprintf "LanguageService: Update typed info - HasFullTypeCheckInfo? %b" checkResults.HasFullTypeCheckInfo)
                  ParseAndCheckResults(checkResults, parseResults)
              | _ -> 
                  Debug.WriteLine("LanguageService: Update typed info - failed")
                  ParseAndCheckResults.Empty
                  
            reply.Reply results
        })

  /// Constructs options for the interactive checker for the given file in the project under the given configuration.
  member x.GetCheckerOptions(fileName, projFilename, source, files, args, targetFramework) =
    let ext = Path.GetExtension(fileName)
    let opts = 
      if (ext = ".fsx" || ext = ".fsscript") then
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        x.GetScriptCheckerOptions(fileName, projFilename, source, targetFramework)
          
      // We are in a project - construct options using current properties
      else
        x.GetProjectCheckerOptions(projFilename, files, args)
    opts
   
  /// Constructs options for the interactive checker for the given script file in the project under the given configuration. 
  member x.GetScriptCheckerOptions(fileName, projFilename, source, targetFramework) =
    let opts = 
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        try 
          let fileName = fixFileName(fileName)
          Debug.WriteLine (sprintf "GetScriptCheckerOptions: Creating for stand-alone file or script: '%s'" fileName )
          let opts = checker.GetProjectOptionsFromScript(fileName, source, fakeDateTimeRepresentingTimeLoaded projFilename)
          
          // The InteractiveChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
          if opts.ProjectOptions |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then opts
          else 
            // Add assemblies that may be missing in the standard assembly resolution
            Debug.WriteLine("GetScriptCheckerOptions: Adding missing core assemblies.")
            let dirs = FSharpEnvironment.getDefaultDirectories (FSharpCompilerVersion.LatestKnown, targetFramework )
            {opts with ProjectOptions = [| yield! opts.ProjectOptions
                                           match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                                           | Some fn -> yield sprintf "-r:%s" fn
                                           | None -> Debug.WriteLine("Resolution: FSharp.Core assembly resolution failed!")
                                           match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
                                           | Some fn -> yield sprintf "-r:%s" fn
                                           | None -> Debug.WriteLine("Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!") |]}
        with e -> failwithf "Exception when getting check options for '%s'\n.Details: %A" fileName e

    // Print contents of check option for debugging purposes
    Debug.WriteLine(sprintf "GetScriptCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, ProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
                         opts.ProjectFileName opts.ProjectFileNames opts.ProjectOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules)
    opts
   
  /// Constructs options for the interactive checker for a project under the given configuration. 
  member x.GetProjectCheckerOptions(projFilename, files, args) =
    let opts = 
      
      // We are in a project - construct options using current properties
        //Debug.WriteLine (sprintf "GetProjectCheckerOptions: Creating for project '%s'" projFilename )

        {ProjectFileName = projFilename
         ProjectFileNames = files
         ProjectOptions = args
         IsIncompleteTypeCheckEnvironment = false
         UseScriptResolutionRules = false   
         LoadTime = fakeDateTimeRepresentingTimeLoaded projFilename
         UnresolvedReferences = None
         ReferencedProjects = [||] } 

    // Print contents of check option for debugging purposes
    //Debug.WriteLine(sprintf "GetProjectCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, ProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
    //                     opts.ProjectFileName opts.ProjectFileNames opts.ProjectOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules)
    opts
    
  

  member x.ParseFileInProject(projectFilename, fileName:string, src, files, args, targetFramework) = 
    let opts = x.GetCheckerOptions(fileName, projectFilename, src, files, args, targetFramework)
    Debug.WriteLine(sprintf "Parsing: Get untyped parse result (fileName=%s)" fileName)
    checker.ParseFileInProject(fileName, src, opts)

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
  member x.ParseAndCheckFileInProject(projectFilename, fileName:string, src, files, args, targetFramework, stale) = 
   async {
    let opts = x.GetCheckerOptions(fileName, projectFilename, src, files, args, targetFramework)
    match x.TryGetStaleTypedParseResult(fileName, opts, src, stale)  with
    | Some results -> return results
    | None -> 
        let opts = x.GetCheckerOptions(fileName, projectFilename,  src, files , args, targetFramework)
        Debug.WriteLine(sprintf "Parsing: Trigger parse (fileName=%s)" fileName)
        let! results = mbox.PostAndAsyncReply(fun r -> fileName, src, opts, r)
        Debug.WriteLine(sprintf "Worker: Starting background compilations")
        checker.StartBackgroundCompile(opts)
        return results
   }

  /// Get all the uses of a symbol in the given file (using 'source' as the source for the file)
  member x.GetUsesOfSymbolAtLocationInFile(projectFilename, fileName, source, files, line:int, col, lineStr, args, targetFramework, stale, queryLexState) =
   async { 
    match SymbolParser.getSymbol source line col lineStr args queryLexState with
    | Some sym ->
        let! checkResults = 
            x.ParseAndCheckFileInProject(projectFilename, fileName, source, files, args, targetFramework, stale)

        return checkResults.GetUsesOfSymbolInFileAtLocation (line, sym.RightColumn, lineStr, sym.Text)
    | None -> return None 
   }

  member x.GetUsesOfSymbolInProject(projectFilename, file, source, files, args, framework, symbol:FSharpSymbol) =
   async { 
    let projectOptions = x.GetCheckerOptions(file, projectFilename, source, files, args, framework)

    //parse and retrieve Checked Project results, this has the entity graph and errors etc
    let! projectResults = checker.ParseAndCheckProject(projectOptions) 
  
    let refs = projectResults.GetUsesOfSymbol(symbol)
    return refs }

  /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
  member x.GetUsesOfSymbolInProjectAtLocationInFile(projectFilename, fileName, source, files, line:int, col, lineStr, args, targetFramework, queryLexState) =
     async { 
         match SymbolParser.getSymbol source line col lineStr args queryLexState with
         | Some sym ->
             let! checkResults = x.ParseAndCheckFileInProject(projectFilename, fileName, source, files, args, targetFramework, AllowStaleResults.MatchingSource)
         
             match checkResults.GetSymbolAtLocation(line+1, sym.RightColumn, lineStr, [sym.Text]) with
             | Some symbol ->
                 let projectOptions = x.GetCheckerOptions(fileName, projectFilename, source, files, args, targetFramework)
                 let! projectResults = checker.ParseAndCheckProject(projectOptions) 
                 let refs = projectResults.GetUsesOfSymbol(symbol)
                 return Some(symbol, sym.Text, refs)
             | None -> return None
         | None -> return None 
     }

  member x.InvalidateConfiguration(options) = checker.InvalidateConfiguration(options)

  // additions

  member x.Checker = checker

  member x.ProcessParseTrees(projectFilename, openDocuments, files: string[], args, targetFramework, parseTreeHandler, ct: System.Threading.CancellationToken) = 
      let rec loop i options = 
          if not ct.IsCancellationRequested && i < files.Length then
              let file = files.[i]
              let source = 
                  match Map.tryFind file openDocuments with
                  | None -> try Some(File.ReadAllText file) with _ -> None 
                  | x -> x
              let options = 
                  match source with
                  | Some source ->
                      let opts = 
                          match options with
                          | None -> 
                              x.GetCheckerOptions(file, projectFilename, source, files, args, targetFramework)
                          | Some opts -> opts
                      let parseResults = checker.ParseFileInProject(file, source, opts) |> Async.RunSynchronously
                      match parseResults.ParseTree with
                      | Some tree -> parseTreeHandler tree
                      | None -> ()
                      Some opts
                  | None -> 
                      options
              loop (i + 1) options
      loop 0 None

    member x.GetAllUsesOfAllSymbolsInFile (projectFilename, fileName:string, src, getLineStr, files, args, targetFramework, stale, queryLexState) =
        async {
            let! results = x.ParseAndCheckFileInProject (projectFilename, fileName, src, files, args, targetFramework, stale)
            let symbolUses = results.GetAllUsesOfAllSymbolsInFile()
            return symbolUses, fun line col -> SymbolParser.getSymbol src line col (getLineStr line) args queryLexState
        }