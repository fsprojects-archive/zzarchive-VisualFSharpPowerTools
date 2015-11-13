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
type ParseAndCheckResults private (infoOpt: (FSharpCheckFileResults * FSharpParseFileResults) option) =

    new (checkResults, parseResults) = ParseAndCheckResults(Some (checkResults, parseResults))

    static member Empty = ParseAndCheckResults(None)

    member __.GetSymbolUseAtLocation(line, colAtEndOfNames, lineStr, identIsland) =
        async {
            match infoOpt with 
            | None -> 
                return None
            | Some (checkResults, _parseResults) -> 
                return! checkResults.GetSymbolUseAtLocation(line, colAtEndOfNames, lineStr, identIsland)
        }

    member __.GetUsesOfSymbolInFile(symbol) =
        async {
            match infoOpt with 
            | None -> 
                return [| |]
            | Some (checkResults, _parseResults) -> 
                return! checkResults.GetUsesOfSymbolInFile(symbol)
        }

    member __.GetAllUsesOfAllSymbolsInFile() =
        async {
            match infoOpt with
            | None -> 
                return [||]
            | Some (checkResults, _) -> 
                return! checkResults.GetAllUsesOfAllSymbolsInFile()
        }

    member __.GetUntypedAst() =
        match infoOpt with 
        | None -> None
        | Some (_, parseResults) -> parseResults.ParseTree

    member __.GetCheckResults() =
        match infoOpt with 
        | None -> None
        | Some (checkResults, _) -> Some checkResults

    member __.GetFormatSpecifierLocations() =
        match infoOpt with 
        | None -> None
        | Some (checkResults, _) -> Some (checkResults.GetFormatSpecifierLocations())

    member __.GetErrors() =
        match infoOpt with 
        | None -> None
        | Some (checkResults, _parseResults) -> Some checkResults.Errors

    member __.GetNavigationItems() =
        match infoOpt with 
        | None -> [| |]
        | Some (_checkResults, parseResults) -> 
           // GetNavigationItems is not 100% solid and throws occasional exceptions
            try parseResults.GetNavigationItems().Declarations
            with _ -> 
                fail "Couldn't update navigation items, ignoring"
                [| |]

    member __.GetPartialAssemblySignature() =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.PartialAssemblySignature)

    member __.ProjectContext =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.ProjectContext)

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

    member __.GetDeclarationLocation(line, col, lineStr, ident, preferSignature) =
        async {
            match infoOpt with 
            | None -> 
                return FSharpFindDeclResult.DeclNotFound FSharpFindDeclFailureReason.Unknown
            | Some (checkResults, _parseResults) -> 
                return! checkResults.GetDeclarationLocationAlternate(line+1, col, lineStr, [ident], preferSignature)       
        }
            
    member __.GetIdentTooltip (line, colAtEndOfNames, lineText, names) =
        Debug.Assert(not (List.isEmpty names), "The names should not be empty (for which GetToolTip raises exceptions).")
        asyncMaybe {
            match infoOpt with
            | Some (checkResults, _) -> 
                let tokenTag = FSharpTokenTag.IDENT
                return! checkResults.GetToolTipTextAlternate(line, colAtEndOfNames, lineText, names, tokenTag) |> liftAsync
            | None -> return! None
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

type WordSpan = 
    { SymbolKind: SymbolKind
      Line: int
      StartCol: int
      EndCol: int }
    static member FromRange kind (r: Range.range) = 
        { SymbolKind = kind
          Line = r.StartLine
          StartCol = r.StartColumn 
          EndCol = r.EndColumn }
    member x.ToRange() = x.Line, x.StartCol, x.Line, x.EndCol

[<AbstractClass>]
type LexerBase() = 
    abstract GetSymbolFromTokensAtLocation: FSharpTokenInfo list * line: int * rightCol: int -> Symbol option
    abstract TokenizeLine: line: int -> FSharpTokenInfo list
    abstract LineCount: int
    member x.GetSymbolAtLocation (line: int, col: int) =
           x.GetSymbolFromTokensAtLocation (x.TokenizeLine line, line, col)
    member x.TokenizeAll() = [|0..x.LineCount-1|] |> Array.map x.TokenizeLine

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.Collections.Concurrent
open System.Threading

type FileState =
    | Checked
    | NeedChecking
    | BeingChecked
    | Cancelled

// --------------------------------------------------------------------------------------
// Language service 

/// Provides functionality for working with the F# interactive checker running in background
type LanguageService (?backgroundCompilation: bool, ?projectCacheSize: int, ?fileSystem: IFileSystem) =

  do Option.iter (fun fs -> Shim.FileSystem <- fs) fileSystem
  let mutable errorHandler = None
  let handleCriticalErrors e file source opts = 
      errorHandler |> Option.iter (fun handle -> handle e file source opts)

  // Create an instance of interactive checker.
  let checkerInstance = 
    FSharpChecker.Create(
        projectCacheSize = defaultArg projectCacheSize 50, 
        keepAllBackgroundResolutions = false,
        keepAssemblyContents = false,
        ImplicitlyStartBackgroundWork = defaultArg backgroundCompilation true)
  
  let checkerAsync (f: FSharpChecker -> Async<'a>) = 
    let ctx = System.Threading.SynchronizationContext.Current
    async {
        do! Async.SwitchToThreadPool()
        let! r = f checkerInstance
        do! Async.SwitchToContext ctx
        return r
    }

  /// When creating new script file on Mac, the filename we get sometimes 
  /// has a name //foo.fsx, and as a result 'Path.GetFullPath' throws in the F#
  /// language service - this fixes the issue by inventing nicer file name.
  let fixFileName path = 
    if (try Path.GetFullPath path |> ignore; true with _ -> false) then path
    else 
        match Environment.OSVersion.Platform with
        | PlatformID.Unix 
        | PlatformID.MacOSX -> Environment.GetEnvironmentVariable "HOME"
        | _ -> Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%"
        </> Path.GetFileName path

  let files = ConcurrentDictionary<string, FileState>()
  
  let isResultObsolete fileName = 
      match files.TryGetValue fileName with
      | true, Cancelled -> true
      | _ -> false
  
  let parseAndCheckFileInProject(filePath, source, options) =
      async { 
          debug "[LanguageService] ParseAndCheckFileInProject - enter"
          let fixedFilePath = fixFileName filePath
          let! res = Async.Catch (checkerAsync <| fun x -> async {
              try
                   // wait until the previous checking completed
                   while files.ContainsKey filePath &&
                         (not (files.TryUpdate (filePath, BeingChecked, Checked)
                               || files.TryUpdate (filePath, BeingChecked, NeedChecking))) do
                       do! Async.Sleep 20
                   
                   debug "[LanguageService] Change state for %s to `BeingChecked`" filePath
                   debug "[LanguageService] Parse and typecheck source..."
                   return! x.ParseAndCheckFileInProject (fixedFilePath, 0, source, options, 
                                                         IsResultObsolete (fun _ -> isResultObsolete filePath), null) 
              finally 
                   if files.TryUpdate (filePath, Checked, BeingChecked) then
                       debug "[LanguageService] %s: BeingChecked => Checked" filePath
                   elif files.TryUpdate (filePath, Checked, Cancelled) then
                       debug "[LanguageService] %s: Cancelled => Checked" filePath })

          debug "[LanguageService]: Parse completed"
          // Construct new typed parse result if the task succeeded
          let results = 
              match res with
              | Choice1Of2 (parseResults, FSharpCheckFileAnswer.Succeeded checkResults) ->
                  // Handle errors on the GUI thread
                  debug "[LanguageService] ParseAndCheckFileInProject - HasFullTypeCheckInfo? %b" checkResults.HasFullTypeCheckInfo
                  debug "[LanguageService] ParseAndCheckFileInProject - Errors? %A" checkResults.Errors
                  ParseAndCheckResults(checkResults, parseResults)
              | Choice1Of2 (_, FSharpCheckFileAnswer.Aborted) ->
                  debug "[LanguageService] ParseAndCheckFileInProject - Aborted"
                  ParseAndCheckResults.Empty
              | Choice2Of2 e -> 
                  fail "[LanguageService] Unexpected type checking errors occurred for '%s' with %A" fixedFilePath options
                  fail "[LanguageService] Calling checker.ParseAndCheckFileInProject failed: %A" e
                  debug "[LanguageService] Type checking fails for '%s' with content=%A and %A.\nResulting exception: %A" fixedFilePath source options e
                  handleCriticalErrors e fixedFilePath source options
                  ParseAndCheckResults.Empty
          return results
      }

  member __.OnFileChanged filePath = 
    files.AddOrUpdate (filePath, NeedChecking, (fun _ oldState -> 
        match oldState with
        | BeingChecked -> Cancelled
        | Cancelled -> Cancelled
        | NeedChecking -> NeedChecking
        | Checked -> NeedChecking))
    |> debug "[LanguageService] %s changed: set status to %A" filePath

  member __.OnFileClosed filePath = 
    match files.TryRemove filePath with
    | true, _ -> debug "[LanguageService] %s was removed from `files` dictionary" filePath
    | _ -> ()

  /// Constructs options for the interactive checker for the given file in the project under the given configuration.
  member x.GetCheckerOptions(fileName, projFilename, source, files, args, referencedProjects, fscVersion) =
    let ext = Path.GetExtension(fileName)
    let opts = 
        if ext = ".fsx" || ext = ".fsscript" then
           // We are in a stand-alone file or we are in a project, but currently editing a script file
           x.GetScriptCheckerOptions(fileName, projFilename, source, fscVersion)
          
        // We are in a project - construct options using current properties
        else async { return x.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) }
    opts

  /// Constructs options for the interactive checker for the given script file in the project under the given configuration. 
  member __.GetScriptCheckerOptions(fileName, projFilename, source, fscVersion) =
      async {
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        try 
            let fileName = fixFileName(fileName)
            debug "GetScriptCheckerOptions: Creating for stand-alone file or script: '%s'" fileName
            let! opts = checkerInstance.GetProjectOptionsFromScript(fileName, source, fakeDateTimeRepresentingTimeLoaded projFilename)
                
            let results =         
                // The FSharpChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
                if opts.OtherOptions |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then
                    match fscVersion with
                    | FSharpCompilerVersion.FSharp_3_0
                    | FSharpCompilerVersion.FSharp_3_1 ->
                        let dirs = FSharpEnvironment.getDefaultDirectories(fscVersion, FSharpTargetFramework.NET_4_5)
                        FSharpEnvironment.resolveAssembly dirs "FSharp.Core"
                        |> Option.map (fun path -> 
                            let fsharpCoreRef = sprintf "-r:%s" path
                            { opts with OtherOptions = [| yield fsharpCoreRef
                                                          yield! opts.OtherOptions |> Seq.filter (fun s -> not (s.Contains "FSharp.Core.dll")) |] })
                        |> Option.getOrElse opts
                    | _ -> opts
                else 
                // Add assemblies that may be missing in the standard assembly resolution
                debug "GetScriptCheckerOptions: Adding missing core assemblies."
                let dirs = FSharpEnvironment.getDefaultDirectories(fscVersion, FSharpTargetFramework.NET_4_5)
                { opts with OtherOptions =  [|  yield! opts.OtherOptions
                                                match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                                                | Some fn -> yield sprintf "-r:%s" fn
                                                | None -> debug "Resolution: FSharp.Core assembly resolution failed!"
                                                match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
                                                | Some fn -> yield sprintf "-r:%s" fn
                                                | None -> debug "Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!" |] }
              
            // Print contents of check option for debugging purposes
            debug "GetScriptCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, FSharpProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
                                    results.ProjectFileName results.ProjectFileNames results.OtherOptions results.IsIncompleteTypeCheckEnvironment results.UseScriptResolutionRules        
            return results
        with e -> 
            return failwithf "Exception when getting check options for '%s'\n.Details: %A" fileName e
      }
  
  /// Constructs options for the interactive checker for a project under the given configuration. 
  member __.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) =
    let opts =
        { ProjectFileName = projFilename
          ProjectFileNames = files
          OtherOptions = args
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = false
          LoadTime = fakeDateTimeRepresentingTimeLoaded projFilename
          UnresolvedReferences = None
          ReferencedProjects = referencedProjects }
    debug "GetProjectCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, FSharpProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A, ReferencedProjects: %A" 
                                    opts.ProjectFileName opts.ProjectFileNames opts.OtherOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules opts.ReferencedProjects
    opts     

  member __.ParseFileInProject(projectOptions, fileName: string, src) = 
    async {
        debug "Parsing: Get untyped parse result (fileName=%s)" fileName
        return! checkerAsync (fun x -> x.ParseFileInProject(fileName, src, projectOptions))
    }

  member internal __.TryGetStaleTypedParseResult(fileName:string, options, src, stale)  = 
    // Try to get recent results from the F# service
    let res = 
        match stale with 
        | AllowStaleResults.MatchingFileName -> checkerInstance.TryGetRecentTypeCheckResultsForFile(fileName, options) 
        | AllowStaleResults.MatchingSource -> checkerInstance.TryGetRecentTypeCheckResultsForFile(fileName, options, source=src)
        | AllowStaleResults.No -> None
    match res with 
    | Some (untyped,typed,_) when typed.HasFullTypeCheckInfo  -> Some (ParseAndCheckResults(typed, untyped))
    | _ -> None

  /// Parses and checks the given file in the given project under the given configuration. Asynchronously
  /// returns the results of checking the file.
  member x.ParseAndCheckFileInProject(opts, fileName: string, src, stale) = 
      async { 
          match x.TryGetStaleTypedParseResult(fileName, opts, src, stale) with
          | Some results -> return results
          | None -> 
              debug "Parsing: Trigger parse (fileName=%s)" fileName
              return! parseAndCheckFileInProject(fileName, src, opts)
      }

  /// Get all the uses of a symbol in the given file (using 'source' as the source for the file)
  member x.GetUsesOfSymbolAtLocationInFile(projectOptions, fileName, source, line, col, lineStr, args, stale, queryLexState) = 
      async { 
          match Lexer.getSymbol source line col lineStr SymbolLookupKind.Fuzzy args queryLexState with
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
  member x.GetUsesOfSymbolInProjectAtLocationInFile(currentProjectOptions: FSharpProjectOptions, dependentProjectsOptions: FSharpProjectOptions seq, 
                                                    fileName, source, line:int, col, lineStr, args, queryLexState, reportProgress : (string -> int -> int -> unit) option) =     
     async { 
         match Lexer.getSymbol source line col lineStr SymbolLookupKind.Fuzzy args queryLexState with
         | Some symbol ->
             let! fileCheckResults = x.ParseAndCheckFileInProject(currentProjectOptions, fileName, source, AllowStaleResults.MatchingSource)
             let! result = fileCheckResults.GetSymbolUseAtLocation(line + 1, symbol.RightColumn, lineStr, [symbol.Text])
             match result with
             | Some fsSymbolUse ->
                 let! refs =                    
                    let dependentProjects = dependentProjectsOptions |> Seq.toArray
                    
                    dependentProjects |> Async.Array.mapi (fun index opts ->
                          async {   
                            try                         
                                let projectName = Path.GetFileNameWithoutExtension(opts.ProjectFileName)
                                reportProgress |> Option.iter (fun progress -> progress projectName index dependentProjects.Length)
                                let! projectResults = checkerAsync (fun x -> x.ParseAndCheckProject opts)
                                let! refs = projectResults.GetUsesOfSymbol fsSymbolUse.Symbol
                                return refs 
                            with e ->
                                handleCriticalErrors e fileName source opts
                                return [||]
                          })
                 let refs = Array.concat refs
                 return Some(fsSymbolUse.Symbol, symbol.Text, refs)
             | None -> return None
         | _ -> return None 
     }

  member __.InvalidateConfiguration options =
      checkerAsync <| fun checker -> async { checker.InvalidateConfiguration options }

  member __.CheckerAsync<'a> (f: FSharpChecker -> Async<'a>) = checkerAsync f
  member __.RawChecker = checkerInstance

  member __.ProcessParseTrees (opts: FSharpProjectOptions, openDocuments, files: string[], parseTreeHandler, ct: CancellationToken) =
      let rec loop i = 
          asyncMaybe {
              if not ct.IsCancellationRequested && i < files.Length then
                  let file = files.[i]
                  let! source = 
                      Map.tryFind file openDocuments 
                      |> Option.orElse (fun _ -> Option.attempt (fun _ -> File.ReadAllText file)) 
                  
                  let! parseResults = checkerInstance.ParseFileInProject(file, source, opts) |> liftAsync
                  let! ast = parseResults.ParseTree
                  parseTreeHandler file ast
                  return! loop (i + 1)
            }
      loop 0 |> Async.Ignore

    member x.GetAllUsesOfAllSymbolsInFile (projectOptions, fileName, source: string, stale, 
                                           checkForUnusedOpens, pf: Profiler) : SymbolUse[] Async =

        async {
            let! results = pf.TimeAsync "LS ParseAndCheckFileInProject" <| fun _ ->
                x.ParseAndCheckFileInProject (projectOptions, fileName, source, stale)

            let! fsharpSymbolsUses = pf.TimeAsync "LS GetAllUsesOfAllSymbolsInFile" <| fun _ ->
                results.GetAllUsesOfAllSymbolsInFile()

            let allSymbolsUses = pf.Time "LS allSymbolsUses" <| fun _ ->
                fsharpSymbolsUses
                |> Array.map (fun symbolUse -> 
                    let fullNames = 
                        match symbolUse.Symbol with
                        // Make sure that unsafe manipulation isn't executed if unused opens are disabled
                        | _ when not checkForUnusedOpens -> None
                        | MemberFunctionOrValue func when func.IsExtensionMember ->
                            if func.IsProperty then
                                let fullNames =
                                    [|
                                        if func.HasGetterMethod then
                                            yield func.GetterMethod.EnclosingEntity.TryGetFullName()
                                        if func.HasSetterMethod then
                                            yield func.SetterMethod.EnclosingEntity.TryGetFullName()
                                    |]
                                    |> Array.choose id
                                match fullNames with
                                | [||] -> None 
                                | _ -> Some fullNames
                            else 
                                match func.EnclosingEntity with
                                // C# extension method
                                | Entity Class ->
                                    let fullName = symbolUse.Symbol.FullName.Split '.'
                                    if fullName.Length > 2 then
                                        (* For C# extension methods FCS returns full name including the class name, like:
                                            Namespace.StaticClass.ExtensionMethod
                                            So, in order to properly detect that "open Namespace" actually opens ExtensionMethod,
                                            we remove "StaticClass" part. This makes C# extension methods looks identically 
                                            with F# extension members.
                                        *)
                                        let fullNameWithoutClassName =
                                            Array.append fullName.[0..fullName.Length - 3] fullName.[fullName.Length - 1..]
                                        Some [|String.Join (".", fullNameWithoutClassName)|]
                                    else None
                                | _ -> None
                        // Operators
                        | MemberFunctionOrValue func ->
                            match func with
                            | Constructor _ -> None
                            | _ -> 
                                Some [| yield func.FullName 
                                        match func.TryGetFullCompiledOperatorNameIdents() with
                                        | Some idents -> yield String.concat "." idents
                                        | None -> ()
                                     |]
                        | Entity e ->
                            match e with
                            | e, TypedAstPatterns.Attribute, _ ->
                                e.TryGetFullName()
                                |> Option.map (fun fullName ->
                                    [| fullName; fullName.Substring(0, fullName.Length - "Attribute".Length) |])
                            | e, _, _ -> 
                                e.TryGetFullName() |> Option.map (fun fullName -> [| fullName |])
                        | RecordField _
                        | UnionCase _ as symbol ->
                            Some [| let fullName = symbol.FullName
                                    yield fullName
                                    let idents = fullName.Split '.'
                                    // Union cases/Record fields can be accessible without mentioning the enclosing type. 
                                    // So we add a FullName without having the type part.
                                    if idents.Length > 1 then
                                        yield String.Join (".", Array.append idents.[0..idents.Length - 3] idents.[idents.Length - 1..])
                                 |]   
                        |  _ -> None
                        |> Option.getOrElse [|symbolUse.Symbol.FullName|]
                        |> Array.map (fun fullName -> fullName.Split '.')
                    
                    { SymbolUse = symbolUse
                      IsUsed = true
                      FullNames = fullNames })
            return allSymbolsUses }

    /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
    member __.IsSymbolUsedInProjects(symbol: FSharpSymbol, currentProjectName: string, projectsOptions: FSharpProjectOptions seq, pf: Profiler) =
        projectsOptions
        |> Seq.toArray
        |> Async.Array.exists (fun opts ->
            async {
                let! projectResults = pf.TimeAsync "IsSymbolUsedInProjects :: ParseAndCheckProject" <| fun _ ->
                     checkerAsync (fun x -> x.ParseAndCheckProject opts)
                let! refs = pf.TimeAsync "IsSymbolUsedInProjects :: GetUsesOfSymbol" <| fun _ ->
                     projectResults.GetUsesOfSymbol symbol
                return
                    if opts.ProjectFileName = currentProjectName then
                        refs.Length > 1
                    else refs.Length > 0 })
    

    member x.GetUnusedDeclarations (symbolsUses, projectOptions, getSymbolDeclProjects, pf: Profiler) : SymbolUse[] Async =
        async {
            let singleDefs = UnusedDeclarations.getSingleDeclarations symbolsUses
            let! notUsedSymbols =
                singleDefs 
                |> Async.Array.map (fun fsSymbol ->
                    async {
                        let! opts = getSymbolDeclProjects fsSymbol
                        match opts with
                        | Some projects ->
                            let! isSymbolUsed = x.IsSymbolUsedInProjects (fsSymbol, projectOptions.ProjectFileName, projects, pf) 
                            if isSymbolUsed then return None
                            else return Some fsSymbol
                        | None -> return None 
                    })
                |> Async.map (Array.choose id)

            return pf.Time "LS return" <| fun _ ->
                match notUsedSymbols with
                | [||] -> symbolsUses
                | _ ->
                    symbolsUses
                    |> Array.map (fun su -> 
                        { su with IsUsed = notUsedSymbols |> Array.forall (fun s -> not (s.IsEffectivelySameAs su.SymbolUse.Symbol)) })
        }

    member x.GetAllEntitiesInProjectAndReferencedAssemblies (projectOptions: FSharpProjectOptions, fileName, source, ?withCache) =
        async {
            let! checkResults = x.ParseAndCheckFileInProject (projectOptions, fileName, source, AllowStaleResults.No)
            return 
                Some [ match checkResults.GetPartialAssemblySignature() with
                       | Some signature -> 
                           yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full signature
                       | None -> ()

                       match checkResults.ProjectContext with
                       | Some ctx ->
                           // FCS sometimes returns several FSharpAssembly for single referenced assembly. 
                           // For example, it returns two different ones for Swensen.Unquote; the first one 
                           // contains no useful entities, the second one does. Our cache prevents to process
                           // the second FSharpAssembly which results with the entities containing in it to be 
                           // not discovered.
                           let assembliesByFileName =
                               ctx.GetReferencedAssemblies()
                               |> Seq.groupBy (fun asm -> asm.FileName)
                               |> Seq.map (fun (fileName, asms) -> fileName, List.ofSeq asms)
                               |> Seq.toList
                               |> List.rev // if mscorlib.dll is the first then FSC raises exception when we try to
                                           // get Content.Entities from it.

                           for fileName, signatures in assembliesByFileName do
                               let contentType = Public // it's always Public for now since we don't support InternalsVisibleTo attribute yet
                               yield! AssemblyContentProvider.getAssemblyContent withCache contentType fileName signatures
                       | None -> () ]
        }

    member x.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, project: FSharpProjectOptions, file, source) =
        async {
            let! checkResults = x.ParseAndCheckFileInProject (project, file, source, AllowStaleResults.No)
            return! checkResults.GetIdentTooltip (line, colAtEndOfNames, lineStr, names)
        }

    member __.SetCriticalErrorHandler func = 
        errorHandler <- Some func