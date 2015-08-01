namespace FSharpVSPowerTools.ProjectSystem

open FSharpVSPowerTools
open FSharp.ViewModule.Progress
open Microsoft.VisualStudio.Editor
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open FSharpVSPowerTools.AssemblyContentProvider

type FilePath = string

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation = 
    | File
    /// The case where the declared symbol may be included into several projects
    | Projects of IProjectProvider list * isLocalForProject: bool

and IProjectProvider =
    abstract IsForStandaloneScript: bool
    abstract ProjectFileName: string
    abstract TargetFramework: FSharpTargetFramework
    abstract CompilerVersion: FSharpCompilerVersion option
    abstract CompilerOptions: string []
    abstract SourceFiles: string []
    abstract FullOutputFilePath: string option
    abstract GetReferencedProjects: unit -> IProjectProvider list
    abstract GetAllReferencedProjectFileNames: unit -> string list 
    abstract GetProjectCheckerOptions: LanguageService -> Async<FSharpProjectOptions>

type ShowProgress = OperationState -> unit

type EntityCache() =
    let dic = Dictionary<AssemblyPath, AssemblyContentCacheEntry>()
    interface IAssemblyContentCache with
        member __.TryGet assembly =
            match dic.TryGetValue assembly with
            | true, entry -> Some entry
            | _ -> None
        member __.Set assembly entry = dic.[assembly] <- entry

    member __.Clear() = dic.Clear()
    member x.Locking f = lock dic <| fun _ -> f (x :> IAssemblyContentCache)

[<Export>]
type VSLanguageService
    [<ImportingConstructor>] 
    (editorFactory: IVsEditorAdaptersFactoryService, 
     fsharpLanguageService: FSharpLanguageService,
     openDocumentsTracker: OpenDocumentsTracker,
     [<Import(typeof<FileSystem>)>] fileSystem: IFileSystem,
     [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =

    let instance = LanguageService (fileSystem)

    /// Log exceptions to 'ActivityLog' if users run 'devenv.exe /Log'.
    /// Clean up instructions are displayed on status bar.
    let suggestRecoveryAfterFailure ex fileName _source opts =
        Logging.logError "The following exception: %A occurs for file '%O' and options '%A'." ex fileName opts
        let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
        statusBar.SetText(Resource.languageServiceErrorMessage) |> ignore 
                
    do instance.SetCriticalErrorHandler(suggestRecoveryAfterFailure)

    let mutable skipLexCache = false

    let buildQueryLexState (textBuffer: ITextBuffer) source defines line =
        try
            if skipLexCache then
                Lexer.queryLexState source defines line
            else
                let vsColorState = editorFactory.GetBufferAdapter(textBuffer) :?> IVsTextColorState
                let colorState = fsharpLanguageService.GetColorStateAtStartOfLine(vsColorState, line)
                fsharpLanguageService.LexStateOfColorState(colorState)
        with e ->
            debug "[Language Service] %O exception occurs while querying lexing states." e
            Logging.logExceptionWithContext(e, "Exception occurs while querying lexing states.")
            Lexer.queryLexState source defines line

    let filterSymbolUsesDuplicates (uses: FSharpSymbolUse[]) =
        uses
        |> Seq.map (fun symbolUse -> (symbolUse.FileName, symbolUse))
        |> Seq.groupBy (fst >> Path.GetFullPathSafe)
        |> Seq.map (fun (_, symbolUses) -> 
            symbolUses 
            |> Seq.map snd 
            |> Seq.distinctBy (fun s -> s.RangeAlternate))
        |> Seq.concat
        |> Seq.toArray

    let mayReferToSameBuffer (snapshot: ITextSnapshot) filePath =
        match openDocumentsTracker.TryFindOpenDocument(filePath) with
        | None -> true
        | Some doc ->
            doc.Snapshot.TextBuffer = snapshot.TextBuffer

    static let initializationTime = DateTime.Now
    let entityCache = EntityCache()

    /// If we are on the strict mode, set project load time to that of the last recently-changed open document.
    /// When there is no open document, use initialization time as project load time.
    let fixProjectLoadTime =
        // Make sure that the options are retrieved after options dialogs have been initialized.
        // Otherwise, the calls will throw exceptions.
        let globalOptions = lazy (Setting.getGlobalOptions(serviceProvider))
        fun opts ->
            if globalOptions.Value.StrictMode then
                let projectFiles = Set.ofArray opts.ProjectFileNames
                let openDocumentsChangeTimes = 
                    openDocumentsTracker.MapOpenDocuments (fun (KeyValue (file, doc)) -> file, doc)
                    |> Seq.choose (fun (file, doc) -> 
                        if doc.Document.IsDirty && Set.contains file projectFiles then 
                            Some doc.LastChangeTime 
                        else None)
                    |> Seq.toList
        
                match openDocumentsChangeTimes with
                | [] -> 
                    { opts with LoadTime = initializationTime }
                | changeTimes -> 
                    { opts with LoadTime = List.max changeTimes }  
            else
                opts

    member __.FixProjectLoadTime opts = fixProjectLoadTime opts
        
    member __.GetSymbol(point: SnapshotPoint, projectProvider: IProjectProvider) =
        let source = point.Snapshot.GetText()
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let col = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()                
        let args = projectProvider.CompilerOptions
        let snapshotSpanFromRange (snapshot: ITextSnapshot) (lineStart, colStart, lineEnd, colEnd) =
            let startPos = snapshot.GetLineFromLineNumber(lineStart).Start.Position + colStart
            let endPos = snapshot.GetLineFromLineNumber(lineEnd).Start.Position + colEnd
            SnapshotSpan(snapshot, startPos, endPos - startPos)
                                
        Lexer.getSymbol source line col lineStr SymbolLookupKind.Fuzzy args (buildQueryLexState point.Snapshot.TextBuffer)
        |> Option.map (fun symbol -> snapshotSpanFromRange point.Snapshot symbol.Range, symbol)

    member __.TokenizeLine(textBuffer: ITextBuffer, args: string[], line) =
        let snapshot = textBuffer.CurrentSnapshot
        let source = snapshot.GetText()
        let lineStr = snapshot.GetLineFromLineNumber(line).GetText()
        Lexer.tokenizeLine source args line lineStr (buildQueryLexState textBuffer)

    member __.ParseFileInProject (currentFile: string, source, projectProvider: IProjectProvider) =
        async {
            let! opts = projectProvider.GetProjectCheckerOptions instance
            return! instance.ParseFileInProject(opts, currentFile, source) 
        }

    member __.ParseAndCheckFileInProject (currentFile: string, source, projectProvider: IProjectProvider) =
        async {
            let! opts = projectProvider.GetProjectCheckerOptions instance
            return! instance.ParseAndCheckFileInProject(opts, currentFile, source, AllowStaleResults.No) 
        }

    member __.ProcessNavigableItemsInProject(openDocuments, projectProvider: IProjectProvider, processNavigableItems, ct) =
        instance.ProcessParseTrees(
            projectProvider.ProjectFileName, 
            openDocuments, 
            projectProvider.SourceFiles, 
            projectProvider.CompilerOptions, 
            projectProvider.CompilerVersion |> Option.getOrElse FSharpCompilerVersion.FSharp_3_1, 
            (Navigation.NavigableItemsCollector.collect >> processNavigableItems), 
            ct)        

    member __.FindUsages (word: SnapshotSpan, currentFile: string, currentProject: IProjectProvider, projectsToCheck: IProjectProvider list, ?progress: ShowProgress) =
        async {
            Debug.Assert(mayReferToSameBuffer word.Snapshot currentFile, 
                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." word.Snapshot currentFile)
            try                 
                let (_, _, endLine, endCol) = word.ToRange()
                let source = word.Snapshot.GetText()
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = currentProject.TargetFramework
                let args = currentProject.CompilerOptions
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                reportProgress progress (Reporting Resource.findSymbolUseCurrentProject)
                let! currentProjectOptions = currentProject.GetProjectCheckerOptions instance
                reportProgress progress (Reporting Resource.findSymbolUseOtherProjects)
                let! projectsToCheckOptions = 
                    projectsToCheck 
                    |> List.toArray
                    |> Async.Array.map (fun p -> p.GetProjectCheckerOptions instance)

                reportProgress progress (Reporting Resource.findSymbolUseAllProjects)

                let newReportProgress projectName index length = 
                    reportProgress progress (Executing(sprintf "Finding usages in %s [%d of %d]..." projectName (index + 1) length, index, length))
                
                let! res =
                    instance.GetUsesOfSymbolInProjectAtLocationInFile
                        (currentProjectOptions, projectsToCheckOptions, currentFile, source, endLine, endCol, 
                         currentLine, args, buildQueryLexState word.Snapshot.TextBuffer, Some newReportProgress)
                return 
                    res 
                    |> Option.map (fun (symbol, lastIdent, refs) -> 
                        symbol, lastIdent, filterSymbolUsesDuplicates refs)
            with e ->
                debug "[Language Service] %O exception occurs while finding usages." e
                Logging.logExceptionWithContext(e, "Exception occurs while finding usages.")
                return None }

    member __.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, fileScopedCheckResults: ParseAndCheckResults) =
        async {
            try 
                let (_, _, endLine, _) = word.ToRange()
                let currentLine = word.Start.GetContainingLine().GetText()
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d" (word.GetText()) endLine sym.RightColumn
                let! res = fileScopedCheckResults.GetUsesOfSymbolInFileAtLocation (endLine, sym.RightColumn, currentLine, sym.Text)
                return res |> Option.map (fun (symbol, ident, refs) -> symbol, ident, filterSymbolUsesDuplicates refs)
            with e ->
                debug "[Language Service] %O exception occurs while finding usages in file." e
                Logging.logExceptionWithContext(e, "Exception occurs while finding usages in file.")
                return None
        }

    member __.GetFSharpSymbolUse (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) = 
        async {
            Debug.Assert(mayReferToSameBuffer word.Snapshot currentFile, 
                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." word.Snapshot currentFile)
            let (_, _, endLine, _) = word.ToRange()
            let source = word.Snapshot.GetText()
            let currentLine = word.Start.GetContainingLine().GetText()
            let! opts = projectProvider.GetProjectCheckerOptions instance
            let! results = instance.ParseAndCheckFileInProject(opts, currentFile, source, stale)
            let! symbol = results.GetSymbolUseAtLocation (endLine+1, symbol.RightColumn, currentLine, [symbol.Text])
            return symbol |> Option.map (fun s -> s, results)
        }

    member __.CreateLexer (snapshot, args) =
        let lineStart, _, lineEnd, _ = SnapshotSpan(snapshot, 0, snapshot.Length).ToRange()
        
        let getLineStr line =
            let lineNumber = line - lineStart
            snapshot.GetLineFromLineNumber(lineNumber).GetText() 
        
        let source = snapshot.GetText()
        
        { new LexerBase() with
            member __.GetSymbolFromTokensAtLocation (tokens, line, rightCol) =
                Lexer.getSymbolFromTokens tokens line rightCol (getLineStr line) SymbolLookupKind.ByRightColumn
            member __.TokenizeLine line =
                Lexer.tokenizeLine source args line (getLineStr line) (buildQueryLexState snapshot.TextBuffer)
            member __.LineCount = lineEnd + 1 }

    member x.GetAllUsesOfAllSymbolsInFile (snapshot: ITextSnapshot, currentFile: string, project: IProjectProvider, stale,
                                            checkForUnusedOpens: bool, profiler: Profiler) = 
        async {
            Debug.Assert(mayReferToSameBuffer snapshot currentFile, 
                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." snapshot currentFile)
            let source = snapshot.GetText() 
            return! x.GetAllUsesOfAllSymbolsInSourceString(source, currentFile, project, stale, checkForUnusedOpens, profiler)
        }

    member __.GetAllUsesOfAllSymbolsInSourceString (source: string, currentFile: string, project: IProjectProvider, stale,
                                                    checkForUnusedOpens: bool, profiler: Profiler) = 
        async {
            let! opts = project.GetProjectCheckerOptions instance
            let! allSymbolsUses = profiler.TimeAsync "instance.GetAllUsesOfAllSymbolsInFile" <| fun _ ->
                instance.GetAllUsesOfAllSymbolsInFile(opts, currentFile, source, stale, checkForUnusedOpens, profiler)
            return allSymbolsUses
        }

     member __.GetSymbolDeclProjects getSymbolDeclLocation currentProject (symbol: FSharpSymbol) =
         async {
             let projects =
                 match getSymbolDeclLocation symbol with
                 | Some SymbolDeclarationLocation.File -> Some [currentProject]
                 | Some (SymbolDeclarationLocation.Projects (declProjects, _)) -> Some declProjects
                 | None -> None
         
             match projects with
             | Some projects ->
                 return! 
                     projects
                     |> List.toArray
                     |> Async.Array.map (fun p -> p.GetProjectCheckerOptions instance)
                     |> Async.map Some
             | None -> return None
         }

     member __.GetProjectCheckerOptions (project: IProjectProvider) = project.GetProjectCheckerOptions instance

     member x.GetUnusedDeclarations (symbolUses, currentProject: IProjectProvider, getSymbolDeclLocation, pf: Profiler) = 
        async {
            let! opts = currentProject.GetProjectCheckerOptions instance
            return! instance.GetUnusedDeclarations(symbolUses, opts, x.GetSymbolDeclProjects getSymbolDeclLocation currentProject, pf)
        }

     member __.GetAllEntities (fileName, source, project: IProjectProvider) =
        async { 
            let! opts = project.GetProjectCheckerOptions instance
            try 
                return! instance.GetAllEntitiesInProjectAndReferencedAssemblies (opts, fileName, source, entityCache.Locking)
            with e ->
                debug "[Language Service] GetAllSymbols raises an exception: %O" e
                Logging.logExceptionWithContext(e, "GetAllSymbols raises an exception.")
                return None
        }

    member __.GetOpenDeclarationTooltip (line, colAtEndOfNames, lineStr, names, project: IProjectProvider, file, source) =
        async {
            let! opts = project.GetProjectCheckerOptions instance
            try return! instance.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, opts, file, source)
            with _ -> return None
        }

    member __.InvalidateProject (projectProvider: IProjectProvider) = 
        async {
            let! opts = projectProvider.GetProjectCheckerOptions(instance) 
            return! instance.InvalidateConfiguration opts
        }

    member __.ClearCaches() = 
        debug "[Language Service] Clearing FCS caches."
        instance.RawChecker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()
        entityCache.Clear()
    
    member __.StartBackgroundCompile (opts: FSharpProjectOptions) =
        debug "[LanguageService] StartBackgroundCompile (%s)" opts.ProjectFileName
        instance.RawChecker.StartBackgroundCompile opts

    member __.CheckerAsync x = instance.CheckerAsync x
    member __.RawChecker = instance.RawChecker

    /// This value is used for testing when VS lex cache isn't available
    member internal __.SkipLexCache 
        with get () = skipLexCache
        and set v = skipLexCache <- v