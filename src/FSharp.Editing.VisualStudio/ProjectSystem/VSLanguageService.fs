namespace FSharp.Editing.VisualStudio.ProjectSystem

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
open FSharpVSPowerTools.AsyncMaybe
open FSharpPowerTools.Core.Symbols
open FSharpPowerTools.Core
open FSharpPowerTools.Core.Infrastructure

type FilePath = string

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation = 
    | File
    /// The case where the declared symbol may be included into several projects
    | Projects of IProjectProvider list * isLocalForProject: bool

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
     openDocumentsTracker: IOpenDocumentsTracker,
     [<Import(typeof<FileSystem>)>] fileSystem: IFileSystem,
     [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =

    let globalOptions = Setting.getGlobalOptions serviceProvider
    let instance = LanguageService (globalOptions.BackgroundCompilation, globalOptions.ProjectCacheSize, fileSystem)

    /// Log exceptions to 'ActivityLog' if users run 'devenv.exe /Log'.
    /// Clean up instructions are displayed on status bar.
    let suggestRecoveryAfterFailure ex fileName _source opts =
        Logging.logError (fun _ -> sprintf "The following exception: %A occurs for file '%O' and options '%A'." ex fileName opts)
        let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
        statusBar.SetText Resource.languageServiceErrorMessage |> ignore 
                
    do instance.SetCriticalErrorHandler suggestRecoveryAfterFailure
       openDocumentsTracker.DocumentChanged.Add instance.OnFileChanged
       openDocumentsTracker.DocumentClosed.Add instance.OnFileClosed

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

    let mayReferToSameBuffer (snapshot: ITextSnapshot) filePath =
        match openDocumentsTracker.TryFindOpenDocument(filePath) with
        | None -> true
        | Some doc ->
            doc.Snapshot.TextBuffer = snapshot.TextBuffer
            
    let getSymbolUsing kind (point: SnapshotPoint) fileName (projectProvider: IProjectProvider) =
        maybe {
            let! source = openDocumentsTracker.TryGetDocumentText fileName
            let line = point.Snapshot.GetLineNumberFromPosition point.Position
            let col = point.Position - point.GetContainingLine().Start.Position
            let lineStr = point.GetContainingLine().GetText()
            let args = projectProvider.CompilerOptions

            let queryLexState = buildQueryLexState point.Snapshot.TextBuffer

            let! symbol = Lexer.getSymbol source line col lineStr kind args queryLexState
            return SnapshotSpan.MakeFromRange point.Snapshot symbol.Range, symbol
        }

    let entityCache = EntityCache()

    member __.GetQueryLexState (textBuffer) : QueryLexState = buildQueryLexState textBuffer

    member __.GetSymbol(point, fileName, projectProvider) =
        getSymbolUsing SymbolLookupKind.Fuzzy point fileName projectProvider

    member __.GetSymbol(point: PointInDocument<FCS>, projectProvider: IProjectProvider, queryLexState) =
        Lexer.getSymbolAtPoint point SymbolLookupKind.Fuzzy (projectProvider.CompilerOptions) queryLexState

    member __.GetLongIdentSymbol(point, fileName, projectProvider) =
        getSymbolUsing SymbolLookupKind.ByLongIdent point fileName projectProvider

    member __.TokenizeLine(fileName: string, textBuffer: ITextBuffer, args: string[], line) =
        maybe {
            let! source = openDocumentsTracker.TryGetDocumentText fileName
            let lineStr = textBuffer.CurrentSnapshot.GetLineFromLineNumber(line).GetText()
            return Lexer.tokenizeLine source args line lineStr (buildQueryLexState textBuffer)
        }

    member __.ParseFileInProject (fileName, projectProvider: IProjectProvider) =
        asyncMaybe {
            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
            let! source = openDocumentsTracker.TryGetDocumentText fileName
            return! instance.ParseFileInProject(opts, fileName, source) |> liftAsync
        }

    member __.ParseFileInProject (fileName, source, projectProvider: IProjectProvider) =
        async {
            let! opts = projectProvider.GetProjectCheckerOptions instance
            return! instance.ParseFileInProject(opts, fileName, source)
        }

    member __.ParseAndCheckFileInProject (currentFile: string, projectProvider: IProjectProvider, ?allowStaleResults) =
        let allowStaleResults = defaultArg allowStaleResults AllowStaleResults.No
        asyncMaybe {
            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
            let! source = openDocumentsTracker.TryGetDocumentText currentFile
            return! instance.ParseAndCheckFileInProject(opts, currentFile, source, allowStaleResults) |> liftAsync
        }

    member __.FindUsages (word: SnapshotSpan, currentFile: string, currentProject: IProjectProvider, projectsToCheck: IProjectProvider list, ?progress: ShowProgress) =
        asyncMaybe {
            Debug.Assert(mayReferToSameBuffer word.Snapshot currentFile, 
                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." word.Snapshot currentFile)
            try
                let range = word.ToRange()
                let endLine = range.End.Line
                let endCol = range.End.Column
                let! source = openDocumentsTracker.TryGetDocumentText currentFile
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = currentProject.TargetFramework
                let args = currentProject.CompilerOptions
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                reportProgress progress (Reporting Resource.findSymbolUseCurrentProject)
                let! currentProjectOptions = currentProject.GetProjectCheckerOptions instance |> liftAsync
                reportProgress progress (Reporting Resource.findSymbolUseOtherProjects)
                let! projectsToCheckOptions = 
                    projectsToCheck 
                    |> List.toArray
                    |> Async.Array.map (fun p -> p.GetProjectCheckerOptions instance)
                    |> liftAsync

                reportProgress progress (Reporting Resource.findSymbolUseAllProjects)

                let newReportProgress projectName index length = 
                    reportProgress progress (Executing(sprintf "Finding usages in %s [%d of %d]..." projectName (index + 1) length, index, length))
                
                let! symbol, lastIdent, refs =
                    instance.GetUsesOfSymbolInProjectAtLocationInFile
                        (currentProjectOptions, projectsToCheckOptions, currentFile, source, endLine, endCol, 
                         currentLine, args, buildQueryLexState word.Snapshot.TextBuffer, Some newReportProgress)
                return symbol, lastIdent, filterSymbolUsesDuplicates refs
            with e ->
                debug "[Language Service] %O exception occurs while finding usages." e
                Logging.logExceptionWithContext(e, "Exception occurs while finding usages.")
                return! None }

    member __.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, fileScopedCheckResults: ParseAndCheckResults) =
        async {
            try 
                let range = word.ToRange()
                let endLine = range.End.Line
                let currentLine = word.Start.GetContainingLine().GetText()
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d" (word.GetText()) endLine sym.RightColumn
                let! res = fileScopedCheckResults.GetUsesOfSymbolInFileAtLocation (endLine, sym.RightColumn, currentLine, sym.Text)
                return res |> Option.map (fun (symbol, ident, refs) -> symbol, ident, filterSymbolUsesDuplicates refs)
            with e ->
                debug "[Language Service] %O exception occurs while finding usages in file." e
                Logging.logExceptionWithContext(e, "Exception occurs while finding usages in file.")
                return None
        }

    member __.FindUsagesInFile (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) =
        asyncMaybe {
            let currentLine = { Line = word.Start.GetContainingLine().GetText(); Range = word.ToRange(); File = currentFile }
            let getCheckResults currentFile = 
                asyncMaybe {
                    let! source = openDocumentsTracker.TryGetDocumentText currentFile
                    let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
                    let! results = instance.ParseAndCheckFileInProject(opts, currentFile, source, stale) |> liftAsync
                    return results
                }
            return! HighlightUsageInFile.findUsageInFile currentLine symbol getCheckResults
        }

    member __.GetFSharpSymbolUse (currentLine: CurrentLine<FCS>, symbol: Symbol, projectProvider: IProjectProvider, stale) = 
        asyncMaybe {
            let! source = openDocumentsTracker.TryGetDocumentText currentLine.File
            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
            let! results = instance.ParseAndCheckFileInProject(opts, currentLine.File, source, stale) |> liftAsync
            let! symbol = results.GetSymbolUseAtLocation (currentLine.EndLine + 1, symbol.RightColumn, currentLine.Line, [symbol.Text])
            return symbol, results
        }
    member __.GetFSharpSymbolUse (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) = 
        asyncMaybe {
            Debug.Assert(mayReferToSameBuffer word.Snapshot currentFile, 
                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." word.Snapshot currentFile)
            let range = word.ToRange()
            let endLine = range.End.Line
            let! source = openDocumentsTracker.TryGetDocumentText currentFile
            let currentLine = word.Start.GetContainingLine().GetText()
            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
            let! results = instance.ParseAndCheckFileInProject(opts, currentFile, source, stale) |> liftAsync
            let! symbol = results.GetSymbolUseAtLocation (endLine+1, symbol.RightColumn, currentLine, [symbol.Text])
            return symbol, results
        }

    member __.CreateLexer (fileName, snapshot, args) =
        maybe {
            let range = SnapshotSpan(snapshot, 0, snapshot.Length).ToRange()
            let lineStart = range.Start.Line
            let lineEnd = range.End.Line
            
            let getLineStr line =
                let lineNumber = line - lineStart
                snapshot.GetLineFromLineNumber(lineNumber).GetText() 
            
            let! source = openDocumentsTracker.TryGetDocumentText fileName
            
            return 
                { new LexerBase() with
                    member __.GetSymbolFromTokensAtLocation (tokens, line, rightCol) =
                        Lexer.getSymbolFromTokens tokens line rightCol (getLineStr line) SymbolLookupKind.ByRightColumn
                    member __.TokenizeLine line =
                        Lexer.tokenizeLine source args line (getLineStr line) (buildQueryLexState snapshot.TextBuffer)
                    member __.LineCount = lineEnd + 1 }
        }

    member x.GetAllUsesOfAllSymbolsInFile (snapshot: ITextSnapshot, currentFile: string, project: IProjectProvider, stale, checkForUnusedOpens: bool) = 
        asyncMaybe {
            Debug.Assert(mayReferToSameBuffer snapshot currentFile, 
                sprintf "Snapshot '%A' doesn't refer to the current document '%s'." snapshot currentFile)
            let! source = openDocumentsTracker.TryGetDocumentText currentFile
            return! x.GetAllUsesOfAllSymbolsInSourceString(source, currentFile, project, stale, checkForUnusedOpens) |> liftAsync
        }

    member __.GetAllUsesOfAllSymbolsInSourceString (source: string, currentFile: string, project: IProjectProvider, stale, checkForUnusedOpens: bool) = 
        async {
            let! opts = project.GetProjectCheckerOptions instance
            let! allSymbolsUses = instance.GetAllUsesOfAllSymbolsInFile(opts, currentFile, source, stale, checkForUnusedOpens)
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

    member x.GetUnusedDeclarations (symbolUses, currentProject: IProjectProvider, getSymbolDeclLocation) =
        async {
            let! opts = currentProject.GetProjectCheckerOptions instance
            return! instance.GetUnusedDeclarations(symbolUses, opts, x.GetSymbolDeclProjects getSymbolDeclLocation currentProject)
        }

    member __.GetAllEntities (fileName, project: IProjectProvider) =
        asyncMaybe { 
            let! opts = project.GetProjectCheckerOptions instance |> liftAsync
            let! source = openDocumentsTracker.TryGetDocumentText fileName
            try 
                return! instance.GetAllEntitiesInProjectAndReferencedAssemblies (opts, fileName, source, entityCache.Locking)
            with e ->
                debug "[Language Service] GetAllSymbols raises an exception: %O" e
                Logging.logExceptionWithContext(e, "GetAllSymbols raises an exception.")
                return! None
        }

    member x.GetLoadDirectiveFileNameAtCursor (fileName, view: Microsoft.VisualStudio.Text.Editor.ITextView, project) =
        asyncMaybe {
            let! parseResult = x.ParseFileInProject(fileName, project)
            let! pos = view.PosAtCaretPosition()
            let! ast = parseResult.ParseTree
            return! UntypedAstUtils.HashDirectiveInfo.getHashLoadDirectiveResolvedPathAtPosition pos ast
        }

    member __.GetOpenDeclarationTooltip (line, colAtEndOfNames, lineStr, names, project: IProjectProvider, file) =
        asyncMaybe {
            let! source = openDocumentsTracker.TryGetDocumentText file
            let! opts = project.GetProjectCheckerOptions instance |> liftAsync
            try return! instance.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, opts, file, source)
            with _ -> return! None
        }

    member __.InvalidateProject (projectProvider: IProjectProvider) = 
        async {
            let! opts = projectProvider.GetProjectCheckerOptions(instance) 
            return! instance.InvalidateConfiguration opts
        }

    member __.ClearCaches() = 
        debug "[Language Service] Clearing FCS caches."
        instance.RawChecker.InvalidateAll()
        entityCache.Clear()
    
    member __.CheckProjectInBackground (opts: FSharpProjectOptions) =
        debug "[LanguageService] StartBackgroundCompile (%s)" opts.ProjectFileName
        instance.RawChecker.CheckProjectInBackground opts

    member __.RawChecker = instance.RawChecker

    /// This value is used for testing when VS lex cache isn't available
    member internal __.SkipLexCache 
        with get () = skipLexCache
        and set v = skipLexCache <- v

    member __.GetCompleteTextForDocument filename =
        openDocumentsTracker.TryGetDocumentText filename
