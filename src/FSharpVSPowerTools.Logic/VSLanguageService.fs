namespace FSharpVSPowerTools.ProjectSystem

open System
open System.IO
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Editor
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.TextManager.Interop
open FSharpVSPowerTools

[<Export>]
type VSLanguageService
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider, 
     editorFactory: IVsEditorAdaptersFactoryService, 
     fsharpLanguageService: FSharpLanguageService,
     openDocumentsTracker: OpenDocumentsTracker,
     [<Import(typeof<ProjectFactory>)>] projectFactory: ProjectFactory) =

    let dte = serviceProvider.GetService<EnvDTE.DTE, Interop.SDTE>()
    let mutable instance = LanguageService (ignore, FileSystem openDocumentsTracker)
    
    let getProjectOptions (project: IProjectProvider) =
        async {
            let! opts = project.GetProjectCheckerOptions(instance)
            let projectFiles = Set.ofArray project.SourceFiles 
            let openDocumentsChangeTimes = 
                    openDocumentsTracker.MapOpenDocuments (fun (KeyValue (file, doc)) -> file, doc)
                    |> Seq.choose (fun (file, doc) -> 
                        if doc.Document.IsDirty && projectFiles |> Set.contains file then Some doc.LastChangeTime else None)
                    |> Seq.toList
        
            return 
                match openDocumentsChangeTimes with
                | [] -> opts
                | changeTimes -> { opts with LoadTime = List.max (opts.LoadTime::changeTimes) }
        }

    let invalidateProject (projectItem: EnvDTE.ProjectItem) =
        async {
            let project = projectItem.ContainingProject
            if box project <> null && isFSharpProject project then
                let p = projectFactory.CreateForProject project
                debug "[Language Service] InteractiveChecker.InvalidateConfiguration for %s" p.ProjectFileName
                let! opts = p.GetProjectCheckerOptions instance
                return instance.InvalidateConfiguration(opts)
        }
        |> Async.StartImmediate

    let events = dte.Events :?> EnvDTE80.Events2
    let projectItemsEvents = events.ProjectItemsEvents
    do projectItemsEvents.add_ItemAdded(fun p -> invalidateProject p)
    do projectItemsEvents.add_ItemRemoved(fun p -> invalidateProject p)
    do projectItemsEvents.add_ItemRenamed(fun p _ -> invalidateProject p)
    do events.SolutionEvents.add_AfterClosing (fun _ -> 
        //instance.Checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()
        instance <- LanguageService (ignore, FileSystem openDocumentsTracker))

    let buildQueryLexState (textBuffer: ITextBuffer) source defines line =
        try
            let vsColorState = editorFactory.GetBufferAdapter(textBuffer) :?> IVsTextColorState
            let colorState = fsharpLanguageService.GetColorStateAtStartOfLine(vsColorState, line)
            fsharpLanguageService.LexStateOfColorState(colorState)
        with e ->
            debug "[Language Service] %O exception occurs while querying lexing states." e
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
        
    member x.GetSymbol(point: SnapshotPoint, projectProvider: IProjectProvider) =
        let source = point.Snapshot.GetText()
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let col = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()                
        let args = projectProvider.CompilerOptions
        let snapshotSpanFromRange (snapshot: ITextSnapshot) (lineStart, colStart, lineEnd, colEnd) =
            let startPos = snapshot.GetLineFromLineNumber(lineStart).Start.Position + colStart
            let endPos = snapshot.GetLineFromLineNumber(lineEnd).Start.Position + colEnd
            SnapshotSpan(snapshot, startPos, endPos - startPos)
                                
        Lexer.getSymbol source line col lineStr args (buildQueryLexState point.Snapshot.TextBuffer)
        |> Option.map (fun symbol -> snapshotSpanFromRange point.Snapshot symbol.Range, symbol)

    member x.TokenizeLine(textBuffer: ITextBuffer, args: string[], line) =
        let snapshot = textBuffer.CurrentSnapshot
        let source = snapshot.GetText()
        let lineStr = snapshot.GetLineFromLineNumber(line).GetText()
        Lexer.tokenizeLine source args line lineStr (buildQueryLexState textBuffer)

    member x.ParseFileInProject (currentFile: string, source, projectProvider: IProjectProvider) =
        async {
            let! opts = projectProvider.GetProjectCheckerOptions instance
            return! instance.ParseFileInProject(opts, currentFile, source) 
        }

    member x.ProcessNavigableItemsInProject(openDocuments, projectProvider: IProjectProvider, processNavigableItems, ct) =
        instance.ProcessParseTrees(
            projectProvider.ProjectFileName, 
            openDocuments, 
            projectProvider.SourceFiles, 
            projectProvider.CompilerOptions, 
            projectProvider.TargetFramework, 
            (Navigation.NavigableItemsCollector.collect >> processNavigableItems), 
            ct)        

    member x.FindUsages (word: SnapshotSpan, currentFile: string, currentProject: IProjectProvider, projectsToCheck: IProjectProvider list) =
        async {
            try 
                let (_, _, endLine, endCol) = word.ToRange()
                let source = word.Snapshot.GetText()
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = currentProject.TargetFramework
                let args = currentProject.CompilerOptions
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                let! currentProjectOptions = getProjectOptions currentProject
                let! projectsToCheckOptions = 
                    projectsToCheck 
                    |> List.toArray
                    |> Async.Array.map getProjectOptions

                let! res =
                    instance.GetUsesOfSymbolInProjectAtLocationInFile
                        (currentProjectOptions, projectsToCheckOptions, currentFile, source, endLine, endCol, 
                         currentLine, args, buildQueryLexState word.Snapshot.TextBuffer)
                return 
                    res 
                    |> Option.map (fun (symbol, lastIdent, refs) -> 
                        symbol, lastIdent, filterSymbolUsesDuplicates refs)
            with e ->
                debug "[Language Service] %O exception occurs while updating." e
                return None }

    member x.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) =
        async {
            try 
                let (_, _, endLine, endCol) = word.ToRange()
                let framework = projectProvider.TargetFramework
                let args = projectProvider.CompilerOptions
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                let! res = x.GetFSharpSymbolUse (word, sym, currentFile, projectProvider, stale)
                return 
                    res 
                    |> Option.map (fun (_, checkResults) -> 
                        x.FindUsagesInFile (word, sym, checkResults)
                        |> Async.map (Option.map (fun (symbol, ident, refs) -> symbol, ident, filterSymbolUsesDuplicates refs)))
            with e ->
                debug "[Language Service] %O exception occurs while updating." e
                return None }

    member x.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, fileScopedCheckResults: ParseAndCheckResults) =
        async {
            try 
                let (_, _, endLine, _) = word.ToRange()
                let currentLine = word.Start.GetContainingLine().GetText()
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d" (word.GetText()) endLine sym.RightColumn
                let! res = fileScopedCheckResults.GetUsesOfSymbolInFileAtLocation (endLine, sym.RightColumn, currentLine, sym.Text)
                return res |> Option.map (fun (symbol, ident, refs) -> symbol, ident, filterSymbolUsesDuplicates refs)
            with e ->
                debug "[Language Service] %O exception occurs while finding usages in file." e
                return None
        }

    member x.GetFSharpSymbolUse (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) = 
        async {
            let (_, _, endLine, _) = word.ToRange()
            let source = word.Snapshot.GetText()
            let currentLine = word.Start.GetContainingLine().GetText()
            let! opts = projectProvider.GetProjectCheckerOptions instance
            let! results = instance.ParseAndCheckFileInProject(opts, currentFile, source, stale)
            let! symbol = results.GetSymbolUseAtLocation (endLine+1, symbol.RightColumn, currentLine, [symbol.Text])
            return symbol |> Option.map (fun s -> s, results)
        }

    member x.GetAllUsesOfAllSymbolsInFile (snapshot: ITextSnapshot, currentFile: string, projectProvider: IProjectProvider, stale) = 
        async {
            let source = snapshot.GetText()
            let args = projectProvider.CompilerOptions
            let lexer = 
                let getLineStr line =
                    let lineStart,_,_,_ = SnapshotSpan(snapshot, 0, snapshot.Length).ToRange()
                    let lineNumber = line - lineStart
                    snapshot.GetLineFromLineNumber(lineNumber).GetText() 

                { new ILexer with
                    member x.GetSymbolAtLocation line col =
                        Lexer.getSymbol source line col (getLineStr line) args (buildQueryLexState snapshot.TextBuffer) 
                    member x.TokenizeLine line =
                        Lexer.tokenizeLine source args line (getLineStr line) (buildQueryLexState snapshot.TextBuffer) }

            let! opts = projectProvider.GetProjectCheckerOptions instance
            let! symbolUses = instance.GetAllUsesOfAllSymbolsInFile(opts, currentFile, source, stale)
            return symbolUses, lexer
        }

    member x.Checker = instance.Checker
