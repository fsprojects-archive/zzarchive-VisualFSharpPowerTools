namespace FSharpVSPowerTools.ProjectSystem

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Editor
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.TextManager.Interop
open FSharp.CompilerBinding
open FSharpVSPowerTools

[<Export>]
type VSLanguageService
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider, 
     editorFactory: IVsEditorAdaptersFactoryService,
     fsharpLanguageService: FSharpLanguageService) =

    let mutable instance = LanguageService (fun _ -> ())
    
    let invalidateProject (projectItem: EnvDTE.ProjectItem) =
        async {
            let project = projectItem.ContainingProject
            if box project <> null && isFSharpProject project then
                let p = ProjectProvider.createForProject project
                debug "[Language Service] InteractiveChecker.InvalidateConfiguration for %s" p.ProjectFileName
                let! opts = instance.GetCheckerOptions (null, p.ProjectFileName, null, p.SourceFiles, 
                                                       p.CompilerOptions, p.TargetFramework)
                return instance.InvalidateConfiguration(opts)
        }
        |> Async.StartImmediate

    let dte = serviceProvider.GetService<EnvDTE.DTE, Interop.SDTE>()
    let events = dte.Events :?> EnvDTE80.Events2
    let projectItemsEvents = events.ProjectItemsEvents
    do projectItemsEvents.add_ItemAdded(fun p -> invalidateProject p)
    do projectItemsEvents.add_ItemRemoved(fun p -> invalidateProject p)
    do projectItemsEvents.add_ItemRenamed(fun p _ -> invalidateProject p)
    do events.SolutionEvents.add_AfterClosing (fun _ -> instance <- LanguageService (fun _ -> ()))

    let buildQueryLexState (textBuffer: ITextBuffer) source defines line =
        try
            let vsColorState = editorFactory.GetBufferAdapter(textBuffer) :?> IVsTextColorState
            let colorState = fsharpLanguageService.GetColorStateAtStartOfLine(vsColorState, line)
            fsharpLanguageService.LexStateOfColorState(colorState)
        with e ->
            debug "[Language Service] %O exception occurs while querying lexing states." e
            Lexer.queryLexState source defines line
    
    member x.GetSymbol(point: SnapshotPoint, projectProvider: IProjectProvider) =
        let source = point.Snapshot.GetText()
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let col = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()                
        let args = projectProvider.CompilerOptions
                                
        Lexer.getSymbol source line col lineStr args (buildQueryLexState point.Snapshot.TextBuffer)
        |> Option.map (fun symbol -> point.FromRange symbol.Range, symbol)

    member x.TokenizeLine(textBuffer: ITextBuffer, args: string[], line) =
        let snapshot = textBuffer.CurrentSnapshot
        let source = snapshot.GetText()
        let lineStr = snapshot.GetLineFromLineNumber(line).GetText()
        Lexer.tokenizeLine source args line lineStr (buildQueryLexState textBuffer)

    member x.ParseFileInProject (currentFile: string, source, projectProvider: IProjectProvider) =
       instance.ParseFileInProject(
            projectProvider.ProjectFileName,
            currentFile,
            source,
            projectProvider.SourceFiles,
            projectProvider.CompilerOptions,
            projectProvider.TargetFramework) 

    member x.ProcessNavigableItemsInProject(openDocuments, projectProvider: IProjectProvider, processNavigableItems, ct) =
        instance.ProcessParseTrees(
            projectProvider.ProjectFileName, 
            openDocuments, 
            projectProvider.SourceFiles, 
            projectProvider.CompilerOptions, 
            projectProvider.TargetFramework, 
            (Navigation.NavigableItemsCollector.collect >> processNavigableItems), 
            ct)        

    member x.FindUsages (word: SnapshotSpan, currentFile: string, project: IProjectProvider, dependencies) =
        async {
            try 
                let (_, _, endLine, endCol) = word.ToRange()
                let source = word.Snapshot.GetText()
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = project.TargetFramework
                let args = project.CompilerOptions
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                let dependentProjectsOptions = 
                    project.GetDependentProjects (serviceProvider.GetService<EnvDTE.DTE>()) dependencies
                    |> List.map (fun p -> p.GetProjectCheckerOptions())

                let projectOptions = project.GetDescription().GetProjectCheckerOptions()

                return! 
                    instance.GetUsesOfSymbolInProjectAtLocationInFile
                        (projectOptions, dependentProjectsOptions, currentFile, source, endLine, endCol, 
                         currentLine, args, buildQueryLexState word.Snapshot.TextBuffer)
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
                return res |> Option.map (fun (_, checkResults) -> x.FindUsagesInFile (word, sym, checkResults))
            with e ->
                debug "[Language Service] %O exception occurs while updating." e
                return None }

    member x.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, fileScopedCheckResults: ParseAndCheckResults) =
        async {
            try 
                let (_, _, endLine, _) = word.ToRange()
                let currentLine = word.Start.GetContainingLine().GetText()
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d" (word.GetText()) endLine sym.RightColumn
                return! fileScopedCheckResults.GetUsesOfSymbolInFileAtLocation (endLine, sym.RightColumn, currentLine, sym.Text)
            with e ->
                debug "[Language Service] %O exception occurs while finding usages in file." e
                return None
        }

    member x.GetFSharpSymbolUse (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) = 
        async {
            let (_, _, endLine, _) = word.ToRange()
            let projectFileName = projectProvider.ProjectFileName
            let source = word.Snapshot.GetText()
            let currentLine = word.Start.GetContainingLine().GetText()
            let framework = projectProvider.TargetFramework
            let args = projectProvider.CompilerOptions
            let sourceFiles = projectProvider.SourceFiles
            let! results = instance.ParseAndCheckFileInProject(   
                               projectFileName, currentFile, source, sourceFiles, args, framework, stale)
            let! symbol = results.GetSymbolUseAtLocation (endLine+1, symbol.RightColumn, currentLine, [symbol.Text])
            return symbol |> Option.map (fun s -> s, results)
        }

    member x.GetAllUsesOfAllSymbolsInFile (snapshot: ITextSnapshot, currentFile: string, projectProvider: IProjectProvider, stale) = 
        async {
            let projectFileName = projectProvider.ProjectFileName
            let source = snapshot.GetText()
            let framework = projectProvider.TargetFramework
            let args = projectProvider.CompilerOptions
            let sourceFiles = 
                match projectProvider.SourceFiles with
                // If there is no source file, use currentFile as an independent script
                | [||] -> [| currentFile |] 
                | files -> files

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

            let! symbolUses = instance.GetAllUsesOfAllSymbolsInFile(
                                projectFileName, currentFile, source, sourceFiles, args, framework, stale)
            return symbolUses, lexer
        }

    member x.ParseFileInProject (snapshot: ITextSnapshot, currentFile: string, projectProvider: IProjectProvider) = 
        async {
            let projectFileName = projectProvider.ProjectFileName
            let source = snapshot.GetText()
            let framework = projectProvider.TargetFramework
            let args = projectProvider.CompilerOptions
            let sourceFiles = 
                match projectProvider.SourceFiles with
                | [||] -> [| currentFile |] 
                | files -> files

            return! instance.ParseFileInProject(projectFileName, currentFile, source, sourceFiles, args, framework)
        }

    member x.Checker = instance.Checker
