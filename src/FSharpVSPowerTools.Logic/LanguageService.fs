namespace FSharpVSPowerTools.ProjectSystem

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.TextManager.Interop
open FSharp.CompilerBinding
open FSharpVSPowerTools

[<Export>]
type VSLanguageService
    [<ImportingConstructor>] 
    ([<Import(typeof<SVsServiceProvider>)>] serviceProvider, 
     editorFactory: IVsEditorAdaptersFactoryService,
     fsharpLanguageService: FSharpLanguageService) =

    let instance = LanguageService(fun _ -> ())
    let solutionEvents = SolutionEvents(serviceProvider)
    do ProjectCache.listen(solutionEvents)
    do ProjectCache.projectChanged.Add (fun p -> 
        debug "[Language Service] InteractiveChecker.InvalidateConfiguration for %s" p.ProjectFileName
        let opts = instance.GetCheckerOptions (null, p.ProjectFileName, null, p.SourceFiles, 
                                               p.CompilerOptions, p.TargetFramework)
        instance.InvalidateConfiguration(opts))

    let buildQueryLexState (textBuffer: ITextBuffer) source defines line =
        try
            let vsColorState = editorFactory.GetBufferAdapter(textBuffer) :?> IVsTextColorState
            let colorState = fsharpLanguageService.GetColorStateAtStartOfLine(vsColorState, line)
            fsharpLanguageService.LexStateOfColorState(colorState)
        with e ->
            debug "[Language Service] %O exception occurs while getting symbol." e
            SymbolParser.queryLexState source defines line
    
    member x.TryGetLocation (symbol: FSharpSymbol) =
        Option.orElse symbol.ImplementationLocation symbol.DeclarationLocation

    member x.GetSymbol(point: SnapshotPoint, projectProvider: IProjectProvider) =
        let source = point.Snapshot.GetText()
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let col = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()                
        let args = projectProvider.CompilerOptions
                                
        SymbolParser.getSymbol source line col lineStr args (Some <| buildQueryLexState point.Snapshot.TextBuffer)
        |> Option.map (fun symbol -> point.FromRange symbol.Range, symbol)

    member x.ProcessNavigableItemsInProject(openDocuments, projectProvider: IProjectProvider, processNavigableItems, ct) =
        instance.ProcessParseTrees(
            projectProvider.ProjectFileName, 
            openDocuments, 
            projectProvider.SourceFiles, 
            projectProvider.CompilerOptions, 
            projectProvider.TargetFramework, 
            (Navigation.NavigableItemsCollector.collect >> processNavigableItems), 
            ct)

    member x.FindUsages (word: SnapshotSpan, currentFile: string, projectProvider: IProjectProvider) =
        async {
            try 
                let (_, _, endLine, endCol) = word.ToRange()
                let projectFileName = projectProvider.ProjectFileName
                let source = word.Snapshot.GetText()
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = projectProvider.TargetFramework
                let args = projectProvider.CompilerOptions
                let sourceFiles = projectProvider.SourceFiles
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                return! 
                    instance.GetUsesOfSymbolInProjectAtLocationInFile
                        (projectFileName, currentFile, source, sourceFiles, endLine, endCol, currentLine, 
                        args, framework, buildQueryLexState word.Snapshot.TextBuffer)
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
            
                let! res = x.GetFSharpSymbol (word, sym, currentFile, projectProvider, stale)
                return res |> Option.map (fun (_, checkResults) -> x.FindUsagesInFile (word, sym, checkResults))
            with e ->
                debug "[Language Service] %O exception occurs while updating." e
                return None }

    member x.FindUsagesInFile (word: SnapshotSpan, sym: Symbol, fileScopedCheckResults: ParseAndCheckResults) =
        try 
            let (_, _, endLine, _) = word.ToRange()
            let currentLine = word.Start.GetContainingLine().GetText()
            
            debug "[Language Service] Get symbol references for '%s' at line %d col %d" (word.GetText()) endLine sym.RightColumn
            fileScopedCheckResults.GetUsesOfSymbolInFileAtLocation (endLine, sym.RightColumn, currentLine, sym.Text)
        with e ->
            debug "[Language Service] %O exception occurs while updating." e
            None

    member x.GetFSharpSymbol (word: SnapshotSpan, symbol: Symbol, currentFile: string, projectProvider: IProjectProvider, stale) = 
        async {
            let (_, _, endLine, _) = word.ToRange()
            let projectFileName = projectProvider.ProjectFileName
            let source = word.Snapshot.GetText()
            let currentLine = word.Start.GetContainingLine().GetText()
            let framework = projectProvider.TargetFramework
            let args = projectProvider.CompilerOptions
            let sourceFiles = projectProvider.SourceFiles
            let! results = instance.ParseAndCheckFileInProject(projectFileName, currentFile, source, sourceFiles, args, framework, stale)
            let symbol = results.GetSymbolAtLocation (endLine+1, symbol.RightColumn, currentLine, [symbol.Text])
            return symbol |> Option.map (fun s -> s, results)
        }

    member x.Checker = instance.Checker
