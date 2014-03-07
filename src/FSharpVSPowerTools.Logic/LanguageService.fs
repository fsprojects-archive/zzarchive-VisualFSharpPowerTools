namespace FSharpVSPowerTools.ProjectSystem

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Shell
open FSharp.CompilerBinding

[<Export>]
type VSLanguageService [<ImportingConstructor>] ([<Import(typeof<SVsServiceProvider>)>] serviceProvider) =
    // TODO: we should reparse the stale document and cache it
    let instance = LanguageService(fun _ -> ())
    let solutionEvents = SolutionEvents(serviceProvider)
    do ProjectCache.listen(solutionEvents)
    do ProjectCache.projectChanged.Add (fun p -> 
        debug "[Language Service] InteractiveChecker.InvalidateConfiguration for %s" p.ProjectFileName
        let req = 
            { FileName = null
              ProjFileName = p.ProjectFileName
              Source = null
              Files = p.SourceFiles
              CompilerOptions = p.CompilerOptions
              TargetFramework = p.TargetFramework }

        let opts = instance.GetCheckerOptions req
        instance.Checker.InvalidateConfiguration opts)
    
    let findUsages (getUsesOfSymbolAtLocation, word: SnapshotSpan, currentFile: string, projectProvider: IProjectProvider) =
        async {
            try 
                let (_, _, endLine, endCol) = word.ToRange()
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = projectProvider.TargetFramework

                let sourceFiles = 
                    match projectProvider.SourceFiles with
                    // If there is no source file, use currentFile as an independent script
                    | [||] -> [| currentFile |] 
                    | files -> files

                let req = 
                     { FileName = currentFile
                       ProjFileName = projectProvider.ProjectFileName
                       Source = word.Snapshot.GetText()
                       Files = sourceFiles
                       CompilerOptions = projectProvider.CompilerOptions
                       TargetFramework = projectProvider.TargetFramework }
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " req.CompilerOptions)
            
                return! getUsesOfSymbolAtLocation (req, endLine, endCol, currentLine)
            with e ->
                debug "[Language Service] %O exception occurs while updating." e
                return None }

    member x.TryGetLocation (symbol: FSharpSymbol) =
        Option.orElse symbol.ImplementationLocation symbol.DeclarationLocation

    member x.GetSymbol(point: SnapshotPoint, projectProvider : IProjectProvider) =
        let source = point.Snapshot.GetText()
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let col = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()                
        let args = projectProvider.CompilerOptions                
        instance.GetSymbol (source, line, col, lineStr, args)
        |> Option.map (fun symbol -> point.FromRange symbol.Range, symbol)

    member x.ProcessNavigableItemsInProject(openDocuments, p: IProjectProvider, processNavigableItems, ct) =
        let req =
            { FileName = null
              ProjFileName = p.ProjectFileName
              Source = null
              Files =  p.SourceFiles
              CompilerOptions = p.CompilerOptions
              TargetFramework = p.TargetFramework }

        instance.ProcessParseTrees (req, openDocuments, (Navigation.NavigableItemsCollector.collect >> processNavigableItems), ct)

    member x.FindUsages (word: SnapshotSpan, currentFile: string, projectProvider: IProjectProvider) =
        findUsages (instance.GetUsesOfSymbolAtLocation, word, currentFile, projectProvider)

    member x.FindUsagesInFile (word: SnapshotSpan, currentFile: string, projectProvider: IProjectProvider) =
        findUsages (instance.GetUsesOfSymbolAtLocationInFile, word, currentFile, projectProvider)

    member x.Checker = instance.Checker
