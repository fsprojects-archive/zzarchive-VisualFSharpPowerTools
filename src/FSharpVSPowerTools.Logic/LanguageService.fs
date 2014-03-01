namespace FSharpVSPowerTools.ProjectSystem

open FSharpVSPowerTools
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
module VSLanguageService =
    // TODO: we should reparse the stale document and cache it
    let Instance = FSharp.CompilerBinding.LanguageService(fun _ -> ())
    do ProjectCache.projectChanged.Add (fun p -> 
        debug "[Language Service] InteractiveChecker.InvalidateConfiguration for %s" p.ProjectFileName
        let opts = Instance.GetCheckerOptions (null, p.ProjectFileName, null, p.SourceFiles, 
                                               p.CompilerOptions, p.TargetFramework)
        Instance.Checker.InvalidateConfiguration opts)

    let getSymbol (point: SnapshotPoint) (projectProvider : ProjectProvider) =
        let source = point.Snapshot.GetText()
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let col = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()                
        let args = projectProvider.CompilerOptions                
        Instance.GetSymbol (source, line, col, lineStr, args)
        |> Option.map (fun symbol -> point.FromRange symbol.Range)

    let findUsages (word : SnapshotSpan) (currentFile : string) (projectProvider : ProjectProvider) =
        async {
            try 
                let (_, _, endLine, endCol) = word.ToRange()
                let projectFileName = projectProvider.ProjectFileName
                let source = word.Snapshot.GetText()
                let currentLine = word.Start.GetContainingLine().GetText()
                let framework = projectProvider.TargetFramework
                let args = projectProvider.CompilerOptions
                let sourceFiles = 
                    match projectProvider.SourceFiles with
                    // If there is no source file, use currentFile as an independent script
                    | [||] -> [| currentFile |] 
                    | files -> files
            
                debug "[Language Service] Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                return! Instance.GetUsesOfSymbolAtLocation(projectFileName, currentFile, source, sourceFiles, 
                                                           endLine, endCol, currentLine, args, framework)
            with e ->
                debug "[Language Service] %O exception occurs while updating." e
                return None }