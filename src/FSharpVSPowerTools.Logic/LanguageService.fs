namespace FSharpVSPowerTools.ProjectSystem

open FSharpVSPowerTools.Core
open Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
module VSLanguageService =
    // TODO: we should reparse the stale document and cache it
    let Instance = FSharp.CompilerBinding.LanguageService(fun _ -> ())

    let findUsages (word : SnapshotSpan) (currentFile : string) (projectProvider : ProjectProvider)
                   (textSnapshot: ITextSnapshot) =
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
            
                debug "Get symbol references for '%s' at line %d col %d on %A framework and '%s' arguments" 
                      (word.GetText()) endLine endCol framework (String.concat " " args)
            
                let! results = Instance.GetUsesOfSymbolAtLocation(projectFileName, currentFile, source, sourceFiles, 
                                                                  endLine, endCol, currentLine, args, framework)
                return 
                    results 
                    |> Option.map (fun (_, lastIdent, _, refs) -> 
                        lastIdent,
                        refs 
                        |> Seq.choose (fun (symbolUse : FSharpSymbolUse) -> 
                            // We have to filter by file name otherwise the range is invalid wrt current snapshot
                            if symbolUse.FileName = currentFile then
                                // Range01 type consists of zero-based values, which is a bit confusing
                                Some (fromVSPos textSnapshot symbolUse.Range)
                            else None)
                        |> Seq.map (fun span -> 
                            // Sometimes F.C.S returns a composite identifier which should be truncated
                            let index = span.GetText().LastIndexOf (lastIdent)
                            if index > 0 then 
                                SnapshotSpan(textSnapshot, span.Start.Position + index, span.Length - index)
                            else span)
                        |> Seq.toList)
            with e ->
                debug "[Rename Refactoring] %O exception occurs while updating." e
                return None }