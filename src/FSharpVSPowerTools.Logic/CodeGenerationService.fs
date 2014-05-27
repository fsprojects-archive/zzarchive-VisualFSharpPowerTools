namespace FSharpVSPowerTools

open Microsoft.VisualStudio.Text
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.ProjectSystem

type VSDocument(doc: EnvDTE.Document, snapshot: ITextSnapshot) =
    interface IDocument with
        member x.FullName = doc.FullName
        member x.GetText() = snapshot.GetText()
        member x.GetLineText0(line0) =
            snapshot.GetLineFromLineNumber(int line0).GetText()

        member x.GetLineText1(line1) =
            snapshot.GetLineFromLineNumber(int line1 - 1).GetText()

type CodeGenerationService(languageService: VSLanguageService) =
    interface ICodeGenerationService<IProjectProvider, SnapshotPoint, SnapshotSpan> with
        member x.GetSymbolAtPosition(project, _document, pos) =
            languageService.GetSymbol(pos, project)
        
        member x.GetSymbolAndUseAtPositionOfKind(project, document, pos, kind) =
            asyncMaybe {
                let x = x :> ICodeGenerationService<_, _, _>
                let! range, symbol = x.GetSymbolAtPosition(project, document, pos) |> liftMaybe

                match symbol.Kind with
                | k when k = kind ->
                    let! symbolUse, _ =
                        languageService.GetFSharpSymbolUse(range, symbol, document.FullName, project, AllowStaleResults.MatchingSource)
                    return range, symbol, symbolUse
                | _ -> return! None |> liftMaybe
            }

        member x.ParseFileInProject(document, project) =
            languageService.ParseFileInProject(document.FullName, document.GetText(), project)
        
        member x.ExtractFSharpPos(pos) =
            let line = pos.Snapshot.GetLineNumberFromPosition pos.Position
            let caretColumn = pos.Position - pos.GetContainingLine().Start.Position
            Pos.fromZ line caretColumn