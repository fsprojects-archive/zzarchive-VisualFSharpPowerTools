namespace FSharp.Editing.VisualStudio.CodeGeneration

open Microsoft.VisualStudio.Text
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.VisualStudio
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.Editing.Features
open FSharp.Editing.Infrastructure

type VSDocument(source: string, filePath: FilePath, snapshot: ITextSnapshot) =
    interface IDocument with
        member __.FullName = filePath
        member __.LineCount = snapshot.LineCount
        member __.GetText() = source
        member __.GetLineText0(line0) =
            snapshot.GetLineFromLineNumber(int line0).GetText()

        member __.GetLineText1(line1) =
            snapshot.GetLineFromLineNumber(int line1 - 1).GetText()
        
type CodeGenerationService(languageService: VSLanguageService, textBuffer: ITextBuffer) =
    interface ICodeGenerationService<IProjectProvider, SnapshotPoint, SnapshotSpan> with
        member __.TokenizeLine(project: IProjectProvider, document: IDocument, line1: int<Line1>): FSharpTokenInfo list option = 
            languageService.TokenizeLine(document.FullName, textBuffer, project.Project.CompilerOptions, int line1 - 1)
        
        member __.GetSymbolAtPosition(project, document, pos) =
            languageService.GetSymbol(pos, document.FullName, project)
        
        member x.GetSymbolAndUseAtPositionOfKind(project, document, pos, kind) =
            asyncMaybe {
                let x = x :> ICodeGenerationService<_, _, _>
                let! range, symbol = x.GetSymbolAtPosition(project, document, pos)

                if symbol.Kind = kind then
                    let! symbolUse, _ =
                        languageService.GetFSharpSymbolUse(range, symbol, document.FullName, project, AllowStaleResults.MatchingSource)
                    return range, symbol, symbolUse
                else
                    return! None
            }

        member __.ParseFileInProject(document, project) = languageService.ParseFileInProject(document.FullName, project)
        
        member __.ExtractFSharpPos(pos) =
            let line = pos.Snapshot.GetLineNumberFromPosition pos.Position
            let caretColumn = pos.Position - pos.GetContainingLine().Start.Position
            Pos.fromZ line caretColumn

type RefactoringIconKind =
    | ExtractMethod = 0
    | EncapsulateField = 1
    | ExtractInterface = 2
    | Rename = 3
    | ReorderParameters = 4
    | RemoveParameters = 5
    | AddUsing = 6
    | GenerateMethod = 7
    | PromoteLocal = 8
    | Snippet = 9

module ResourceProvider =
    open System
    open Microsoft.VisualStudio.Shell.Interop
    open System.Drawing
    open System.Windows.Interop
    open System.Windows
    open System.Windows.Media.Imaging
    open System.Windows.Media
    open Microsoft.VisualStudio

    let getRefactoringIcon (serviceProvider: IServiceProvider) (kind: RefactoringIconKind) =
        let manager = serviceProvider.GetService<IVsResourceManager, SVsResourceManager>()
        let cmdDefUiPackageGuid = Guid "{44E07B02-29A5-11D3-B882-00C04F79F802}"
        let IDBMP_REFACTOR_IMAGES = "#2029"        
        match manager.LoadResourceBitmap(ref cmdDefUiPackageGuid, 0, IDBMP_REFACTOR_IMAGES) with
        | VSConstants.S_OK, hbmpValue ->
            let iconSize = 16
            use bitmap = Bitmap.FromHbitmap(hbmpValue)
            // Get rid of the backdrop behind the refactoring icons.
            bitmap.MakeTransparent(System.Drawing.Color.Black)
            Imaging.CreateBitmapSourceFromHBitmap(
                       bitmap.GetHbitmap(), 
                       IntPtr.Zero,
                       Int32Rect(int kind * iconSize, 0, iconSize, iconSize),
                       BitmapSizeOptions.FromEmptyOptions()) :> ImageSource
         | _ -> 
            null    