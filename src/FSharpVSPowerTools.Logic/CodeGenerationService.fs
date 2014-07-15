namespace FSharpVSPowerTools

open Microsoft.VisualStudio.Text
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.ProjectSystem

type VSDocument(doc: EnvDTE.Document, snapshot: ITextSnapshot) =
    interface IDocument with
        member x.FullName = doc.FullName
        member x.LineCount = snapshot.LineCount
        member x.GetText() = snapshot.GetText()
        member x.GetLineText0(line0) =
            snapshot.GetLineFromLineNumber(int line0).GetText()

        member x.GetLineText1(line1) =
            snapshot.GetLineFromLineNumber(int line1 - 1).GetText()
        
type CodeGenerationService(languageService: VSLanguageService, textBuffer: ITextBuffer) =
    interface ICodeGenerationService<IProjectProvider, SnapshotPoint, SnapshotSpan> with
        member x.TokenizeLine(project: IProjectProvider, _document: IDocument, line1: int<Line1>): TokenInformation list = 
            languageService.TokenizeLine(textBuffer, project.CompilerOptions, int line1 - 1)
        
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

    let getRefactoringIcon (serviceProvider: IServiceProvider) (kind: RefactoringIconKind) =
        let manager = serviceProvider.GetService<IVsResourceManager, SVsResourceManager>()
        let hbmpValue: IntPtr ref = ref IntPtr.Zero
        let cmdDefUiPackageGuid = Guid "{44E07B02-29A5-11D3-B882-00C04F79F802}"
        let IDBMP_REFACTOR_IMAGES = "#2029"
        if manager.LoadResourceBitmap(ref cmdDefUiPackageGuid, 0, IDBMP_REFACTOR_IMAGES, hbmpValue) = 0 then
            let iconSize = 16
            use bitmap = Bitmap.FromHbitmap(!hbmpValue)
            // Get rid of the backdrop behind the refactoring icons.
            bitmap.MakeTransparent(System.Drawing.Color.Black)
            Imaging.CreateBitmapSourceFromHBitmap(
                       bitmap.GetHbitmap(), 
                       IntPtr.Zero,
                       Int32Rect(int kind * iconSize, 0, iconSize, iconSize),
                       BitmapSizeOptions.FromEmptyOptions()) :> ImageSource
         else null    