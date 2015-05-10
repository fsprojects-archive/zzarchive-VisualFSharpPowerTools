namespace FSharpVSPowerTools.SymbolInfo

open System
open System.IO
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows
open System.Threading
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Projection
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.AsyncMaybe
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.ViewModule
open FSharpVSPowerTools.CodeGeneration
open Microsoft.FSharp.Compiler.SourceCodeServices

type SymbolInfoVisual = FsXaml.XAML<"SymbolInfoMargin.xaml", ExposeNamedProperties=true>

type SymbolInfoViewModel() as self = 
    inherit ViewModelBase()
    let symbolInfo = self.Factory.Backing(<@@ self.SymbolInfo @@>, "")
    member __.SymbolInfo
        with get () = symbolInfo.Value
        and set (v) = symbolInfo.Value <- v

type SymbolInfoMargin (textDocument: ITextDocument,
                       view: ITextView, 
                       vsLanguageService: VSLanguageService, 
                       serviceProvider: IServiceProvider,
                       projectFactory: ProjectFactory) = 

    let updateLock = obj()
    let mutable currentWord = None
    let model = SymbolInfoViewModel()
    let visual = SymbolInfoVisual()
    do visual.Root.DataContext <- model
    let mutable requestedPoint = SnapshotPoint()

    let buffer = view.TextBuffer

    // Perform a synchronous update, in case multiple background threads are running
    let synchronousUpdate(currentRequest: SnapshotPoint, newWord: SnapshotSpan option, si: string option) =
        lock updateLock (fun () ->
            if currentRequest = requestedPoint then
                currentWord <- newWord
                model.SymbolInfo <- match si with Some x -> x | None -> "")

    let doUpdate (currentRequest: SnapshotPoint, symbol, newWord: SnapshotSpan,
                  fileName: string, projectProvider: IProjectProvider) =
        async {
            if currentRequest = requestedPoint then
                try
                    let! res = vsLanguageService.GetFSharpSymbolUse (newWord, symbol, fileName, projectProvider, AllowStaleResults.No)
                    match res with
                    | Some (symbolUse, _) ->
                        let span = fromFSharpRange newWord.Snapshot symbolUse.RangeAlternate

                        match span with
                        | Some _ -> 
                            let lineStr = requestedPoint.GetContainingLine().GetText()
                            
                            let! tooltip =
                                vsLanguageService.GetOpenDeclarationTooltip(
                                    symbol.Line + 1, symbol.RightColumn, lineStr, [symbol.Text], projectProvider, 
                                    textDocument.FilePath, newWord.Snapshot.GetText())
                            let info = 
                                tooltip 
                                |> Option.bind (fun (FSharpToolTipText x) -> List.tryHead x)
                                |> Option.bind (function
                                    | FSharpToolTipElement.Single (s, _) -> Some s
                                    | FSharpToolTipElement.Group ((s, _) :: _) -> Some s
                                    | _ -> None)

//                            let fmt = SignatureGenerator.formatSymbol 
//                                        (fun _ -> [])
//                                        4 FSharpDisplayContext.Empty [] symbolUse.Symbol
//                                        SignatureGenerator.Filterer.NoFilters SignatureGenerator.BlankLines.Default false
                            synchronousUpdate (currentRequest, Some newWord, info)
                        | None ->
                            // Return empty values in order to clear up markers
                            synchronousUpdate (currentRequest, None, None)
                    | None -> synchronousUpdate (currentRequest, None, None)
                with e ->
                    Logging.logExceptionWithMessage e "Failed to update highlight references."
                    synchronousUpdate (currentRequest, None, None)
        }

    let updateAtCaretPosition () =
        // If the new cursor position is still within the current word (and on the same snapshot),
        // we don't need to check it.
        match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
        | Some point, Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> ()
        | Some point, _ ->
            asyncMaybe {
                requestedPoint <- point
                let currentRequest = requestedPoint
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetCurrentDocument(textDocument.FilePath) |> liftMaybe
                let! project = projectFactory.CreateForDocument buffer doc |> liftMaybe
                match vsLanguageService.GetSymbol(currentRequest, project) with
                | Some (newWord, symbol) ->
                    // If this is the same word we currently have, we're done (e.g. caret moved within a word).
                    match currentWord with
                    | Some cw when cw = newWord -> ()
                    | _ ->
                        // If we are still up-to-date (another change hasn't happened yet), do a real update
                        return! doUpdate (currentRequest, symbol, newWord, doc.FullName, project) |> liftAsync
                | None -> return synchronousUpdate (currentRequest, None, None)
            } 
            |> Async.map (Option.iter id)
            |> Async.StartInThreadPoolSafe
        | _ -> ()

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, 
                                                      updateAtCaretPosition)

    interface IWpfTextViewMargin with
        member __.VisualElement = upcast visual
        member __.MarginSize = visual.ActualHeight + 2.
        member __.Enabled = true
        
        member x.GetTextViewMargin name =
            match name with
            | Constants.symbolInfoMargin -> upcast x
            | _ -> Unchecked.defaultof<_>

    interface IDisposable with
        member __.Dispose() = (docEventListener :> IDisposable).Dispose()