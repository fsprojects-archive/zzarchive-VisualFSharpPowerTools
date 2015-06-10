namespace FSharpVSPowerTools.QuickInfo

open System
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.AsyncMaybe
open FSharp.ViewModule
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

type QuickInfoVisual = FsXaml.XAML<"QuickInfoMargin.xaml", ExposeNamedProperties=true>

type QuickInfoViewModel() as self = 
    inherit ViewModelBase()
    let quickInfo = self.Factory.Backing(<@@ self.QuickInfo @@>, "")
    member __.QuickInfo
        with get () = quickInfo.Value
        and set (v) = quickInfo.Value <- v

type QuickInfoMargin (textDocument: ITextDocument,
                      view: ITextView, 
                      vsLanguageService: VSLanguageService, 
                      serviceProvider: IServiceProvider,
                      projectFactory: ProjectFactory) = 

    let updateLock = obj()
    let model = QuickInfoViewModel()
    let visual = QuickInfoVisual()

    do visual.Root.DataContext <- model
       visual.tbQuickInfo.MouseDoubleClick.Add (fun _ ->
           System.Windows.Clipboard.SetText visual.tbQuickInfo.Text
           visual.tbQuickInfo.SelectAll())
    
    let buffer = view.TextBuffer

    let updateQuickInfo (tooltip: string option, errors: ((FSharpErrorSeverity * string list) []) option) = lock updateLock <| fun () -> 
        model.QuickInfo <-
            errors
            |> Option.map (fun errors ->
                errors 
                |> Array.map (fun (severity, errors) -> 
                    let errors = List.map String.trim errors
                    let title = 
                        match errors with
                        | [_] -> sprintf "%+A" severity
                        | _ -> sprintf "%+As (%d)" severity errors.Length
                    title + ": " + 
                    (match errors with 
                     | [e] -> e
                     | _ -> 
                        errors 
                        |> List.mapi (fun i e -> sprintf "%d. %s" (i + 1) e) 
                        |> List.toArray 
                        |> String.concat " "))
                |> String.concat " ")
            |> Option.orElse (tooltip |> Option.bind (fun tooltip ->
                tooltip 
                |> String.split StringSplitOptions.RemoveEmptyEntries [|"\r\n"; "\r"; "\n"|] 
                |> Array.toList 
                |> List.tryHead))
            |> Option.getOrElse ""

    let flattenLines (x: string) =
        match x with
        | null -> ""
        | x ->
            x 
            |> String.split StringSplitOptions.RemoveEmptyEntries [|"\r\n"; "\r"; "\n"|]
            |> Array.mapi (fun i x -> 
                if i > 0 && x.Length > 0 && Char.IsUpper x.[0] then ". " + (String.trim x) 
                else " " + (String.trim x))
            |> String.concat ""
            |> fun x -> if x.Length > 0 && x.[x.Length - 1] <> '.' then x + "." else x

    let updateAtCaretPosition () =
        let caretPos = view.Caret.Position
        match buffer.GetSnapshotPoint caretPos with
        | Some point ->
            asyncMaybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetCurrentDocument(textDocument.FilePath)
                let! project = projectFactory.CreateForDocument buffer doc
                let! tooltip =
                    asyncMaybe {
                        let! newWord, symbol = vsLanguageService.GetSymbol (point, project)
                        let lineStr = point.GetContainingLine().GetText()

                        let! (FSharpToolTipText tooltip) =
                            vsLanguageService.GetOpenDeclarationTooltip(
                                symbol.Line + 1, symbol.RightColumn, lineStr, [symbol.Text], project, 
                                textDocument.FilePath, newWord.Snapshot.GetText())
                        return!
                            tooltip
                            |> List.tryHead
                            |> Option.bind (function
                                | FSharpToolTipElement.Single (s, _) -> Some s
                                | FSharpToolTipElement.Group ((s, _) :: _) -> Some s
                                | _ -> None)
                    } |> Async.map Some
                let! checkResults = 
                    vsLanguageService.ParseAndCheckFileInProject(textDocument.FilePath, buffer.CurrentSnapshot.GetText(), project) |> liftAsync
                let! errors =
                    asyncMaybe {
                        let! errors = checkResults.GetErrors()
                        do! (if Array.isEmpty errors then None else Some())
                        return!
                            seq { for e in errors do
                                    if String.Equals(textDocument.FilePath, e.FileName, StringComparison.InvariantCultureIgnoreCase) then
                                        match fromRange buffer.CurrentSnapshot (e.StartLineAlternate, e.StartColumn, e.EndLineAlternate, e.EndColumn) with
                                        | Some span when point.InSpan span -> yield e.Severity, flattenLines e.Message
                                        | _ -> () }
                            |> Seq.groupBy fst
                            |> Seq.sortBy (fun (severity, _) -> if severity = FSharpErrorSeverity.Error then 0 else 1)
                            |> Seq.map (fun (s, es) -> s, es |> Seq.map snd |> Seq.distinct |> List.ofSeq)
                            |> Seq.toArray
                            |> function [||] -> None | es -> Some es
                    } |> Async.map Some
                return tooltip, errors
            } 
            |> Async.map (Option.getOrElse (None, None) >> updateQuickInfo)
            |> Async.StartInThreadPoolSafe
        | _ -> updateQuickInfo (None, None)

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, updateAtCaretPosition)

    interface IWpfTextViewMargin with
        member __.VisualElement = upcast visual
        member __.MarginSize = visual.ActualHeight + 2.
        member __.Enabled = true
        
        member x.GetTextViewMargin name =
            match name with
            | Constants.QuickInfoMargin -> upcast x
            | _ -> Unchecked.defaultof<_>

    interface IDisposable with
        member __.Dispose() = (docEventListener :> IDisposable).Dispose()