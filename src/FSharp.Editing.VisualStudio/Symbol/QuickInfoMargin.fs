namespace FSharp.Editing.VisualStudio.Symbol

open System
open System.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open FSharp.Editing
open FSharp.Editing.VisualStudio
open FSharp.ViewModule
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.Editing.StringBuilder

type QuickInfoVisual = FsXaml.XAML< @"Gui/QuickInfoMargin.xaml">

type QuickInfoViewModel() as self = 
    inherit ViewModelBase()
    let quickInfo = self.Factory.Backing(<@@ self.QuickInfo @@>, "")
    member __.QuickInfo
        with get () = quickInfo.Value
        and set v = quickInfo.Value <- v

type QuickInfoMargin 
    (
        doc: ITextDocument,
        view: ITextView,
        vsLanguageService: VSLanguageService,
        projectFactory: ProjectFactory
    ) =

    let updateLock = obj()
    let model = QuickInfoViewModel()
    let visual = QuickInfoVisual()

    do 
        visual.DataContext <- model
        visual.tbQuickInfo.MouseDoubleClick.Add (fun _ ->
           System.Windows.Clipboard.SetText visual.tbQuickInfo.Text
           visual.tbQuickInfo.SelectAll())

    let buffer = view.TextBuffer
    let mutable currentWord: SnapshotSpan option = None

    let updateQuickInfo (tooltip: string option, errors: ((FSharpErrorSeverity * string list) []) option,
                         newWord: SnapshotSpan option) = lock updateLock <| fun () -> 
        currentWord <- newWord
        
        // helper function to lead a string builder across the collection of
        // errors accumulating lines annotated with their index number
        let errorString (errors:string list) (sb:StringBuilder) =
            match errors with
            | [e] -> append e sb
            | _ -> (sb, errors ) ||> List.foldi (fun sb i e ->
                sb |> append (sprintf "%d. %s" (i + 1) e) |> append " ")        
        
        let currentInfo =
            match errors, tooltip with
            | Some es, _ -> // if the tooltip contains errors show them
                let sb = StringBuilder ()
                for (severity, err) in es do
                    let errorls = List.map String.trim err
                    let title =
                        match errorls with
                        | [_] -> sprintf "%A" severity
                        | _ -> sprintf "%A (%d)" severity errorls.Length
                    sb |> append title |> append ": " |> errorString errorls |> appendi " "
                string sb
            | None, Some tt ->   // show type info if there aren't any errors
                match  String.firstNonEmptyLine tt with
                | Some str ->
                    if str.StartsWith ("type ", StringComparison.Ordinal) then
                        let index = str.LastIndexOf ("=", StringComparison.Ordinal)
                        if index > 0 then str.[0..index-1] else str
                    else str
                | None -> ""
            | None, None -> ""  // if there are no results the panel will be empty        

        model.QuickInfo <- currentInfo

    let flattener (sb:StringBuilder) (str:string) : StringBuilder =
            if str.Length > 0 && Char.IsUpper str.[0] then (sb.Append ". ").Append (String.trim str)
            else (sb.Append " ").Append (String.trim str)


    let flattenLines (str:string) : string =
        if isNull str then "" else
        let flatstr =
            str
            |> String.getNonEmptyLines
            |> Array.foldi (fun sb i line ->
                if i = 0 then sb |> append (String.trim line)
                else flattener sb line) (new StringBuilder())
            |> string
        match flatstr |> String.toCharArray |> Array.tryLast with
        | None  -> ""
        | Some '.' -> flatstr
        | Some _ -> flatstr + "."

    let project() = projectFactory.CreateForDocument buffer doc.FilePath

    let updateAtCaretPosition (CallInUIContext callInUIContext) =
        async {
            let caretPos = view.Caret.Position
            match buffer.GetSnapshotPoint caretPos, currentWord with
            | Some point, Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> ()
            | Some point, _ ->
                let! res = 
                    asyncMaybe {
                        let! project = project()
                        let! tooltip, newWord =
                            asyncMaybe {
                                let! newWord, longIdent = vsLanguageService.GetSymbol (point, doc.FilePath, project)
                                let lineStr = point.GetContainingLine().GetText()
                                let idents = String.split StringSplitOptions.None [|"."|] longIdent.Text |> Array.toList
                                let! (FSharpToolTipText tooltip) =
                                    vsLanguageService.GetOpenDeclarationTooltip(
                                        longIdent.Line + 1, longIdent.RightColumn, lineStr, idents, project,
                                        doc.FilePath)
                                let! tooltip =
                                    tooltip
                                    |> List.tryHead
                                    |> Option.bind (function
                                        | FSharpToolTipElement.Single (s, _) -> Some s
                                        | FSharpToolTipElement.Group ((s, _) :: _) -> Some s
                                        | _ -> None)
                                return Some tooltip, newWord
                            }
                        let! checkResults = 
                            vsLanguageService.ParseAndCheckFileInProject(doc.FilePath, project, AllowStaleResults.MatchingSource)
                        let! errors =
                            asyncMaybe {
                                let! errors = checkResults.CheckErrors
                                do! (if Array.isEmpty errors then None else Some())
                                return!
                                    seq { for e in errors do
                                            if String.Equals(doc.FilePath, e.FileName, StringComparison.InvariantCultureIgnoreCase) then
                                                match fromRange buffer.CurrentSnapshot (e.StartLineAlternate, e.StartColumn, e.EndLineAlternate, e.EndColumn) with
                                                | Some span when point.InSpan span -> yield e.Severity, flattenLines e.Message
                                                | _ -> () }
                                    |> Seq.groupBy fst
                                    |> Seq.sortBy (fun (severity, _) -> if severity = FSharpErrorSeverity.Error then 0 else 1)
                                    |> Seq.map (fun (s, es) -> s, es |> Seq.map snd |> Seq.distinct |> List.ofSeq)
                                    |> Seq.toArray
                                    |> function [||] -> None | es -> Some es
                            } |> Async.map Some
                        return tooltip, errors, Some newWord
                    }
                let res = res |> Option.getOrElse (None, None, None) 
                return! callInUIContext (fun () -> updateQuickInfo res)
            | None, _ -> return updateQuickInfo (None, None, None)
        } |> Async.Ignore

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
