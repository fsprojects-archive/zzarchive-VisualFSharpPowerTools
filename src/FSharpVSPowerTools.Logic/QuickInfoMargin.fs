namespace FSharpVSPowerTools.QuickInfo

open System
open System.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.AsyncMaybe
open FSharp.ViewModule
open Microsoft.FSharp.Compiler
open System.Windows.Interactivity
open System.Windows.Controls
open System.Windows.Input

type QuickInfoVisual = FsXaml.XAML<"QuickInfoMargin.xaml", ExposeNamedProperties=true>

type DoubleClickBehavior() =
    inherit Behavior<TextBox>()
    let mutable associatedObjectMouseDoubleClick = null
    
    let initAssociatedObjectMouseDoubleClick (textBox: TextBox) =
        associatedObjectMouseDoubleClick <- MouseButtonEventHandler (fun sender routedEventArgs -> textBox.SelectAll())

    override x.OnAttached() =
        initAssociatedObjectMouseDoubleClick x.AssociatedObject
        x.AssociatedObject.MouseDoubleClick.AddHandler associatedObjectMouseDoubleClick
        base.OnAttached()

    override x.OnDetaching() =
        x.AssociatedObject.MouseDoubleClick.RemoveHandler associatedObjectMouseDoubleClick
        base.OnDetaching()

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
    let buffer = view.TextBuffer

    let updateError (errors: (FSharpErrorSeverity * string list) []) = lock updateLock <| fun () -> 
        model.QuickInfo <-
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
            |> String.concat " "

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
                let! checkResults = vsLanguageService.ParseAndCheckFileInProject(textDocument.FilePath, buffer.CurrentSnapshot.GetText(), project) |> liftAsync
                let! errors = checkResults.GetErrors()
                do! (if Array.isEmpty errors then None else Some())
                return 
                    seq { for e in errors do
                            if String.Equals(textDocument.FilePath, e.FileName, StringComparison.InvariantCultureIgnoreCase) then
                                match fromRange buffer.CurrentSnapshot (e.StartLineAlternate, e.StartColumn, e.EndLineAlternate, e.EndColumn) with
                                | Some span when point.InSpan span -> yield e.Severity, flattenLines e.Message
                                | _ -> () }
                    |> Seq.groupBy fst
                    |> Seq.sortBy (fun (severity, _) -> if severity = FSharpErrorSeverity.Error then 0 else 1)
                    |> Seq.map (fun (s, es) -> s, es |> Seq.map snd |> Seq.distinct |> List.ofSeq)
                    |> Seq.toArray
            } 
            |> Async.map (Option.getOrElse [||] >> updateError)
            |> Async.StartInThreadPoolSafe
        | _ -> updateError [||]

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