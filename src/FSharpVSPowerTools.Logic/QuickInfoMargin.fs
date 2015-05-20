namespace FSharpVSPowerTools.QuickInfo

open System
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.AsyncMaybe
open FSharp.ViewModule

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
    let buffer = view.TextBuffer

    let updateError error = lock updateLock (fun () -> model.QuickInfo <- match error with Some x -> x | None -> "")

    let updateAtCaretPosition () =
        let caretPos = view.Caret.Position
        match buffer.GetSnapshotPoint caretPos with
        | Some point ->
            asyncMaybe {
                let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                let! doc = dte.GetCurrentDocument(textDocument.FilePath) |> liftMaybe
                let! project = projectFactory.CreateForDocument buffer doc |> liftMaybe
                let! checkResults = vsLanguageService.ParseAndCheckFileInProject(textDocument.FilePath, buffer.CurrentSnapshot.GetText(), project) |> liftAsync
                let! errors = checkResults.GetErrors() |> liftMaybe
                do! (if Array.isEmpty errors then None else Some()) |> liftMaybe
                return 
                    [| for e in errors do
                         if String.Equals(textDocument.FilePath, e.FileName, StringComparison.InvariantCultureIgnoreCase) then
                             match fromRange buffer.CurrentSnapshot (e.StartLineAlternate, e.StartColumn, e.EndLineAlternate, e.EndColumn) with
                             | Some span when point.InSpan span -> yield sprintf "%+A: %s" e.Severity e.Message 
                             | _ -> () |]
                    |> String.concat Environment.NewLine 
            } 
            |> Async.map updateError
            |> Async.StartInThreadPoolSafe
        | _ -> updateError None

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, updateAtCaretPosition)

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