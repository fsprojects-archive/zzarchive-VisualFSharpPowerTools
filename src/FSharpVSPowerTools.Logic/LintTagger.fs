namespace FSharpVSPowerTools.Linting

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open FSharpLint.Application

module Lint =
    [<Literal>]
    let TagErrorType = "F# Lint"

type LintTag(tooltip) = 
    inherit ErrorTag(Lint.TagErrorType, tooltip)

type LintTagger(textDocument: ITextDocument,
                view: ITextView, 
                vsLanguageService: VSLanguageService, 
                serviceProvider: IServiceProvider,
                projectFactory: ProjectFactory) as self =
    let tagsChanged = Event<_, _>()
    let mutable wordSpans = []

    let buffer = view.TextBuffer

    let updateAtCaretPosition () =
        asyncMaybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            let! project = projectFactory.CreateForDocument buffer doc
            let source = buffer.CurrentSnapshot.GetText()
            let! checkResults = vsLanguageService.ParseAndCheckFileInProject (doc.FullName, source, project) |> liftAsync
            let! ast = checkResults.GetUntypedAst()
            let res = 
                Lint.lintParsedFile 
                    { FinishEarly = None
                      Configuration = None //Configuration.defaultConfiguration
                      ReceivedWarning = None }
                    { Ast = ast
                      Source = source
                      TypeCheckResults = checkResults.GetCheckResults() }
                    doc.FullName

            return
                match res with
                | LintResult.Success warnings ->
                    warnings |> List.choose (fun warn -> 
                        fromFSharpRange view.TextBuffer.CurrentSnapshot warn.Range
                        |> Option.map (fun span -> warn, span))
                | LintResult.Failure _ -> []
        }
        |> Async.map (fun spans -> 
            let spans = 
                match spans with 
                | Some x -> x
                | None -> []
            wordSpans <- spans
            let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
            tagsChanged.Trigger(self, SnapshotSpanEventArgs span))
        |> Async.StartInThreadPoolSafe

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.bufferEvent view.TextBuffer], 200us, 
                                                      updateAtCaretPosition)

    let getTags (spans: NormalizedSnapshotSpanCollection): ITagSpan<LintTag> list = 
        [
            match wordSpans with
            | [] -> ()
            | _ ->
                let currentSnapshot = spans.[0].Snapshot
                let wordSpans = 
                    if currentSnapshot = (snd wordSpans.[0]).Snapshot then
                        wordSpans
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        wordSpans |> List.map (fun (warn, span) -> warn, span.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive))
                
                for warn, span in wordSpans do
                    if spans.OverlapsWith span then
                        yield upcast TagSpan<LintTag>(span, LintTag(warn.Info))
        ]

    interface ITagger<LintTag> with
        member __.GetTags spans =
            upcast (protectOrDefault (fun _ -> getTags spans) [])
        
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
