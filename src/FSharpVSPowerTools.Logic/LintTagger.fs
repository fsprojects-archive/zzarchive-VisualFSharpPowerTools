namespace FSharpVSPowerTools.Linting

open System
open System.IO
open System.Threading
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open FSharpLint.Application
open FSharpLint.Framework.Configuration
open Microsoft.FSharp.Compiler

type LintTag(tooltip) = 
    inherit ErrorTag(Constants.LintTagErrorType, tooltip)

type LintTagger(textDocument: ITextDocument,
                vsLanguageService: VSLanguageService, 
                serviceProvider: IServiceProvider,
                projectFactory: ProjectFactory) as self =
    let tagsChanged = Event<_, _>()
    let mutable wordSpans = []
    let buffer = textDocument.TextBuffer

    let updateAtCaretPosition () =
        let uiContext = SynchronizationContext.Current
        asyncMaybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            let! project = projectFactory.CreateForDocument buffer doc
            let source = buffer.CurrentSnapshot.GetText()
            let! parseFileResults = vsLanguageService.ParseFileInProject (doc.FullName, source, project) |> liftAsync
            let! ast = parseFileResults.ParseTree

            let lintOptions = Setting.getLintOptions serviceProvider
            lintOptions.UpdateDirectories()
            let config = Path.GetDirectoryName doc.FullName |> lintOptions.GetConfigurationForDirectory

            let shouldFileBeIgnored =
                match config.IgnoreFiles with
                | Some(ignoreFiles) ->
                    IgnoreFiles.shouldFileBeIgnored
                        ignoreFiles.Files
                        textDocument.FilePath
                | None -> false

            if not shouldFileBeIgnored then
                let res = 
                    let version = dte.Version |> VisualStudioVersion.fromDTEVersion |> VisualStudioVersion.toBestMatchFSharpVersion 
                    Lint.lintParsedFile
                        { Lint.OptionalLintParameters.Default with Configuration = Some config }
                        { Ast = ast
                          Source = source
                          TypeCheckResults = None
                          FSharpVersion = version }
                        doc.FullName

                return
                    match res with
                    | LintResult.Success warnings ->
                        warnings 
                        |> Seq.choose (fun warn ->
                            let r = warn.Range
                            let endCol =
                                if r.StartLine = r.EndLine then
                                    min r.EndColumn (r.StartColumn + 2)
                                else r.StartColumn + 2
                            let range =
                                Range.mkRange "" 
                                    (Range.mkPos r.StartLine r.StartColumn)
                                    (Range.mkPos r.StartLine endCol)

                            fromFSharpRange buffer.CurrentSnapshot range
                            |> Option.map (fun span -> warn, span))
                        |> Seq.toList
                    | LintResult.Failure _ -> []
                else
                    return []
        }
        |> Async.bind (fun spans -> 
            async {
                let spans = spans |> Option.getOrElse []
                wordSpans <- spans
                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                do! Async.SwitchToContext uiContext
                tagsChanged.Trigger(self, SnapshotSpanEventArgs span)
            })
        |> Async.StartInThreadPoolSafe

    let docEventListener = new DocumentEventListener ([ViewChange.bufferEvent buffer], 200us, updateAtCaretPosition)

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
