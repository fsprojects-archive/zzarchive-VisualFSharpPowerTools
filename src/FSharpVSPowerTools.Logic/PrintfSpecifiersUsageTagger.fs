namespace FSharpVSPowerTools.PrintfSpecifiersHighlightUsage

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.PrintfSpecifiersUsageGetter
open Microsoft.FSharp.Compiler

type PrintfSpecifiersUsageTag() = 
    // todo change type to allow different (dark yellow?) color, same as R# uses
    inherit TextMarkerTag("MarkerFormatDefinition/HighlightedReference") 

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

/// This tagger will provide tags for every printf format specifier under the cursor.
type PrintfSpecifiersUsageTagger
    (
        textDocument: ITextDocument,
        view: ITextView, 
        vsLanguageService: VSLanguageService, 
        serviceProvider: IServiceProvider,
        projectFactory: ProjectFactory
    ) as self =
    
    let tagsChanged = Event<_, _>()
    let mutable spans: NormalizedSnapshotSpanCollection option = None
    let mutable usages: PrintfSpecifierUse[] option = None
    let buffer = view.TextBuffer

    let printfSpecifierUseToSpans (u: PrintfSpecifierUse): SnapshotSpan[] =
        [| fromFSharpRange buffer.CurrentSnapshot u.SpecifierRange
           fromFSharpRange buffer.CurrentSnapshot u.ArgumentRange |]
        |> Array.choose id
    
    let findUsagesAtPoint (point: SnapshotPoint) (usages: PrintfSpecifierUse[]): PrintfSpecifierUse option =
        let pos = 
            Range.mkPos ((point.Snapshot.GetLineNumberFromPosition point.Position) + 1) 
                        (point.Position - point.GetContainingLine().Start.Position)
        usages 
        |> Array.filter (fun x -> 
            Range.rangeContainsPos x.SpecifierRange pos 
            || Range.rangeContainsPos x.ArgumentRange pos)
        |> Array.tryHead

    let onCaretMove (CallInUIContext callInUIContext) =
        asyncMaybe {
            let! point = buffer.GetSnapshotPoint view.Caret.Position
            let! usages = usages
            let! usages = findUsagesAtPoint point usages
            let spans = printfSpecifierUseToSpans usages
            return NormalizedSnapshotSpanCollection spans
        } 
        |> Async.bind (fun x ->
            async {
                spans <- x
                return! callInUIContext <| fun _ -> tagsChanged.Trigger(self, SnapshotSpanEventArgs buffer.CurrentSnapshot.FullSpan)
            }
        )

    let onBufferChanged callInUIContext =
        asyncMaybe {
            let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
            let! doc = dte.GetCurrentDocument(textDocument.FilePath)
            let! project = projectFactory.CreateForDocument buffer doc
            try
                let! checkResults = vsLanguageService.ParseAndCheckFileInProject (doc.FullName, project)
                return! PrintfSpecifiersUsageGetter.getAll checkResults
            with e ->
                Logging.logExceptionWithContext(e, "Failed to update printf specifier usages.")
                return! None
        } 
        |> Async.bind (fun x -> 
            async {
                usages <- x
                return! onCaretMove callInUIContext
            })

    let bufferChangedEventListener = new DocumentEventListener ([ViewChange.bufferEvent buffer], 200us, onBufferChanged)

    let docEventListener = 
        new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, onCaretMove)

    let tagSpan span = TagSpan<PrintfSpecifiersUsageTag>(span, PrintfSpecifiersUsageTag()) :> ITagSpan<_>

    let getTags (targetSpans: NormalizedSnapshotSpanCollection): ITagSpan<PrintfSpecifiersUsageTag> list = 
        match spans with
        | Some spans when spans.Count > 0 -> 
            let targetSnapshot = targetSpans.[0].Snapshot
            let spans = 
                if targetSnapshot = spans.[0].Snapshot then spans
                else NormalizedSnapshotSpanCollection
                         (spans |> Seq.map (fun span -> span.TranslateTo(targetSnapshot, SpanTrackingMode.EdgeExclusive)))
            
            spans |> Seq.map tagSpan |> Seq.toList
        | _ -> []

    interface ITagger<PrintfSpecifiersUsageTag> with
        member __.GetTags spans =
            upcast (protectOrDefault (fun _ -> getTags spans) [])
        
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            dispose bufferChangedEventListener
            dispose docEventListener
