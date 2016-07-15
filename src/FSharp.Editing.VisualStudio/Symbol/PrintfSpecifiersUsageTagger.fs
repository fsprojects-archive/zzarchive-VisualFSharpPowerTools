namespace FSharp.Editing.VisualStudio.Symbol

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open FSharp.Editing
open FSharp.Editing.VisualStudio
open FSharp.Editing.Features.PrintfSpecifiersUsageGetter
open Microsoft.FSharp.Compiler
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.Editing.Features

type PrintfSpecifiersUsageTag() = 
    inherit TextMarkerTag(Constants.fsharpPrintfTagType) 

// Reference at http://msdn.microsoft.com/en-us/library/vstudio/dd885121.aspx

/// This tagger will provide tags for every printf format specifier under the cursor.
type PrintfSpecifiersUsageTagger
    (
        doc: ITextDocument,
        view: ITextView, 
        vsLanguageService: VSLanguageService, 
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

    let onCaretMoveListener = 
        lazy (new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 200us, onCaretMove))

    let project() = projectFactory.CreateForDocument buffer doc.FilePath

    let onBufferChanged ((CallInUIContext callInUIContext) as ciuc) =
        asyncMaybe {
            let! project = project()
            try
                let! checkResults = vsLanguageService.ParseAndCheckFileInProject (doc.FilePath, project, AllowStaleResults.MatchingSource)
                return! PrintfSpecifiersUsageGetter.getAll checkResults (fun e -> Logging.logError (fun _ -> e))
            with e ->
                Logging.logExceptionWithContext(e, "Failed to update printf specifier usages.")
                return! None
        } 
        |> Async.bind (fun x -> 
            async {
                usages <- x
                return! 
                    if onCaretMoveListener.IsValueCreated then
                        onCaretMove ciuc
                    else 
                        callInUIContext <| fun _ -> 
                            onCaretMoveListener.Force() |> ignore
            })

    let bufferChangedEventListener = new DocumentEventListener ([ViewChange.bufferEvent buffer], 100us, onBufferChanged)
    
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
            if onCaretMoveListener.IsValueCreated then 
                dispose onCaretMoveListener.Value
