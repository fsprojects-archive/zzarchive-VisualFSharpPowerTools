namespace FSharpVSPowerTools.Refactoring

open System
open System.IO
open System.Windows
open System.Windows.Threading
open System.Collections.Generic
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell.Interop
open FSharpVSPowerTools
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open FSharp.CompilerBinding
open Microsoft.FSharp.Compiler.SourceCodeServices

type ImplementInterfaceSmartTag(actionSets) = 
    inherit SmartTag(SmartTagType.Factoid, actionSets)

/// This tagger will provide tags for every word in the buffer that
/// matches the word currently under the cursor.
type ImplementInterfaceSmartTagger(view: ITextView, buffer: ITextBuffer, 
                                   editorOptionsFactory: IEditorOptionsFactoryService, textUndoHistory: ITextUndoHistory,
                                   vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider) as self =
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable interfaceDefinition = None

    let updateAtCaretPosition() =
        match buffer.GetSnapshotPoint view.Caret.Position with
        | Some point ->
            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            match currentWord with
            | Some cw when cw.Snapshot = view.TextSnapshot && point.InSpan cw -> ()
            | _ ->
                maybe {
                    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                    let! doc = dte.GetActiveDocument()
                    let! project = ProjectProvider.createForDocument doc
                    let! newWord, symbol = vsLanguageService.GetSymbol(point, project)
                    // If this is the same word we currently have, we're done (e.g. caret moved within a word).
                    match currentWord with
                    | Some cw when cw = newWord -> ()
                    | _ ->
                        let results = 
                            vsLanguageService.GetFSharpSymbol (newWord, symbol, doc.FullName, project, AllowStaleResults.MatchingSource)
                            |> Async.RunSynchronously
                        match results with
                        | Some (fsSymbol, _) when (fsSymbol :? FSharpEntity) ->
                            let e = fsSymbol :?> FSharpEntity
                            if e.IsInterface then
                                interfaceDefinition <- Some e
                                currentWord <- Some newWord
                                let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                                return tagsChanged.Trigger(self, SnapshotSpanEventArgs(span))
                            else
                                interfaceDefinition <- None
                        | _ ->
                            interfaceDefinition <- None
                }
                |> ignore
        | _ -> ()

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    TimeSpan.FromMilliseconds 200.,
                                    fun _ -> updateAtCaretPosition())

    let implementInterface (span: SnapshotSpan) entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = "Explicitly Implement Interface"
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = 
                let line = span.Start.GetContainingLine()
                let lineStr = line.GetText()
                let editorOptions = editorOptionsFactory.GetOptions(buffer)
                let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
                let startColumn = lineStr.Length - lineStr.TrimStart(' ').Length + indentSize
                let stub = InterfaceStubGenerator.formatInterface 
                                startColumn indentSize "x" "raise (System.NotImplementedException())" entity
                use transaction = textUndoHistory.CreateTransaction("Explicitly Implement Interface")
                buffer.Insert(line.End.Position, stub) |> ignore
                transaction.Complete() }

    member x.GetSmartTagActions(span: SnapshotSpan, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        let trackingSpan = span.Snapshot.CreateTrackingSpan(span.Span, SpanTrackingMode.EdgeInclusive)
        let _snapshot = trackingSpan.TextBuffer.CurrentSnapshot

        actionList.Add(implementInterface span entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<ImplementInterfaceSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<ImplementInterfaceSmartTag> seq =
            seq {
                match currentWord, interfaceDefinition with
                | Some word, Some entity ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<ImplementInterfaceSmartTag>(span, ImplementInterfaceSmartTag(x.GetSmartTagActions(span, entity)))
                          :> ITagSpan<_>
                | _ -> ()
            }

        member x.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member x.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)


