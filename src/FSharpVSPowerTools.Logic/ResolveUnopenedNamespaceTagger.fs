namespace FSharpVSPowerTools.Refactoring

open System.Collections.Generic
open System.Windows
open System.Windows.Threading
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.CodeGeneration.RecordStubGenerator
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type ResolveUnopenedNamespaceSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type ResolveUnopenedNamespaceSmartTagger
         (view: ITextView, buffer: ITextBuffer, textUndoHistory: ITextUndoHistory,
          vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider,
          projectFactory: ProjectFactory) as self =
          
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable state: (Symbol * string list) option = None

    let updateAtCaretPosition() =
        match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
        | Some point, Some word when word.Snapshot = view.TextSnapshot && point.InSpan word -> ()
        | _ ->
            let res =
                maybe {
                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                    let! doc = dte.GetActiveDocument()
                    let! project = projectFactory.CreateForDocument buffer doc
                    let! word, _ = vsLanguageService.GetSymbol(point, project) 
                    return point, doc, project, word
                }
            match res with
            | Some (point, doc, project, newWord) ->
                let wordChanged = 
                    match currentWord with
                    | None -> true
                    | Some oldWord -> newWord <> oldWord
                if wordChanged then
                    asyncMaybe {
                        let! newWord, sym = vsLanguageService.GetSymbol (point, project) |> liftMaybe
                        // Recheck cursor position to ensure it's still in new word
                        let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
                        if not (point.InSpan newWord) then return! liftMaybe None
                        else
                            let! res = 
                                vsLanguageService.GetFSharpSymbolUse(newWord, sym, doc.FullName, project, AllowStaleResults.No) |> liftAsync
                            
                            match res with 
                            | Some _ -> return! liftMaybe None
                            | None ->
                                let! entities = vsLanguageService.GetAllEntities project
                                let entitiesNames =
                                    entities 
                                    |> Seq.map (fun e -> try e.FullName with _ -> e.DisplayName)
                                    |> Seq.filter (fun name -> name.EndsWith sym.Text)
                                    //|> Seq.truncate 5
                                    |> Seq.toList

                                return! Some (sym, entitiesNames) |> liftMaybe
                    }
                    |> Async.map (fun result -> 
                        state <- result
                        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                        tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
                    |> Async.StartImmediateSafe
                    currentWord <- Some newWord
            | _ -> ()

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    500us, updateAtCaretPosition)

    let openNamespace (_snapshot: ITextSnapshot) _ns = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        //let currentLine = snapshot.GetLineFromLineNumber(insertionPos.InsertionPos.Line-1).Start.Position + insertionPos.InsertionPos.Column
        //buffer.Insert(currentLine, stub) |> ignore
        transaction.Complete()

    let openNamespaceAction snapshot ns =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = ns
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = openNamespace snapshot ns
        }

    member x.GetSmartTagActions(snapshot, candidates, _symbol) =
        let actionList = 
            candidates 
            |> List.map (fun ns -> sprintf "open %s" ns) 
            |> List.map (openNamespaceAction snapshot)
            |> Seq.toReadOnlyCollection

        [ SmartTagActionSet(actionList) ] |> Seq.toReadOnlyCollection

    interface ITagger<ResolveUnopenedNamespaceSmartTag> with
        member x.GetTags _spans =
            seq {
                match currentWord, state with
                | Some word, Some (symbol, candidates) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, ResolveUnopenedNamespaceSmartTag(x.GetSmartTagActions(word.Snapshot, candidates, symbol)))
                          :> _
                | _ -> ()
            }

        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish