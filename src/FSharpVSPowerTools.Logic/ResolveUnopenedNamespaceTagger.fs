namespace FSharpVSPowerTools.Refactoring

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell.Interop
open System
open FSharpVSPowerTools
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem

type ResolveUnopenedNamespaceSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type ResolveUnopenedNamespaceSmartTagger
         (view: ITextView, buffer: ITextBuffer, textUndoHistory: ITextUndoHistory,
          vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider,
          projectFactory: ProjectFactory) as self =
    
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)
          
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable state: (Entity list * Pos) option = None 

    let triggerTagsChanged() =
        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
        tagsChanged.Trigger(self, SnapshotSpanEventArgs(span))

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
                                let! entities = vsLanguageService.GetAllEntities (doc.FullName, newWord.Snapshot.GetText(), project)
                                
                                let! checkResults = 
                                    vsLanguageService.ParseFileInProject (doc.FullName, newWord.Snapshot.GetText(), project) |> liftAsync

                                return! 
                                    checkResults.ParseTree 
                                    |> Option.map (fun tree ->
                                        let currentNs, pos = 
                                            match Ast.findNearestOpenStatementBlock (codeGenService.ExtractFSharpPos point).Line tree with
                                            | Some (ns, pos) -> ns, pos
                                            | None -> None, { Line = 1; Col = 1 }

                                        entities |> List.choose (Entity.tryCreate currentNs sym.Text), pos) 
                                    |> liftMaybe 
                    }
                    |> Async.map (fun result -> 
                        state <- result
                        triggerTagsChanged())
                    |> Async.StartImmediateSafe
                    currentWord <- Some newWord
            | _ -> 
                currentWord <- None 
                triggerTagsChanged()

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                                      500us, updateAtCaretPosition)

    let openNamespace (snapshotSpan: SnapshotSpan) pos ns = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        let line = snapshotSpan.Snapshot.GetLineFromLineNumber(pos.Line - 1).Start.Position
        let snapshot = snapshotSpan.Snapshot.TextBuffer.Insert (line, (String.replicate pos.Col " ") + "open " + ns + Environment.NewLine)
        let nextLine = snapshot.GetLineFromLineNumber pos.Line
        // if there's not blank line between open declaration block and the rest of the code, we add one
        let snapshot = 
            if nextLine.GetText().Trim() <> "" then 
                snapshot.TextBuffer.Insert(nextLine.Start.Position, Environment.NewLine)
            else snapshot
        // for top level module we add a blank line between the module declaration and first open statement
        if pos.Col = 0 then
            let prevLine = snapshot.GetLineFromLineNumber (pos.Line - 2)
            if not (prevLine.GetText().Trim().StartsWith "open") then
                snapshot.TextBuffer.Insert(prevLine.End.Position, Environment.NewLine) |> ignore
        transaction.Complete()

    let fullyQualifySymbol (snapshotSpan: SnapshotSpan) fullSymbolName = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        snapshotSpan.Snapshot.TextBuffer.Replace (snapshotSpan.Span, fullSymbolName) |> ignore
        transaction.Complete()

    let openNamespaceAction snapshot pos ns =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = "open " + ns
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = openNamespace snapshot pos ns
        }

    let qualifySymbolAction snapshotSpan fullSymbolName =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = fullSymbolName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = fullyQualifySymbol snapshotSpan fullSymbolName
        }

    let getSmartTagActions snapshotSpan candidates pos =
        let openNamespaceActions = 
            candidates
            |> List.map (fun e -> openNamespaceAction snapshotSpan pos e.Namespace)
            
        let qualifySymbolActions =
            candidates
            |> List.map (fun e -> e.Namespace + "." + e.Name)
            |> List.map (qualifySymbolAction snapshotSpan)
            
        let actions = openNamespaceActions @ qualifySymbolActions |> Seq.toReadOnlyCollection

        [ SmartTagActionSet(actions) ] |> Seq.toReadOnlyCollection

    interface ITagger<ResolveUnopenedNamespaceSmartTag> with
        member x.GetTags _spans =
            seq {
                match currentWord, state with
                | Some word, Some (candidates, pos) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, ResolveUnopenedNamespaceSmartTag(getSmartTagActions word candidates pos))
                          :> _
                | _ -> ()
            }
             
        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member x.Dispose() = 
            (docEventListener :> IDisposable).Dispose()