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
open Microsoft.FSharp.Compiler

type ResolveUnopenedNamespaceSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type ResolveUnopenedNamespaceSmartTagger
         (textDocument: ITextDocument,
          view: ITextView, 
          textUndoHistory: ITextUndoHistory,
          vsLanguageService: VSLanguageService, 
          serviceProvider: IServiceProvider,
          projectFactory: ProjectFactory) as self =
    
    let buffer = view.TextBuffer
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable state: (Entity * InsertContext) list option = None 

    let updateAtCaretPosition() =
        match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
        | Some point, Some word when word.Snapshot = view.TextSnapshot && point.InSpan word -> ()
        | _ ->
            let res =
                maybe {
                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                    let dte = serviceProvider.GetService<EnvDTE.DTE, SDTE>()
                    let! doc = dte.GetCurrentDocument(textDocument.FilePath)
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
                    currentWord <- Some newWord
                    state <- None
                    //let ctx = System.Threading.SynchronizationContext.Current
                    asyncMaybe {
                        let! newWord, sym = vsLanguageService.GetSymbol (point, project) |> liftMaybe
                        // Recheck cursor position to ensure it's still in new word
                        let! point = buffer.GetSnapshotPoint view.Caret.Position |> liftMaybe
                        if not (point.InSpan newWord) then return! liftMaybe None
                        else
                            //do! Async.SwitchToThreadPool() |> liftAsync
                            let! res = 
                                vsLanguageService.GetFSharpSymbolUse(newWord, sym, doc.FullName, project, AllowStaleResults.No) |> liftAsync
                            
                            match res with
                            | Some _ -> return! liftMaybe None
                            | None ->
                                let! checkResults = 
                                    vsLanguageService.ParseFileInProject (doc.FullName, newWord.Snapshot.GetText(), project) |> liftAsync

                                let pos = codeGenService.ExtractFSharpPos point
                                let! parseTree = liftMaybe checkResults.ParseTree
                                
                                let! entityKind = ParsedInput.getEntityKind parseTree pos |> liftMaybe
                                let! entities = vsLanguageService.GetAllEntities (doc.FullName, newWord.Snapshot.GetText(), project)

                                //entities |> Seq.map string |> fun es -> System.IO.File.WriteAllLines (@"l:\entities.txt", es)

                                let isAttribute = entityKind = EntityKind.Attribute
                                let entities =
                                    entities |> List.filter (fun e ->
                                        match entityKind, e.Kind with
                                        | EntityKind.Attribute, EntityKind.Attribute 
                                        | EntityKind.Type, (EntityKind.Type | EntityKind.Attribute)
                                        | EntityKind.FunctionOrValue _, _ -> true 
                                        | EntityKind.Attribute, _
                                        | _, EntityKind.Module _
                                        | EntityKind.Module _, _
                                        | EntityKind.Type, _ -> false)

                                let entities = 
                                    entities
                                    |> List.map (fun e -> 
                                         [ yield e.TopRequireQualifiedAccessParent, e.AutoOpenParent, e.Namespace, e.CleanedIdents
                                           if isAttribute then
                                               let lastIdent = e.CleanedIdents.[e.CleanedIdents.Length - 1]
                                               if e.Kind = EntityKind.Attribute && lastIdent.EndsWith "Attribute" then
                                                   yield 
                                                       e.TopRequireQualifiedAccessParent, 
                                                       e.AutoOpenParent,
                                                       e.Namespace,
                                                       e.CleanedIdents 
                                                       |> Array.replace (e.CleanedIdents.Length - 1) (lastIdent.Substring(0, lastIdent.Length - 9)) ])
                                    |> List.concat

                                debug "[ResolveUnopenedNamespaceSmartTagger] %d entities found" (List.length entities)
                                
                                let! idents = UntypedAstUtils.getLongIdentAt parseTree (Range.mkPos pos.Line sym.RightColumn) |> liftMaybe
                                let createEntity = ParsedInput.tryFindInsertionContext pos.Line parseTree idents
                                return entities |> Seq.map createEntity |> Seq.concat |> Seq.toList
                    }
                    |> Async.map (fun result -> 
                         state <- result
                         buffer.TriggerTagsChanged self tagsChanged)
                    |> Async.StartImmediateSafe
                    
            | _ -> 
                currentWord <- None 
                buffer.TriggerTagsChanged self tagsChanged

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                                      100us, updateAtCaretPosition)

    let openNamespace (snapshotSpan: SnapshotSpan) (ctx: InsertContext) ns name = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        // first, replace the symbol with (potentially) partially qualified name
        let snapshot = 
            if name <> "" then snapshotSpan.Snapshot.TextBuffer.Replace (snapshotSpan.Span, name)
            else snapshotSpan.Snapshot
        
        let doc =
            { new IInsertContextDocument<ITextSnapshot> with
                  member __.Insert (snapshot, line, lineStr) = 
                    let pos = snapshot.GetLineFromLineNumber(line).Start.Position
                    snapshot.TextBuffer.Insert (pos, lineStr + Environment.NewLine)
                  member __.GetLineStr (snapshot, line) = snapshot.GetLineFromLineNumber(line).GetText() }
        
        InsertContext.insertOpenDeclaration snapshot doc ctx ns |> ignore
        transaction.Complete()

    let replaceFullyQualifiedSymbol (snapshotSpan: SnapshotSpan) qualifier = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        snapshotSpan.Snapshot.TextBuffer.Replace (snapshotSpan.Span, qualifier) |> ignore
        transaction.Complete()

    let openNamespaceIcon = ResourceProvider.getRefactoringIcon serviceProvider RefactoringIconKind.AddUsing

    let openNamespaceAction snapshot ctx name ns multipleNames =
        let displayText = "open " + ns + if multipleNames then " (" + name + ")" else ""

        { new ISmartTagAction with
            member __.ActionSets = null
            member __.DisplayText = displayText
            member __.Icon = openNamespaceIcon
            member __.IsEnabled = true
            member __.Invoke() = openNamespace snapshot ctx ns name
        }

    let qualifiedSymbolAction snapshotSpan (fullName, qualifier) =
        { new ISmartTagAction with
            member __.ActionSets = null
            member __.DisplayText = fullName
            member __.Icon = null
            member __.IsEnabled = true
            member __.Invoke() = replaceFullyQualifiedSymbol snapshotSpan qualifier
        }

    let getSmartTagActions snapshotSpan candidates =
        let openNamespaceActions = 
            candidates
            |> Seq.choose (fun (entity, ctx) -> entity.Namespace |> Option.map (fun ns -> ns, entity.Name, ctx))
            |> Seq.groupBy (fun (ns, _, _) -> ns)
            |> Seq.map (fun (ns, xs) -> 
                ns, 
                xs 
                |> Seq.map (fun (_, name, ctx) -> name, ctx) 
                |> Seq.distinctBy (fun (name, _) -> name)
                |> Seq.sortBy fst
                |> Seq.toArray)
            |> Seq.map (fun (ns, names) ->
                let multipleNames = names |> Array.length > 1
                names |> Seq.map (fun (name, ctx) -> ns, name, ctx, multipleNames))
            |> Seq.concat
            |> Seq.map (fun (ns, name, ctx, multipleNames) -> 
                openNamespaceAction snapshotSpan ctx name ns multipleNames)
            
        let qualifySymbolActions =
            candidates
            |> Seq.map (fun (entity, _) -> entity.FullRelativeName, entity.Qualifier)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.map (qualifiedSymbolAction snapshotSpan)
            
        [ SmartTagActionSet (Seq.toReadOnlyCollection openNamespaceActions)
          SmartTagActionSet (Seq.toReadOnlyCollection qualifySymbolActions) ] 
        |> Seq.toReadOnlyCollection

    interface ITagger<ResolveUnopenedNamespaceSmartTag> with
        member __.GetTags _ =
            seq {
                match currentWord, state with
                | Some word, Some candidates ->
                    let span =
                        if buffer.CurrentSnapshot = word.Snapshot then word
                        else word.TranslateTo(buffer.CurrentSnapshot, SpanTrackingMode.EdgeExclusive)

                    yield TagSpan<_>(span, ResolveUnopenedNamespaceSmartTag(getSmartTagActions word candidates)) :> _
                | _ -> ()
            }
             
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()