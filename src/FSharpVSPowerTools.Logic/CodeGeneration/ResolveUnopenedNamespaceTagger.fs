namespace FSharpVSPowerTools.Refactoring

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open System
open FSharpPowerTools.Core.Infrastructure
open FSharpVSPowerTools
open FSharpVSPowerTools.CodeGeneration
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler

type ResolveUnopenedNamespaceSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type UnopenedNamespaceResolver
         (doc: ITextDocument,
          view: ITextView, 
          textUndoHistory: ITextUndoHistory,
          vsLanguageService: VSLanguageService, 
          projectFactory: ProjectFactory) as self =
    
    let buffer = view.TextBuffer
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)

    let changed = Event<_>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable suggestions: SuggestionGroup list = [] 
    
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

    let fixUnderscoresInMenuText (text: string) = text.Replace("_", "__")

    let openNamespaceAction snapshot ctx name ns multipleNames = 
        let displayText = "open " + ns + if multipleNames then " (" + name + ")" else ""

        { new ISuggestion with
            member __.Text = fixUnderscoresInMenuText displayText
            member __.Invoke() = openNamespace snapshot ctx ns name
            member __.NeedsIcon = true }

    let qualifiedSymbolAction snapshotSpan (fullName, qualifier) =
        { new ISuggestion with
            member __.Text = fixUnderscoresInMenuText fullName
            member __.Invoke() = replaceFullyQualifiedSymbol snapshotSpan qualifier
            member __.NeedsIcon = false }

    let getSuggestions (snapshotSpan: SnapshotSpan) (candidates: (Entity * InsertContext) list) : SuggestionGroup list =
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
            |> Seq.toList
            
        let qualifySymbolActions =
            candidates
            |> Seq.map (fun (entity, _) -> entity.FullRelativeName, entity.Qualifier)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.map (qualifiedSymbolAction snapshotSpan)
            |> Seq.toList
            
        match openNamespaceActions, qualifySymbolActions with
        | [], [] -> []
        | _ -> [ openNamespaceActions; qualifySymbolActions ]

    let project() = projectFactory.CreateForDocument buffer doc.FilePath

    let updateAtCaretPosition (CallInUIContext callInUIContext) =
        async {
            match buffer.GetSnapshotPoint view.Caret.Position, currentWord with
            | Some point, Some word when word.Snapshot = view.TextSnapshot && point.InSpan word -> return ()
            | _ ->
                let! result = asyncMaybe {
                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                    let! project = project()
                    let newWordAndSym = vsLanguageService.GetSymbol (point, doc.FilePath, project)
                    let newWord = newWordAndSym |> Option.map fst
                    let oldWord = currentWord
                    currentWord <- newWord
                    let! newWord = newWord
                    let! sym = newWordAndSym |> Option.map snd
                    
                    do! match oldWord, currentWord with
                        | _, None -> None 
                        | None, _ ->  Some()
                        | Some oldWord, Some newWord -> 
                            if oldWord <> newWord then Some()
                            else None
                    
                    let! res = vsLanguageService.GetFSharpSymbolUse(newWord, sym, doc.FilePath, project, AllowStaleResults.No) |> liftAsync
                    
                    match res with
                    | Some _ -> return! None
                    | None ->
                        let! checkResults = vsLanguageService.ParseFileInProject (doc.FilePath, project)
                        let pos = codeGenService.ExtractFSharpPos point
                        let! parseTree = checkResults.ParseTree
                        let! entityKind = ParsedInput.getEntityKind parseTree pos
                        let! entities = vsLanguageService.GetAllEntities (doc.FilePath, project)

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
                        
                        let! idents = UntypedAstUtils.getLongIdentAt parseTree (Range.mkPos pos.Line sym.RightColumn)
                        let createEntity = ParsedInput.tryFindInsertionContext pos.Line parseTree idents
                        return entities |> Seq.map createEntity |> Seq.concat |> Seq.toList |> getSuggestions newWord 
                } 
                suggestions <- result |> Option.getOrElse []
                do! callInUIContext <| fun _ -> changed.Trigger self
        }

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                                      100us, updateAtCaretPosition)

    member __.Updated = changed.Publish
    member __.CurrentWord = 
        currentWord |> Option.map (fun word ->
            if buffer.CurrentSnapshot = word.Snapshot then word
            else word.TranslateTo(buffer.CurrentSnapshot, SpanTrackingMode.EdgeExclusive))
    member __.Suggestions = suggestions

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()

type ResolveUnopenedNamespaceSmartTagger(buffer: ITextBuffer, serviceProvider: IServiceProvider, 
                                         resolver: UnopenedNamespaceResolver) as self =
    let tagsChanged = Event<_, _>()
    let openNamespaceIcon = ResourceProvider.getRefactoringIcon serviceProvider RefactoringIconKind.AddUsing
    do resolver.Updated.Add (fun _ -> buffer.TriggerTagsChanged self tagsChanged)

    interface ITagger<ResolveUnopenedNamespaceSmartTag> with
        member __.GetTags _ =
            protectOrDefault (fun _ ->
                seq {
                    match resolver.CurrentWord, resolver.Suggestions with
                    | None, _ 
                    | _, [] -> ()
                    | Some word, suggestions ->
                        let actions =
                            suggestions
                            |> List.map (fun xs ->
                                xs 
                                |> List.map (fun s ->
                                    { new ISmartTagAction with
                                        member __.ActionSets = null
                                        member __.DisplayText = s.Text
                                        member __.Icon = if s.NeedsIcon then openNamespaceIcon else null
                                        member __.IsEnabled = true
                                        member __.Invoke() = s.Invoke() })
                                |> Seq.toReadOnlyCollection
                                |> fun xs -> SmartTagActionSet xs)
                            |> Seq.toReadOnlyCollection

                        yield TagSpan<_>(word, ResolveUnopenedNamespaceSmartTag actions) :> _
                })
                Seq.empty
             
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = (resolver :> IDisposable).Dispose()