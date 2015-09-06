﻿namespace FSharpVSPowerTools.Refactoring

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
open System.Threading

type ResolveUnopenedNamespaceSmartTag(actionSets) =
    inherit SmartTag(SmartTagType.Factoid, actionSets)

type UnopenedNamespaceResolver
         (textDocument: ITextDocument,
          view: ITextView, 
          textUndoHistory: ITextUndoHistory,
          vsLanguageService: VSLanguageService, 
          serviceProvider: IServiceProvider,
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

    let updateAtCaretPosition() = 
        let uiContext = SynchronizationContext.Current
        async {
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
                do! match res with
                    | Some (point, doc, project, newWord) ->
                        let wordChanged = 
                            match currentWord with
                            | None -> true
                            | Some oldWord -> newWord <> oldWord
                        if wordChanged then
                            currentWord <- Some newWord
                            suggestions <- []
                            asyncMaybe {
                                let! newWord, sym = vsLanguageService.GetSymbol (point, project)
                                // Recheck cursor position to ensure it's still in new word
                                let! point = buffer.GetSnapshotPoint view.Caret.Position
                                if not (point.InSpan newWord) then return! None
                                else
                                    let! res = 
                                        vsLanguageService.GetFSharpSymbolUse(newWord, sym, doc.FullName, project, AllowStaleResults.No) |> liftAsync
                                    
                                    match res with
                                    | Some _ -> return! None
                                    | None ->
                                        let! checkResults = 
                                            vsLanguageService.ParseFileInProject (doc.FullName, newWord.Snapshot.GetText(), project) |> liftAsync
                    
                                        let pos = codeGenService.ExtractFSharpPos point
                                        let! parseTree = checkResults.ParseTree
                                        let! entityKind = ParsedInput.getEntityKind parseTree pos
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
                                        
                                        let! idents = UntypedAstUtils.getLongIdentAt parseTree (Range.mkPos pos.Line sym.RightColumn)
                                        let createEntity = ParsedInput.tryFindInsertionContext pos.Line parseTree idents
                                        return entities |> Seq.map createEntity |> Seq.concat |> Seq.toList |> getSuggestions newWord 
                            }
                            |> Async.bind (fun result -> 
                                async {
                                    // Switch back to UI thread before firing events
                                    do! Async.SwitchToContext uiContext
                                    suggestions <- result |> Option.getOrElse []
                                    changed.Trigger self
                                })
                        else async.Return()
                        
                    | _ -> 
                        currentWord <- None 
                        async {
                            do! Async.SwitchToContext uiContext
                            changed.Trigger self }

        } |> Async.StartInThreadPoolSafe

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