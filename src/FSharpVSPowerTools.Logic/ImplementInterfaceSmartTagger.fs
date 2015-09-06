namespace FSharpVSPowerTools.Refactoring

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell.Interop
open System
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharpVSPowerTools.CodeGeneration
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler
open System.Threading

type ImplementInterfaceSmartTag(actionSets) = 
    inherit SmartTag(SmartTagType.Factoid, actionSets)

[<NoEquality; NoComparison>]
type InterfaceState =
    { InterfaceData: InterfaceData 
      EndPosOfWith: pos option
      Tokens: FSharpTokenInfo list }

type ImplementInterface
        (textDocument: ITextDocument,
         view: ITextView, 
         editorOptionsFactory: IEditorOptionsFactoryService, 
         textUndoHistory: ITextUndoHistory,
         vsLanguageService: VSLanguageService, 
         serviceProvider: IServiceProvider,
         projectFactory: ProjectFactory,
         objectIdentifier: string,
         defaultBody: string) as self =

    let changed = Event<_>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable suggestions: ISuggestion list = []
    
    let buffer = view.TextBuffer

    let queryInterfaceState (point: SnapshotPoint) (doc: EnvDTE.Document) (project: IProjectProvider) =
        async {
            let line = point.Snapshot.GetLineNumberFromPosition point.Position
            let column = point.Position - point.GetContainingLine().Start.Position
            let source = point.Snapshot.GetText()
            let! ast = vsLanguageService.ParseFileInProject(doc.FullName, source, project)
            let pos = Pos.fromZ line column
            let data =
                ast.ParseTree
                |> Option.bind (InterfaceStubGenerator.tryFindInterfaceDeclaration pos)
                |> Option.map (fun iface -> 
                    let tokens = vsLanguageService.TokenizeLine(buffer, project.CompilerOptions, line) 
                    let endPosOfWidth =
                        tokens 
                        |> List.tryPick (fun (t: FSharpTokenInfo) ->
                                if t.CharClass = FSharpTokenCharKind.Keyword && t.LeftColumn >= column && t.TokenName = "WITH" then
                                    Some (Pos.fromZ line (t.RightColumn + 1))
                                else None)
                    { InterfaceData = iface; EndPosOfWith = endPosOfWidth; Tokens = tokens })
            return data
        }

    let hasSameStartPos (r1: range) (r2: range) = r1.Start = r2.Start

    let getLineIdent (lineStr: string) =
        lineStr.Length - lineStr.TrimStart(' ').Length

    let inferStartColumn indentSize state = 
        match InterfaceStubGenerator.getMemberNameAndRanges state.InterfaceData with
        | (_, range) :: _ ->
            let lineStr = buffer.CurrentSnapshot.GetLineFromLineNumber(range.StartLine-1).GetText()
            getLineIdent lineStr
        | [] ->
            match state.InterfaceData with
            | InterfaceData.Interface _ as iface ->
                // 'interface ISomething with' is often in a new line, we use the indentation of that line
                let lineStr = buffer.CurrentSnapshot.GetLineFromLineNumber(iface.Range.StartLine-1).GetText()
                getLineIdent lineStr + indentSize
            | InterfaceData.ObjExpr _ as iface ->
                state.Tokens 
                |> List.tryPick (fun (t: FSharpTokenInfo) ->
                            if t.CharClass = FSharpTokenCharKind.Keyword && t.TokenName = "NEW" then
                                Some (t.LeftColumn + indentSize)
                            else None)
                // There is no reference point, we indent the content at the start column of the interface
                |> Option.getOrElse iface.Range.StartColumn

    let handleImplementInterface (snapshot: ITextSnapshot) state displayContext implementedMemberSignatures entity verboseMode = 
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let startColumn = inferStartColumn indentSize state
        let typeParams = state.InterfaceData.TypeParameters
        let stub = InterfaceStubGenerator.formatInterface 
                       startColumn indentSize typeParams objectIdentifier defaultBody
                       displayContext implementedMemberSignatures entity verboseMode
        if String.IsNullOrEmpty stub then
            let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
            statusBar.SetText(Resource.interfaceFilledStatusMessage) |> ignore
        else
            use transaction = textUndoHistory.CreateTransaction(Resource.implementInterfaceCommandName)
            match state.EndPosOfWith with
            | Some pos -> 
                let currentPos = snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
                buffer.Insert(currentPos, stub + new String(' ', startColumn)) |> ignore
            | None ->
                let range = state.InterfaceData.Range
                let currentPos = snapshot.GetLineFromLineNumber(range.EndLine-1).Start.Position + range.EndColumn
                buffer.Insert(currentPos, " with") |> ignore
                buffer.Insert(currentPos + 5, stub + new String(' ', startColumn)) |> ignore
            transaction.Complete()

    let getSuggestions (word: SnapshotSpan) (state: InterfaceState, displayContext, entity, results: ParseAndCheckResults) =
        if InterfaceStubGenerator.hasNoInterfaceMember entity then []
        else
            let membersAndRanges = InterfaceStubGenerator.getMemberNameAndRanges state.InterfaceData
            let interfaceMembers = InterfaceStubGenerator.getInterfaceMembers entity
            let hasTypeCheckError = 
                match results.GetErrors() with
                | Some errors -> errors |> Array.exists (fun e -> e.Severity = FSharpErrorSeverity.Error)
                | None -> false
            // This comparison is a bit expensive
            if hasTypeCheckError && List.length membersAndRanges <> Seq.length interfaceMembers then
                let word = 
                    let currentSnapshot = buffer.CurrentSnapshot
                    if currentSnapshot = word.Snapshot then word
                    else word.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive)

                let createSuggestion name verboseMode =
                    { new ISuggestion with
                        member __.NeedsIcon = false
                        member __.Text = name
                        member __.Invoke() = 
                          let uiContext = SynchronizationContext.Current
                          async {
                              let getMemberByLocation(name, range: range) =
                                  let lineStr = 
                                      match fromFSharpRange word.Snapshot range with
                                      | Some span -> span.End.GetContainingLine().GetText()
                                      | None -> String.Empty
                                  results.GetSymbolUseAtLocation(range.EndLine, range.EndColumn, lineStr, [name])
                              let! implementedMemberSignatures =
                                InterfaceStubGenerator.getImplementedMemberSignatures getMemberByLocation displayContext state.InterfaceData
                              do! Async.SwitchToContext uiContext
                              return handleImplementInterface word.Snapshot state displayContext implementedMemberSignatures entity verboseMode
                          } |> Async.StartInThreadPoolSafe }

                [ createSuggestion Resource.implementInterfaceCommandName true
                  createSuggestion Resource.implementInterfaceLightweightCommandName false ]
            else []

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
                        let! word, symbol = vsLanguageService.GetSymbol(point, project) 
                        return point, doc, project, word, symbol
                    }
            
                match res with
                | Some (point, doc, project, newWord, symbol) ->
                    let wordChanged = 
                        match currentWord with
                        | None -> true
                        | Some oldWord -> newWord <> oldWord
                    do! if wordChanged then
                            currentWord <- Some newWord
                            suggestions <- []
                            asyncMaybe {
                                match symbol.Kind with
                                | SymbolKind.Ident ->
                                    let! interfaceState = queryInterfaceState point doc project
                                    let! (fsSymbolUse, results) = 
                                        vsLanguageService.GetFSharpSymbolUse (newWord, symbol, doc.FullName, project, AllowStaleResults.MatchingSource)
                                    // Recheck cursor position to ensure it's still in new word
                                    let! point = buffer.GetSnapshotPoint view.Caret.Position
                                    return!
                                        (match fsSymbolUse.Symbol with
                                        | :? FSharpEntity as entity when point.InSpan newWord ->
                                            // The entity might correspond to another symbol so we check for symbol text and start ranges as well
                                            if InterfaceStubGenerator.isInterface entity && entity.DisplayName = symbol.Text 
                                                && hasSameStartPos fsSymbolUse.RangeAlternate interfaceState.InterfaceData.Range then
                                                Some (interfaceState, fsSymbolUse.DisplayContext, entity, results)
                                            else None
                                        | _ -> None)
                                | _ -> return! None
                            } 
                            |> Async.bind (fun result -> 
                                async {
                                    // Switch back to UI thread before firing events
                                    do! Async.SwitchToContext uiContext
                                    suggestions <- result |> Option.map (getSuggestions newWord) |> Option.getOrElse []
                                    changed.Trigger self
                                })
                        else async.Return ()
                        
                | _ -> 
                    currentWord <- None 
                    return! async {
                        do! Async.SwitchToContext uiContext
                        changed.Trigger self }
        } |> Async.StartInThreadPoolSafe

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    500us, updateAtCaretPosition)

    member __.Changed = changed.Publish
    member __.CurrentWord = 
        currentWord |> Option.map (fun word ->
            if buffer.CurrentSnapshot = word.Snapshot then word
            else word.TranslateTo(buffer.CurrentSnapshot, SpanTrackingMode.EdgeExclusive))
    member __.Suggestions = suggestions

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()

type ImplementInterfaceSmartTagger(buffer: ITextBuffer, implementInterface: ImplementInterface) =
    let tagsChanged = Event<_, _>()
    do implementInterface.Changed.Add (fun x -> buffer.TriggerTagsChanged x tagsChanged)
    interface ITagger<ImplementInterfaceSmartTag> with
        member __.GetTags _: ITagSpan<ImplementInterfaceSmartTag> seq =
            protectOrDefault (fun _ ->
                seq {
                    match implementInterface.CurrentWord, implementInterface.Suggestions with
                    | None, _
                    | _, [] -> ()
                    | Some word, suggestions ->
                        let actions =
                            suggestions
                            |> List.map (fun s ->
                                 { new ISmartTagAction with
                                     member __.ActionSets = null
                                     member __.DisplayText = s.Text
                                     member __.Icon = null
                                     member __.IsEnabled = true
                                     member __.Invoke() = s.Invoke() })
                            |> Seq.toReadOnlyCollection
                            |> fun xs -> [ SmartTagActionSet xs ]
                            |> Seq.toReadOnlyCollection

                        yield TagSpan<_>(word, ImplementInterfaceSmartTag actions) :> ITagSpan<_>
                }) Seq.empty

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (implementInterface :> IDisposable).Dispose()