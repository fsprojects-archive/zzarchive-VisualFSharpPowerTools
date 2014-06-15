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

type Entity = 
    { Namespace: string
      Name: string }

module Ast =
    let findNearestOpenStatementBlock (pos: pos) (ast: ParsedInput) = 
        let result = ref None
        
        let doRange (syn: obj) (line, col) = 
            if line < pos.Line then 
                match !result with
                | None -> result := Some (syn, line, col)
                | Some (_, oldLine, _) when oldLine < line -> 
                    result := Some (syn, line, col)
                | _ -> ()

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.iter walkSynModuleOrNamespace moduleOrNamespaceList

        and walkSynModuleOrNamespace(SynModuleOrNamespace(_, _, decls, _, _, _, range) as syn) =
            doRange syn (range.StartLine, range.StartColumn)
            List.iter walkSynModuleDecl decls

        and walkSynModuleDecl(decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, _, _, _, _, range), modules, _, _) as syn ->
                doRange syn (range.StartLine, range.StartColumn + 4)
                List.iter walkSynModuleDecl modules
            | SynModuleDecl.Open (_, range) as syn -> doRange syn (range.StartLine, range.StartColumn - 5)
            | _ -> ()

        match ast with
        | ParsedInput.SigFile _input -> ()
        | ParsedInput.ImplFile input -> walkImplFileInput input

        let res = !result |> Option.map (fun (_, line, col) -> Pos.fromZ line col)
        //debug "[ResolveUnopenedNamespaceSmartTagger] Syn, line, col = %A, AST = %A" (!result) ast
        res 
         
type ResolveUnopenedNamespaceSmartTagger
         (view: ITextView, buffer: ITextBuffer, textUndoHistory: ITextUndoHistory,
          vsLanguageService: VSLanguageService, serviceProvider: IServiceProvider,
          projectFactory: ProjectFactory) as self =
    
    let codeGenService: ICodeGenerationService<_, _, _> = upcast CodeGenerationService(vsLanguageService, buffer)
          
    let tagsChanged = Event<_, _>()
    let mutable currentWord: SnapshotSpan option = None
    let mutable state: (Symbol * Entity list * int * int) option = None

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
                                let entitiesNames =
                                    entities 
                                    |> Seq.map (fun e ->
                                        try e.FullName 
                                        with _ -> e.DisplayName)
                                    |> Seq.choose (fun name -> 
                                        match name.LastIndexOf '.' with
                                        | -1 -> None
                                        | lastDotIndex -> 
                                            let lastIdent = name.Substring (lastDotIndex + 1)
                                            if lastIdent = sym.Text then 
                                                Some { Namespace = name.Substring (0, lastDotIndex); Name = lastIdent }
                                            else None)
                                    |> Seq.toList

                                let! checkResults = 
                                    vsLanguageService.ParseFileInProject (doc.FullName, newWord.Snapshot.GetText(), project) |> liftAsync

                                return! 
                                    checkResults.ParseTree 
                                    |> Option.map (fun tree ->
                                        let line, column = 
                                            match Ast.findNearestOpenStatementBlock (codeGenService.ExtractFSharpPos point) tree with
                                            | Some pos -> pos.Line, pos.Column
                                            | None -> 1, 1
                                        sym, entitiesNames, line, column)
                                    |> liftMaybe
                    }
                    |> Async.map (fun result -> 
                        state <- result
                        let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                        tagsChanged.Trigger(self, SnapshotSpanEventArgs(span)))
                    |> Async.StartImmediateSafe
                    currentWord <- Some newWord
            | _ -> ()

    let docEventListener = new DocumentEventListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                                      500us, updateAtCaretPosition)

    let openNamespace (snapshotSpan: SnapshotSpan) line col ns = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        let line' = snapshotSpan.Snapshot.GetLineFromLineNumber(line - 1).Start.Position
        let snapshot = snapshotSpan.Snapshot.TextBuffer.Insert (line', (String.replicate col " ") + "open " + ns + Environment.NewLine)
        let nextLine = snapshot.GetLineFromLineNumber line
        // if there's not blank line between open declaration block and the rest of the code, we add one
        let snapshot = 
            if nextLine.GetText().Trim() <> "" then
                snapshot.TextBuffer.Insert(nextLine.Start.Position, Environment.NewLine)
            else snapshot
        // for top level module we add a blank line between the module declaration and first open statement
        if col = 0 then
            let prevLine = snapshot.GetLineFromLineNumber (line - 2)
            if not (prevLine.GetText().Trim().StartsWith "open") then
                snapshot.TextBuffer.Insert(prevLine.End.Position, Environment.NewLine) |> ignore
        transaction.Complete()

    let fullyQualifySymbol (snapshotSpan: SnapshotSpan) fullSymbolName = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        snapshotSpan.Snapshot.TextBuffer.Replace (snapshotSpan.Span, fullSymbolName) |> ignore
        transaction.Complete()

    let openNamespaceAction snapshot line col ns =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = "open " + ns
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = openNamespace snapshot line col ns
        }

    let qualifySymbolAction snapshotSpan fullSymbolName =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = fullSymbolName
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = fullyQualifySymbol snapshotSpan fullSymbolName
        }

    let getSmartTagActions snapshotSpan candidates _symbol line col =
        let openNamespaceActions = 
            candidates
            |> List.map (fun e -> e.Namespace) 
            |> List.map (openNamespaceAction snapshotSpan line col)
            
        let qualifySymbolActions =
            candidates
            |> List.map (fun e -> 
                match e.Namespace with
                | "" -> e.Name
                | ns -> ns + "." + e.Name)
            |> List.map (qualifySymbolAction snapshotSpan)
            
        let actions = openNamespaceActions @ qualifySymbolActions |> Seq.toReadOnlyCollection

        [ SmartTagActionSet(actions) ] |> Seq.toReadOnlyCollection

    interface ITagger<ResolveUnopenedNamespaceSmartTag> with
        member x.GetTags _spans =
            seq {
                match currentWord, state with
                | Some word, Some (symbol, candidates, line, col) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<_>(span, ResolveUnopenedNamespaceSmartTag(getSmartTagActions word candidates symbol line col))
                          :> _
                | _ -> ()
            }
             
        [<CLIEvent>]
        member x.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member x.Dispose() = 
            (docEventListener :> IDisposable).Dispose()