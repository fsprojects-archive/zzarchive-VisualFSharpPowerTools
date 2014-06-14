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
        let doRange (range: range) = if range.StartLine < pos.Line then result := Some range

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) = 
            List.iter walkSynModuleOrNamespace moduleOrNamespaceList

        and walkSynModuleOrNamespace(SynModuleOrNamespace(_, _, decls, _, _, _, range)) =
            doRange range
            List.iter walkSynModuleDecl decls

        and walkSynModuleDecl(decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(_, modules, _, _) -> List.iter walkSynModuleDecl modules
            | SynModuleDecl.Open (_, range) -> doRange range
            | _ -> ()

        match ast with
        | ParsedInput.SigFile _input -> ()
        | ParsedInput.ImplFile input -> walkImplFileInput input
        !result |> Option.map (fun res -> Pos.fromZ (res.StartLine - 1) (res.StartColumn + 1))
         
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
                                let! entities = vsLanguageService.GetAllEntities project
                                let entitiesNames =
                                    entities 
                                    |> Seq.map (fun e -> try e.FullName with _ -> e.DisplayName)
                                    |> Seq.choose (fun name -> 
                                        match name.LastIndexOf '.' with
                                        | -1 -> 
                                            if name = sym.Text then Some { Namespace = ""; Name = name } else None
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
        let line = snapshotSpan.Snapshot.GetLineFromLineNumber(line).Start.Position + col
        //buffer.Insert(currentLine, stub) |> ignore
        snapshotSpan.Snapshot.TextBuffer.Insert (line, "open " + ns) |> ignore
        transaction.Complete()

    let fullyQualifySymbol (snapshotSpan: SnapshotSpan) fullSymbolName = 
        use transaction = textUndoHistory.CreateTransaction(Resource.recordGenerationCommandName)
        //let currentLine = snapshot.GetLineFromLineNumber(insertionPos.InsertionPos.Line-1).Start.Position + insertionPos.InsertionPos.Column
        //buffer.Insert(currentLine, stub) |> ignore
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