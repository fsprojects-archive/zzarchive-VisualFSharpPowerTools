namespace FSharpVSPowerTools.Refactoring

open System.Windows
open System.Windows.Threading
open System.Collections.Generic
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open System
open System.IO
open FSharpVSPowerTools.Core
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
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

    let collectInterfaceData (point: SnapshotPoint) (doc: EnvDTE.Document) (project: IProjectProvider) =
        let line = point.Snapshot.GetLineNumberFromPosition point.Position
        let column = point.Position - point.GetContainingLine().Start.Position
        let lineStr = point.GetContainingLine().GetText()
        let source = point.Snapshot.GetText()
        let defines = 
            project.CompilerOptions
            |> Seq.choose (fun s -> if s.StartsWith "--define:" then Some s.[9..] else None)
            |> Seq.toList
        let tokens = vsLanguageService.TokenizeLine(buffer, defines, line) 
        let endPosOfWith =
            tokens |> List.tryPick (fun (t: TokenInformation) ->
                        if t.CharClass = TokenCharKind.Keyword && t.LeftColumn >= column then
                            let text = lineStr.[t.LeftColumn..t.RightColumn]
                            match text with
                            | "with" -> Some (Pos.fromZ line (t.RightColumn + 1))
                            | _ -> None
                        else None)
        let ast = 
            vsLanguageService.ParseFileInProject(doc.FullName, source, project)
            |> Async.RunSynchronously
        let pos = Pos.fromZ line column
        ast.ParseTree
        |> Option.bind (InterfaceStubGenerator.tryFindInterfaceDeclaration pos)
        |> Option.map (fun iface -> iface, endPosOfWith)

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
                    match symbol.Kind with
                    | SymbolKind.Ident ->
                        match collectInterfaceData point doc project with
                        | Some interfaceData ->
                            let results = 
                                vsLanguageService.GetFSharpSymbol (newWord, symbol, doc.FullName, project, AllowStaleResults.MatchingSource)
                                |> Async.RunSynchronously
                            match results with
                            | Some (fsSymbol, _) when (fsSymbol :? FSharpEntity) ->
                                let entity = fsSymbol :?> FSharpEntity
                                if entity.IsInterface then
                                    interfaceDefinition <- Some (interfaceData, entity)
                                    currentWord <- Some newWord
                                    let span = SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)
                                    tagsChanged.Trigger(self, SnapshotSpanEventArgs(span))
                                else
                                    interfaceDefinition <- None
                            | _ ->
                                interfaceDefinition <- None
                        | None ->
                            interfaceDefinition <- None 
                    | _ ->
                       interfaceDefinition <- None 
                }
                |> ignore
        | _ -> ()

    let _ = DocumentEventsListener ([ViewChange.layoutEvent view; ViewChange.caretEvent view], 
                                    TimeSpan.FromMilliseconds 200.,
                                    fun _ -> updateAtCaretPosition())

    let getRange = function
        | InterfaceData.Interface(typ, _) -> typ.Range
        | InterfaceData.ObjExpr(typ, _) -> typ.Range

    let inferStartColumn (lineStr: string) = function
        | InterfaceData.Interface(_, Some (m :: _)) ->
            let line = buffer.CurrentSnapshot.GetLineFromLineNumber(m.Range.StartLine-1)
            let str = line.GetText()
            str.Length - str.TrimStart(' ').Length
        | InterfaceData.ObjExpr(_, b :: _) -> 
            let line = buffer.CurrentSnapshot.GetLineFromLineNumber(b.RangeOfBindingSansRhs.StartLine-1)
            let str = line.GetText()
            str.Length - str.TrimStart(' ').Length
        | _ ->
            let editorOptions = editorOptionsFactory.GetOptions(buffer)
            let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
            lineStr.Length - lineStr.TrimStart(' ').Length + indentSize

    let countMembers = function
        | InterfaceData.Interface(_, None) -> 0
        | InterfaceData.Interface(_, Some members) -> 
            List.length members
        | InterfaceData.ObjExpr(_, bindings) ->
            List.length bindings

    let getTypeParameters = function
        | InterfaceData.Interface(typ, _)
        | InterfaceData.ObjExpr(typ, _) ->
            match typ with
            | SynType.App(_, _, ts, _, _, _, _)
            | SynType.LongIdentApp(_, _, _, ts, _, _, _) ->
                let (|TypeIdent|_|) = function
                    | SynType.Var(SynTypar.Typar(s, req , _), _) ->
                        match req with
                        | NoStaticReq -> 
                            Some ("'" + s.idText)
                        | HeadTypeStaticReq -> 
                            Some ("^" + s.idText)
                    | SynType.LongIdent(LongIdentWithDots(xs, _)) ->
                        xs |> Seq.map (fun x -> x.idText) |> String.concat "." |> Some
                    | _ -> 
                        None
                ts |> Seq.choose (|TypeIdent|_|) |> Seq.toArray
            | _ ->
                [||]

    // Check whether the interface has been fully implemented
    let shouldImplementInterface (interfaceData, _) (entity: FSharpEntity) =
        // TODO: counting members is not enough.
        // We should match member signatures, 
        // it will be tricky in case of specialized interface implementation
        let c = InterfaceStubGenerator.countInterfaceMembers entity
        if c = 0 then false
        else
            let c' = countMembers interfaceData
            c' < c

    let handleImplementInterface (span: SnapshotSpan) (interfaceData, posOpt: pos option) entity = 
        let line = span.Start.GetContainingLine()
        let lineStr = line.GetText()
        let startColumn = inferStartColumn lineStr interfaceData
        let editorOptions = editorOptionsFactory.GetOptions(buffer)
        let indentSize = editorOptions.GetOptionValue((IndentSize()).Key)
        let typeParams = getTypeParameters interfaceData
        let stub = InterfaceStubGenerator.formatInterface 
                        startColumn indentSize typeParams "x" "raise (System.NotImplementedException())" entity

        use transaction = textUndoHistory.CreateTransaction("Implement Interface Explicitly")
        match posOpt with
        | Some pos -> 
            let current = span.Snapshot.GetLineFromLineNumber(pos.Line-1).Start.Position + pos.Column
            buffer.Insert(current, stub) |> ignore
        | None ->
            let range = getRange interfaceData
            let current = span.Snapshot.GetLineFromLineNumber(range.EndLine-1).Start.Position + range.EndColumn
            buffer.Insert(current, " with") |> ignore
            buffer.Insert(current + 5, stub) |> ignore
        transaction.Complete()

    let implementInterface span data entity =
        { new ISmartTagAction with
            member x.ActionSets = null
            member x.DisplayText = "Implement Interface Explicitly"
            member x.Icon = null
            member x.IsEnabled = true
            member x.Invoke() = 
                if shouldImplementInterface data entity then
                    handleImplementInterface span data entity
                else
                    MessageBox.Show(Resource.implementInterfaceErrorMessage, Resource.vsPackageTitle, 
                        MessageBoxButton.OK, MessageBoxImage.Error) |> ignore }

    member x.GetSmartTagActions(span: SnapshotSpan, data, entity: FSharpEntity) =
        let actionSetList = ResizeArray<SmartTagActionSet>()
        let actionList = ResizeArray<ISmartTagAction>()

        let trackingSpan = span.Snapshot.CreateTrackingSpan(span.Span, SpanTrackingMode.EdgeInclusive)
        let _snapshot = trackingSpan.TextBuffer.CurrentSnapshot

        actionList.Add(implementInterface span data entity)
        let actionSet = SmartTagActionSet(actionList.AsReadOnly())
        actionSetList.Add(actionSet)
        actionSetList.AsReadOnly()

    interface ITagger<ImplementInterfaceSmartTag> with
        member x.GetTags(_spans: NormalizedSnapshotSpanCollection): ITagSpan<ImplementInterfaceSmartTag> seq =
            seq {
                match currentWord, interfaceDefinition with
                | Some word, Some (data, entity) ->
                    let span = SnapshotSpan(buffer.CurrentSnapshot, word.Span)
                    yield TagSpan<ImplementInterfaceSmartTag>(span, 
                            ImplementInterfaceSmartTag(x.GetSmartTagActions(span, data, entity)))
                            :> ITagSpan<_>
                | _ -> ()
            }

        member x.add_TagsChanged(handler) = tagsChanged.Publish.AddHandler(handler)
        member x.remove_TagsChanged(handler) = tagsChanged.Publish.RemoveHandler(handler)



