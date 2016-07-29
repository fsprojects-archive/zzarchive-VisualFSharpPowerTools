namespace FSharp.Editing.ProjectSystem

open System
open System.Text
open System.Reflection
open System.Composition
open System.Linq
open System.Threading
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open Microsoft.CodeAnalysis
open FSharp.Editing
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Host
open Microsoft.CodeAnalysis.Host.Mef


type IHostServicesProvider =
    abstract Assemblies : Assembly ImmutableArray with get


[<Export>]
type HostServicesAggregator [<ImportingConstructor>] 
    ( [<ImportMany>] hostServicesProviders : seq<IHostServicesProvider>) =
    let builder = ImmutableHashSet.CreateBuilder<Assembly>() 
    do
        for asm in MefHostServices.DefaultAssemblies do 
            builder.Add asm |> ignore
        for provider in hostServicesProviders do
            for asm in provider.Assemblies do
                builder.Add asm |> ignore
    let assemblies = builder.ToImmutableArray()
    
    member __.CreateHostServices () =
        MefHostServices.Create assemblies


[<CustomEquality; NoComparison>]
type LinePositionSpanTextChange =
    {   NewText     : string
        StartLine   : int
        StartColumn : int
        EndLine     : int
        EndColumn   : int        
    }
    override self.Equals obj =
        match obj with
        | :? LinePositionSpanTextChange as other ->
            self.NewText     = other.NewText     &&
            self.StartLine   = other.StartLine   &&
            self.StartColumn = other.StartColumn &&
            self.EndLine     = other.EndLine     &&
            self.EndColumn   = other.EndColumn
        | _ -> false
            
    override self.GetHashCode() =
        self.NewText.GetHashCode()
        * (23 + self.StartLine)
        * (29 + self.StartColumn)
        * (31 + self.EndLine)
        * (37 + self.EndColumn)

[<NoComparison>]
type Request =
    {   Line     : int
        Column   : int
        Buffer   : string
        FileName : string
        FromDisk : bool
        Changes  : LinePositionSpanTextChange list
    }


type ChangeBufferRequest =
    {   FileName    : string
        StartLine   : int
        StartColumn : int
        EndLine     : int
        EndColumn   : int
        NewText     : string
    }


module Workspace =

    let convertTextChanges (document:Document) (changes: TextChange seq) = async {
        let! text = document.GetTextAsync() |> Async.AwaitTask

        return changes
        |> Seq.sortByDescending (fun change -> change.Span.Start)
        |> Seq.map (fun change ->
            let span = change.Span
            let newText = change.NewText
            let prefix, postfix, span =
                if newText.Length > 0 then
                    // Roslyn computes text changes on character arrays. So it might happen that a
                    // change starts inbetween \r\n which is OK when you are offset-based but a problem
                    // when you are line,column-based. This code extends text edits which just overlap
                    // a with a line break to its full line break
                    if span.Start > 0 && newText.[0] = '\n' 
                      && text.[span.Start - 1] = '\r' then
                        // text: foo\r\nbar\r\nfoo
                        // edit:      [----)
                        "\r", "", TextSpan.FromBounds(span.Start - 1, span.End)
                    elif span.End < text.Length - 1 && text.[span.End] = '\n' 
                     && newText.[newText.Length - 1] = '\r' then
                     
                        // text: foo\r\nbar\r\nfoo
                        // edit:        [----)
                        "", "\n", TextSpan.FromBounds(span.Start, span.End + 1)
                    else
                        "","",span
                else
                    "","",span
            let linePositionSpan = text.Lines.GetLinePositionSpan span
            {   NewText     = prefix + newText + postfix
                StartLine   = linePositionSpan.Start.Line
                StartColumn = linePositionSpan.Start.Character
                EndLine     = linePositionSpan.End.Line
                EndColumn   = linePositionSpan.End.Character
            } : LinePositionSpanTextChange
        )
    }
    
    let findProjectsByFileName (filename:string) (workspace:Workspace) =
        let dirInfo = (FileInfo filename).Directory
        let candidates =
            workspace.CurrentSolution.Projects
            |> Seq.groupBy (fun project -> (FileInfo project.FilePath).Directory.FullName)
            |> Map.ofSeq

        let rec loop (dirInfo:DirectoryInfo) projects =
            if isNull dirInfo then projects else
            match candidates.TryFind dirInfo.FullName with
            | Some projects -> projects
            | None -> loop dirInfo.Parent projects

        loop dirInfo Seq.empty           

open Workspace 


[<Export; Shared>]
type FSharpWorkspace [<ImportingConstructor>] (aggregator:HostServicesAggregator) as self =
    inherit Workspace(aggregator.CreateHostServices(), "FSharp")
            
    let bufferManager = new BufferManager(self)
    let disposables = ResizeArray<IDisposable>() 

    do 
        disposables.Add bufferManager
    //let activeDocuments = HashSet<DocumentId>()

    override __.CanOpenDocuments = true

    override  __.CanApplyChange _ = true

    override __.OpenDocument(docId, activate) =
        let doc = base.CurrentSolution.GetDocument docId
        if isNull doc then () else
        let task = doc.GetTextAsync(CancellationToken.None)
        task.Wait(CancellationToken.None)
        let text = task.Result
        base.OnDocumentOpened(docId,text.Container,activate)


    override __.CloseDocument(docId) =
        let doc = base.CurrentSolution.GetDocument docId
        if isNull doc then () else
        let task = doc.GetTextAsync(CancellationToken.None)
        task.Wait(CancellationToken.None)
        let text = task.Result

        let versionTask = doc.GetTextVersionAsync(CancellationToken.None)
        versionTask.Wait(CancellationToken.None)
        let version = versionTask.Result
        let loader = TextLoader.From(TextAndVersion.Create(text,version,doc.FilePath))
        base.OnDocumentClosed(docId,loader)


    member __.AddProject projectInfo =
        base.OnProjectAdded projectInfo

    member __.AddProjectReference (projectId, projectReference) =
        base.OnProjectReferenceAdded(projectId,projectReference)

    member __.AddMetadataReference (projectId, metadataReference) =
        base.OnMetadataReferenceAdded(projectId, metadataReference)
    
    member __.RemoveMetadataReference(projectId, metadataReference) =
        base.OnMetadataReferenceRemoved(projectId, metadataReference)

    member __. AddDocument (documentInfo) =
        base.OnDocumentAdded(documentInfo)

    member __. RemoveDocument (documentId) =
        base.OnDocumentRemoved(documentId)

    member __. RemoveProject (projectId) =
        base.OnProjectRemoved(projectId)

    member __. SetCompilationOptions(projectId, options) =
        base.OnCompilationOptionsChanged(projectId, options)

    member __. SetParseOptions(projectId, parseOptions) =
        base.OnParseOptionsChanged(projectId, parseOptions)

    member __. OnDocumentChanged(documentId, text) =
        base.OnDocumentTextChanged(documentId, text, PreservationMode.PreserveIdentity)

    member __.TryGetDocumentId (filePath) =
        let documentIds = base.CurrentSolution.GetDocumentIdsWithFilePath(filePath)
        match documentIds.FirstOrDefault() with
        | null -> None
        | docId -> Some docId

    member self.GetDocuments(filePath) =
        base.CurrentSolution
            .GetDocumentIdsWithFilePath(filePath)
            .Select(fun docId -> self.CurrentSolution.GetDocument(docId))

    member self.TryGetDocument(filePath) =
        self.TryGetDocumentId(filePath)
        |> Option.map(fun docId -> self.CurrentSolution.GetDocument(docId))

    interface IDisposable with
        member __.Dispose () = 
            disposables |> Seq.iter (fun d -> d.Dispose())
            disposables.Clear()



and internal BufferManager (workspace:FSharpWorkspace) as self =
    let transientDocuments = Dictionary<string, DocumentId list>(StringComparer.OrdinalIgnoreCase)
    let transientDocumentIds = HashSet<DocumentId>()
    let lockObj = obj()
    let workspaceEvent = workspace.WorkspaceChanged
    let subscriptions = ResizeArray<IDisposable>()
    do
        workspaceEvent.Subscribe(self.OnWorkspaceChanged)
        |> subscriptions.Add

    let tryAddTransientDocument (fileName:string) (fileContent:string) =
        if String.IsNullOrWhiteSpace fileName then false else
        let projects = findProjectsByFileName fileName workspace
        if projects.Count() = 0 then false else
        let sourceText = SourceText.From fileContent
        let documents: DocumentInfo list =
            (projects,[]) ||> Seq.foldBack (fun project  docs ->
                let docId = DocumentId.CreateNewId project.Id
                let version = VersionStamp.Create()
                let docInfo = DocumentInfo.Create(docId,fileName,filePath=fileName,loader=TextLoader.From(TextAndVersion.Create(sourceText,version)))
                docInfo::docs
            )
        lock lockObj (fun () ->
            let docIds = documents |> List.map(fun doc -> doc.Id)
            transientDocuments.Add(fileName, docIds)
            transientDocumentIds.UnionWith docIds
        )
        documents |> List.iter(fun doc -> workspace.AddDocument doc)
        true


    member __.UpdateBuffer (request:Request) = async {
        let buffer = if request.FromDisk then File.ReadAllText(request.FileName) else request.Buffer
        let changes = request.Changes
        let documentIds = workspace.CurrentSolution.GetDocumentIdsWithFilePath request.FileName 
        if not documentIds.IsEmpty then
            if changes = [] then
                let sourceText = SourceText.From buffer
                documentIds |> Seq.iter(fun docId -> workspace.OnDocumentChanged(docId, sourceText)) 
            else
                for docId in documentIds do
                    let doc = workspace.CurrentSolution.GetDocument docId 
                    let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
                    let sourceText =
                        (sourceText, changes) 
                        ||> List.fold (fun sourceText change ->
                            let startOffset = sourceText.Lines.GetPosition(LinePosition(change.StartLine,change.StartLine))
                            let endOffset = sourceText.Lines.GetPosition(LinePosition(change.EndLine,change.EndColumn))
                            sourceText.WithChanges
                                ([| TextChange(TextSpan(startOffset, endOffset - startOffset), change.NewText) |])
                        )
                    workspace.OnDocumentChanged(docId, sourceText)
        elif not (isNull buffer) then
            tryAddTransientDocument request.FileName buffer |> ignore
    }
            

    member __.UpdateChangeBuffer (request:ChangeBufferRequest) = async {
        if isNull request.FileName then () else
        let documentIds = workspace.CurrentSolution.GetDocumentIdsWithFilePath( request.FileName )
        if not documentIds.IsEmpty then
            for docId in documentIds do
                let doc = workspace.CurrentSolution.GetDocument docId 
                let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
                let startOffset = sourceText.Lines.GetPosition(LinePosition(request.StartLine,request.StartLine))
                let endOffset = sourceText.Lines.GetPosition(LinePosition(request.EndLine,request.EndColumn))
                let sourceText = 
                    sourceText.WithChanges
                        ([| TextChange(TextSpan(startOffset, endOffset - startOffset), request.NewText) |])
                workspace.OnDocumentChanged(docId, sourceText)
        else
            tryAddTransientDocument request.FileName request.NewText |> ignore
    }

    member __.OnWorkspaceChanged (args:WorkspaceChangeEventArgs) =
        let filename =
            match args.Kind with
            | WorkspaceChangeKind.DocumentAdded -> 
                (args.NewSolution.GetDocument args.DocumentId).FilePath
            | WorkspaceChangeKind.DocumentRemoved ->
                (args.OldSolution.GetDocument args.DocumentId).FilePath
            | _ -> String.Empty

        if String.IsNullOrEmpty filename then () else

        lock lockObj ( fun ()  ->
            match  Dict.tryFind filename transientDocuments with
            | None -> ()
            | Some docIds ->
                transientDocuments.Remove filename |> ignore
                for docId in docIds do
                    workspace.RemoveDocument docId |> ignore
                    transientDocumentIds.Remove docId |> ignore
        )


    interface IDisposable with
        member __.Dispose () = 
            subscriptions |> Seq.iter (fun d -> d.Dispose())
            subscriptions.Clear()
      


