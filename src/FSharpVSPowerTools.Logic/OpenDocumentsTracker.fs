namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Text
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.Collections.Generic
open FSharpVSPowerTools

[<NoComparison>]
type OpenDocument =
    { Document: ITextDocument
      Snapshot: ITextSnapshot 
      Encoding: Encoding
      LastChangeTime: DateTime
      ViewCount: int }
    member x.Text = lazy (x.Snapshot.GetText())
    static member Create document snapshot encoding lastChangeTime = 
        { Document = document
          Snapshot = snapshot
          Encoding = encoding
          LastChangeTime = lastChangeTime
          ViewCount = 1 }

type IOpenDocumentsTracker =
    abstract RegisterView: IWpfTextView -> unit
    abstract MapOpenDocuments: (KeyValuePair<string, OpenDocument> -> 'a) -> seq<'a>
    abstract TryFindOpenDocument: string -> OpenDocument option
    abstract TryGetDocumentText: string -> string option
    abstract DocumentChanged: IEvent<string>
    abstract DocumentClosed: IEvent<string>

[<Export(typeof<IOpenDocumentsTracker>)>]
type OpenDocumentsTracker [<ImportingConstructor>](textDocumentFactoryService: ITextDocumentFactoryService) =
    [<VolatileField>]
    let mutable openDocuments = Map.empty
    let documentChanged = Event<_>()
    let documentClosed = Event<_>()

    let tryGetDocument buffer = 
        match textDocumentFactoryService.TryGetTextDocument buffer with
        | true, doc -> Some doc
        | _ -> None

    interface IOpenDocumentsTracker with
        member __.RegisterView(view: IWpfTextView) = 
            ForegroundThreadGuard.CheckThread()
            maybe {
                let! doc = tryGetDocument view.TextBuffer
                let path = doc.FilePath
                
                let textBufferChanged (args: TextContentChangedEventArgs) =
                    match openDocuments |> Map.tryFind path with
                    | Some doc ->
                        ForegroundThreadGuard.CheckThread()
                        openDocuments <- Map.add path { doc with Snapshot = args.After
                                                                 LastChangeTime = DateTime.UtcNow } openDocuments
                        documentChanged.Trigger path
                    | None -> ()
                
                let textBufferChangedSubscription = view.TextBuffer.ChangedHighPriority.Subscribe textBufferChanged
                
                let rec viewClosed _ =
                    ForegroundThreadGuard.CheckThread()
                    textBufferChangedSubscription.Dispose()
                    viewClosedSubscription.Dispose()
                    openDocuments <- Map.remove path openDocuments
                    documentClosed.Trigger path
                
                and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
                let! lastWriteTime = File.tryGetLastWriteTime path
                
                let openDocument = 
                    openDocuments 
                    |> Map.tryFind path
                    |> Option.map (fun doc -> { doc with ViewCount = doc.ViewCount + 1 })
                    |> Option.getOrTry (fun _ ->
                        OpenDocument.Create doc view.TextBuffer.CurrentSnapshot doc.Encoding lastWriteTime)
                
                openDocuments <- Map.add path openDocument openDocuments
            } |> ignore
        
        member __.MapOpenDocuments f = Seq.map f openDocuments
        member __.TryFindOpenDocument path = Map.tryFind path openDocuments
        member __.TryGetDocumentText path = 
            let doc = openDocuments |> Map.tryFind path 
            doc |> Option.map (fun x -> x.Text.Value)
        member __.DocumentChanged = documentChanged.Publish
        member __.DocumentClosed = documentClosed.Publish