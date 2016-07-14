namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Text
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.Collections.Generic
open FSharpVSPowerTools
open FSharpPowerTools.Core.Infrastructure

[<NoComparison>]
type OpenDocument =
    { Document: ITextDocument
      Snapshot: ITextSnapshot 
      Encoding: Encoding
      LastChangeTime: DateTime
      ViewCount: int }
    static member Create document snapshot encoding lastChangeTime = 
        { Document = document
          Snapshot = snapshot
          Encoding = encoding
          LastChangeTime = lastChangeTime
          ViewCount = 1 }
    member private x.text = lazy (x.Snapshot.GetText())
    member x.Text = x.text
    interface IOpenDocument with
        member x.Text = x.text

type IVSOpenDocumentsTracker =
    inherit IOpenDocumentsTracker<OpenDocument>
    abstract RegisterView: IWpfTextView -> unit

type IOpenDocumentsTracker = IOpenDocumentsTracker<OpenDocument>

[<Export(typeof<IVSOpenDocumentsTracker>); Export(typeof<IOpenDocumentsTracker<OpenDocument>>)>]
type OpenDocumentsTracker [<ImportingConstructor>](textDocumentFactoryService: ITextDocumentFactoryService) =
    [<VolatileField>]
    let mutable openDocs = Map.empty
    let documentChanged = Event<_>()
    let documentClosed = Event<_>()
    let tryFindDoc path = openDocs |> Map.tryFind path
    let addDoc path doc = openDocs <- openDocs |> Map.add path doc
    
    let updateDoc path (f: OpenDocument -> OpenDocument) =
        match tryFindDoc path with
        | Some doc -> addDoc path (f doc)
        | None -> ()

    let tryGetDocument buffer = 
        match textDocumentFactoryService.TryGetTextDocument buffer with
        | true, doc -> Some doc
        | _ -> None

    interface IVSOpenDocumentsTracker with
        member __.RegisterView (view: IWpfTextView) = 
            ForegroundThreadGuard.CheckThread()
            maybe {
                let! doc = tryGetDocument view.TextBuffer
                let path = doc.FilePath
                
                let textBufferChanged (args: TextContentChangedEventArgs) =
                    if openDocs |> Map.containsKey path then
                        ForegroundThreadGuard.CheckThread()
                        updateDoc path (fun doc -> { doc with Snapshot = args.After
                                                              LastChangeTime = DateTime.UtcNow })
                        documentChanged.Trigger path
                
                let textBufferChangedSubscription = view.TextBuffer.ChangedHighPriority.Subscribe textBufferChanged
                
                let rec viewClosed _ =
                    match tryFindDoc path with
                    | Some doc when doc.ViewCount = 1 ->
                        Logging.logInfo (fun _ -> sprintf "[OpenDocumentTracker] Last view for %s, removing from map." path)
                        ForegroundThreadGuard.CheckThread()
                        textBufferChangedSubscription.Dispose()
                        viewClosedSubscription.Dispose()
                        openDocs <- Map.remove path openDocs
                        documentClosed.Trigger path
                    | Some doc ->
                        Logging.logInfo (fun _ -> 
                            sprintf "[OpenDocumentTracker] Still %d view are open for %s, do not remove." 
                                    (doc.ViewCount - 1) path) 
                        updateDoc path (fun doc -> { doc with ViewCount = doc.ViewCount - 1 })
                    | None -> ()
                
                and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
                let! lastWriteTime = File.tryGetLastWriteTime path
                
                tryFindDoc path
                |> Option.map (fun doc -> { doc with ViewCount = doc.ViewCount + 1 })
                |> Option.getOrTry (fun _ ->
                    OpenDocument.Create doc view.TextBuffer.CurrentSnapshot doc.Encoding lastWriteTime)
                |> addDoc path
            } |> ignore
        
        member __.MapOpenDocuments f = Seq.map f openDocs
        member __.TryFindOpenDocument path = Map.tryFind path openDocs
        member __.TryGetDocumentText path = 
            let doc = openDocs |> Map.tryFind path 
            doc |> Option.map (fun x -> x.Text.Value)
        member __.DocumentChanged = documentChanged.Publish
        member __.DocumentClosed = documentClosed.Publish