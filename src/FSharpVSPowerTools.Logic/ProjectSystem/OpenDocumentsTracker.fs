namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Text
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.Collections.Concurrent
open System.Collections.Generic

[<NoComparison>]
type OpenDocument =
    { Document: ITextDocument
      Snapshot: ITextSnapshot 
      Encoding: Encoding
      LastChangeTime: DateTime }
    member x.Text = lazy (x.Snapshot.GetText ())
    static member Create document snapshot encoding = 
        { Document = document; Snapshot = snapshot; Encoding = encoding; LastChangeTime = DateTime.Now }

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
    let mutable openDocuments = ConcurrentDictionary<string,OpenDocument>()
    let documentChanged = Event<_> ()
    let documentClosed = Event<_> ()

    interface IOpenDocumentsTracker with
        member __.RegisterView (view: IWpfTextView) = 
            ForegroundThreadGuard.CheckThread()
            match textDocumentFactoryService.TryGetTextDocument view.TextBuffer with
            | true, doc ->
                let path = doc.FilePath
                let textBufferChanged (args: TextContentChangedEventArgs) =
                    ForegroundThreadGuard.CheckThread()
                    openDocuments.TryAdd(path,(OpenDocument.Create doc args.After doc.Encoding)) |> ignore
                    documentChanged.Trigger path
        
                let textBufferChangedSubscription: IDisposable = view.TextBuffer.ChangedHighPriority.Subscribe textBufferChanged
                
                let rec viewClosed _ = 
                    ForegroundThreadGuard.CheckThread()
                    textBufferChangedSubscription.Dispose()
                    viewClosedSubscription.Dispose()
                    openDocuments.TryRemove path  |> ignore
                    documentClosed.Trigger path
        
                and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
                
                openDocuments.TryAdd(path,(OpenDocument.Create doc view.TextBuffer.CurrentSnapshot doc.Encoding)) |> ignore
        
            | _ -> ()
        
        member __.MapOpenDocuments f = 
            // use current collection snapshot
            Seq.map f openDocuments
        
        member __.TryFindOpenDocument path = if openDocuments.ContainsKey path then Some openDocuments.[path] else None
        member __.TryGetDocumentText path = if openDocuments.ContainsKey path then Some openDocuments.[path].Text.Value else None
        member __.DocumentChanged = documentChanged.Publish
        member __.DocumentClosed = documentClosed.Publish