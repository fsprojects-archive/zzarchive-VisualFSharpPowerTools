namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Text
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor

[<NoComparison>]
type OpenDocument =
    { Document: ITextDocument
      Snapshot: ITextSnapshot 
      Encoding: Encoding
      LastChangeTime: DateTime }
    static member Create document snapshot encoding = 
        { Document = document; Snapshot = snapshot; Encoding = encoding; LastChangeTime = DateTime.Now }

[<Export(typeof<OpenDocumentsTracker>)>]
type OpenDocumentsTracker [<ImportingConstructor>](textDocumentFactoryService: ITextDocumentFactoryService) =
    [<VolatileField>]
    let mutable openDocuments = Map.empty
    let documentChanged = Event<_>()
    let documentClosed = Event<_>()

    member __.RegisterView(view: IWpfTextView) = 
        ForegroundThreadGuard.CheckThread()
        match textDocumentFactoryService.TryGetTextDocument view.TextBuffer with
        | true, doc ->
            let path = doc.FilePath
            let textBufferChanged (args: TextContentChangedEventArgs) =
                ForegroundThreadGuard.CheckThread()
                openDocuments <- Map.add path (OpenDocument.Create doc args.After doc.Encoding) openDocuments
                documentChanged.Trigger path

            let textBufferChangedSubscription: IDisposable = view.TextBuffer.ChangedHighPriority.Subscribe textBufferChanged
            
            let rec viewClosed _ = 
                ForegroundThreadGuard.CheckThread()
                textBufferChangedSubscription.Dispose()
                viewClosedSubscription.Dispose()
                openDocuments <- Map.remove path openDocuments
                documentClosed.Trigger path

            and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
            
            openDocuments <- Map.add path (OpenDocument.Create doc view.TextBuffer.CurrentSnapshot doc.Encoding) openDocuments

        | _ -> ()
    
    member __.MapOpenDocuments f = 
        // use current collection snapshot
        Seq.map f openDocuments

    member __.TryFindOpenDocument path = Map.tryFind path openDocuments
    member __.DocumentChanged = documentChanged.Publish
    member __.DocumentClosed = documentClosed.Publish