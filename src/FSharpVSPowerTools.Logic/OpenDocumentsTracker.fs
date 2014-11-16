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

    member x.RegisterView(view: IWpfTextView) = 
        ForegroundThreadGuard.CheckThread()
        match textDocumentFactoryService.TryGetTextDocument(view.TextBuffer) with
        | true, doc ->
            let path = doc.FilePath
            let rec textBufferChanged (args: TextContentChangedEventArgs) =
                ForegroundThreadGuard.CheckThread()
                openDocuments <- Map.add path (OpenDocument.Create doc args.After doc.Encoding) openDocuments

            and textBufferChangedSubscription: IDisposable = view.TextBuffer.ChangedHighPriority.Subscribe(textBufferChanged)
            and viewClosed _ = 
                ForegroundThreadGuard.CheckThread()
                textBufferChangedSubscription.Dispose()
                viewClosedSubscription.Dispose()
                openDocuments <- Map.remove path openDocuments

            and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
            
            openDocuments <- Map.add path (OpenDocument.Create doc view.TextBuffer.CurrentSnapshot doc.Encoding) openDocuments

        | _ -> ()
    
    member x.MapOpenDocuments f = 
        // use current collection snapshot
        Seq.map f openDocuments

    member x.TryFindOpenDocument path = Map.tryFind path openDocuments