namespace FSharpVSPowerTools.ProjectSystem

open System
open System.Collections.Generic
open System.ComponentModel.Composition

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor

[<Export(typeof<OpenDocumentsTracker>)>]
type OpenDocumentsTracker
    [<ImportingConstructor>]
    (
        threadGuard: IThreadGuard,
        textDocumentFactoryService: ITextDocumentFactoryService

    ) =
    [<VolatileField>]    
    let mutable openDocuments = Map.empty

    member x.RegisterView(view: IWpfTextView) = 
        threadGuard.EnsureOnCorrectThread()
        match textDocumentFactoryService.TryGetTextDocument(view.TextBuffer) with
        | true, document ->
            let path = document.FilePath
            let rec textBufferChanged (args: TextContentChangedEventArgs) =
                threadGuard.EnsureOnCorrectThread()
                
                openDocuments <- Map.add path args.After openDocuments

            and textBufferChangedSubscription: IDisposable = view.TextBuffer.Changed.Subscribe(textBufferChanged)
            and viewClosed _ = 
                threadGuard.EnsureOnCorrectThread()

                textBufferChangedSubscription.Dispose()
                viewClosedSubscription.Dispose()
                openDocuments <- Map.remove path openDocuments

            and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
            
            openDocuments <- Map.add path view.TextBuffer.CurrentSnapshot openDocuments

        | _ -> ()
    
    member x.MapOpenDocuments f = 
        // use current collection snapshot
        Seq.map f openDocuments