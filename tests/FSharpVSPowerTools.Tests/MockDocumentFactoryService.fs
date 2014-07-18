namespace FSharpVSPowerTools.Tests

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities

type MockDocumentFactoryService() =
    interface ITextDocumentFactoryService with
        member x.CreateAndLoadTextDocument(filePath: string, contentType: IContentType): ITextDocument = 
            failwith "Not implemented yet"
        
        member x.CreateAndLoadTextDocument(filePath: string, contentType: IContentType, encoding: Text.Encoding, characterSubstitutionsOccurred: byref<bool>): ITextDocument = 
            failwith "Not implemented yet"
        
        member x.CreateAndLoadTextDocument(filePath: string, contentType: IContentType, attemptUtf8Detection: bool, characterSubstitutionsOccurred: byref<bool>): ITextDocument = 
            failwith "Not implemented yet"
        
        member x.CreateTextDocument(textBuffer: ITextBuffer, filePath: string): ITextDocument = 
            failwith "Not implemented yet"
        
        [<CLIEvent>]
        member x.TextDocumentCreated: IEvent<EventHandler<TextDocumentEventArgs>, _> = 
            failwith "Not implemented yet"
        
        [<CLIEvent>]
        member x.TextDocumentDisposed: IEvent<EventHandler<TextDocumentEventArgs>, _> = 
            failwith "Not implemented yet"
        
        member x.TryGetTextDocument(textBuffer: ITextBuffer, textDocument: byref<ITextDocument>): bool = 
            textBuffer.Properties.TryGetProperty(typeof<ITextDocument>, &textDocument) 

        
