[<RequireQualifiedAccess>]
module FSharpVSPowerTools.Tests.Mocks

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities

let createDocumentFactoryService() =
    { 
        new ITextDocumentFactoryService with
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType): ITextDocument = notimpl
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType, _encoding: Text.Encoding, _characterSubstitutionsOccurred: byref<bool>): ITextDocument = notimpl
            member __.CreateAndLoadTextDocument(_filePath: string, _contentType: IContentType, _attemptUtf8Detection: bool, _characterSubstitutionsOccurred: byref<bool>): ITextDocument = notimpl
            member __.CreateTextDocument(_textBuffer: ITextBuffer, _filePath: string): ITextDocument = notimpl
            [<CLIEvent>]
            member __.TextDocumentCreated: IEvent<EventHandler<TextDocumentEventArgs>, _> = notimpl
            [<CLIEvent>]
            member __.TextDocumentDisposed: IEvent<EventHandler<TextDocumentEventArgs>, _> = notimpl
        
            member __.TryGetTextDocument(textBuffer: ITextBuffer, textDocument: byref<ITextDocument>): bool = 
                textBuffer.Properties.TryGetProperty(typeof<ITextDocument>, &textDocument) 
    }