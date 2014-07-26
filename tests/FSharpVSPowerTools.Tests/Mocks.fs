[<RequireQualifiedAccess>]
module FSharpVSPowerTools.Tests.Mocks

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Text.Editor

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

let createClassificationType classificationType =
    {
        new IClassificationType with
            member __.BaseTypes: IClassificationType seq = notimpl
            member __.IsOfType(_type: string): bool = notimpl
                            
            member __.Classification: string = classificationType            
    }

let createClassificationTypeRegistryService() =
    { 
        new IClassificationTypeRegistryService with
            member __.CreateClassificationType(_type: string, _baseTypes: IClassificationType seq): IClassificationType = notimpl
            member __.CreateTransientClassificationType(_baseTypes: IClassificationType seq): IClassificationType = notimpl
            member __.CreateTransientClassificationType(_baseTypes: IClassificationType []): IClassificationType = notimpl

            member __.GetClassificationType(``type``: string): IClassificationType = 
                createClassificationType ``type``
    }

let createVsEditorAdaptersFactoryService() =
    {
        new IVsEditorAdaptersFactoryService with
            member __.CreateVsCodeWindowAdapter(_serviceProvider: IServiceProvider): IVsCodeWindow = notimpl
            member __.CreateVsTextBufferAdapter(_serviceProvider: IServiceProvider): IVsTextBuffer = notimpl
            member __.CreateVsTextBufferAdapter(_serviceProvider: IServiceProvider, _contentType: IContentType): IVsTextBuffer = notimpl
            member __.CreateVsTextBufferAdapterForSecondaryBuffer(_serviceProvider: IServiceProvider, _secondaryBuffer: ITextBuffer): IVsTextBuffer = notimpl
            member __.CreateVsTextBufferCoordinatorAdapter(): IVsTextBufferCoordinator = notimpl
            member __.CreateVsTextViewAdapter(_serviceProvider: IServiceProvider): IVsTextView = notimpl
            member __.CreateVsTextViewAdapter(_serviceProvider: IServiceProvider, _roles: ITextViewRoleSet): IVsTextView = notimpl
            member __.GetBufferAdapter(_textBuffer: ITextBuffer): IVsTextBuffer = notimpl
            member __.GetDataBuffer(_bufferAdapter: IVsTextBuffer): ITextBuffer = notimpl
            member __.GetDocumentBuffer(_bufferAdapter: IVsTextBuffer): ITextBuffer = notimpl
            member __.GetViewAdapter(_textView: ITextView): IVsTextView = notimpl
            member __.GetWpfTextView(_viewAdapter: IVsTextView): IWpfTextView = notimpl
            member __.GetWpfTextViewHost(_viewAdapter: IVsTextView): IWpfTextViewHost = notimpl
            member __.SetDataBuffer(_bufferAdapter: IVsTextBuffer, _dataBuffer: ITextBuffer): unit = notimpl
    }