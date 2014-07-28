[<RequireQualifiedAccess>]
module FSharpVSPowerTools.Tests.Mocks

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio

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

let createSVsResourceManager() =
    {
        new IVsResourceManager with
            member __.GetSatelliteAssemblyPath(assemblyPath: string, lcid: int, pbstrPath: byref<string>): int = 
                notimpl
            member __.LoadResourceBitmap(guidPackage: byref<Guid>, culture: int, pszResourceName: string, hbmpValue: byref<nativeint>): int = 
                VSConstants.S_FALSE

            member __.LoadResourceBitmap2(pszAssemblyPath: string, culture: int, szResourceName: string, hbmpValue: byref<nativeint>): int = 
                notimpl
            member __.LoadResourceBlob(guidPackage: byref<Guid>, culture: int, pszResourceName: string, pBytes: byref<nativeint>, lAllocated: byref<int>): int = 
                notimpl
            member __.LoadResourceBlob2(pszAssemblyPath: string, culture: int, pszResourceName: string, pBytes: byref<nativeint>, lAllocated: byref<int>): int = 
                notimpl
            member __.LoadResourceIcon(guidPackage: byref<Guid>, culture: int, pszResourceName: string, cx: int, cy: int, hicoValue: byref<nativeint>): int = 
                notimpl
            member __.LoadResourceIcon2(pszAssemblyPath: string, culture: int, pszResourceName: string, cx: int, cy: int, hicoValue: byref<nativeint>): int = 
                notimpl
            member __.LoadResourceString(guidPackage: byref<Guid>, culture: int, pszResourceName: string, pbstrValue: byref<string>): int = 
                notimpl
            member __.LoadResourceString2(pszAssemblyPath: string, culture: int, pszResourceName: string, pbstrValue: byref<string>): int = 
                notimpl
    }
