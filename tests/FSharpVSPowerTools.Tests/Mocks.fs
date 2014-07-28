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

let createTextUndoTransation() =
    { 
        new ITextUndoTransaction with
            member __.AddUndo(undo: ITextUndoPrimitive): unit = notimpl
            member __.CanRedo: bool = notimpl
            member __.CanUndo: bool = notimpl
            member __.Cancel(): unit = notimpl
            member __.Complete() = 
                ()

            member __.Description with get (): string = notimpl and set (v: string): unit = notimpl
            member __.Dispose(): unit = 
                ()

            member __.Do(): unit = notimpl
            member __.History: ITextUndoHistory = notimpl            
            member __.MergePolicy with get (): IMergeTextUndoTransactionPolicy = notimpl and set (v: IMergeTextUndoTransactionPolicy): unit = notimpl            
            member __.Parent: ITextUndoTransaction = notimpl            
            member __.State: UndoTransactionState = notimpl            
            member __.Undo(): unit = notimpl            
            member __.UndoPrimitives: Collections.Generic.IList<ITextUndoPrimitive> = notimpl            
    }

let createTextUndoHistory() =
    {
        new ITextUndoHistory with
            member __.CanRedo: bool = notimpl
            member __.CanUndo: bool = notimpl
            member __.CreateTransaction(_description: string): ITextUndoTransaction = createTextUndoTransation()
            member __.CurrentTransaction: ITextUndoTransaction = notimpl
            member __.LastRedoTransaction: ITextUndoTransaction = notimpl
            member __.LastUndoTransaction: ITextUndoTransaction = notimpl
            member __.Properties: PropertyCollection = notimpl
            member __.Redo(_count: int): unit = notimpl
            member __.RedoDescription: string = notimpl
            member __.RedoStack: ITextUndoTransaction seq = notimpl
            member __.State: TextUndoHistoryState = notimpl
            member __.Undo(_count: int): unit = notimpl
            member __.UndoDescription: string = notimpl
            [<CLIEvent>]
            member __.UndoRedoHappened: IEvent<EventHandler<TextUndoRedoEventArgs>, _> = notimpl
            member __.UndoStack: ITextUndoTransaction seq = notimpl
            [<CLIEvent>]
            member __.UndoTransactionCompleted: IEvent<EventHandler<TextUndoTransactionCompletedEventArgs>, _> = notimpl            
    }

let createTextUndoHistoryRegistry() =
    {
        new ITextUndoHistoryRegistry with
            member __.AttachHistory(_context: obj, _history: ITextUndoHistory): unit = notimpl
            member __.GetHistory(_context: obj): ITextUndoHistory = notimpl
            member __.RegisterHistory(context: obj): ITextUndoHistory = 
                createTextUndoHistory()
            
            member __.RemoveHistory(_history: ITextUndoHistory): unit = notimpl
            member __.TryGetHistory(_context: obj, _history: byref<ITextUndoHistory>): bool = notimpl            
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
