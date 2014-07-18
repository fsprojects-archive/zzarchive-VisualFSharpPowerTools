namespace FSharpVSPowerTools.Tests

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open TestUtilities

type MockTextBuffer(content: string, ?fileName, ?contentType) as self =
    let properties = PropertyCollection()
    let fileName = defaultArg fileName @"C:\Tests.fs"
    let contentType = defaultArg contentType "F#"
    do 
        let textDocument = new MockTextDocument(fileName, self)        
        properties.AddProperty(typeof<ITextDocument>, textDocument)

    let snapshot = MockTextSnapshot(self, content)

    let changed = Event<_, _>()
    let changedHighPriority = Event<_, _>()
    let changedLowPriority = Event<_, _>()
    let changing = Event<_, _>()
    let contentTypeChanged = Event<_, _>()
    let postChanged = Event<_, _>()
    let readOnlyRegionsChanged = Event<_, _>()
    
    interface ITextBuffer with        
        member x.Properties: PropertyCollection = 
            properties

        member x.ChangeContentType(newContentType: IContentType, editTag: obj): unit = 
            failwith "Not implemented yet"

        [<CLIEvent>]
        member x.Changed: IEvent<EventHandler<TextContentChangedEventArgs>, _> = 
            changed.Publish
               
        [<CLIEvent>]
        member x.ChangedHighPriority: IEvent<EventHandler<TextContentChangedEventArgs>, _> = 
            changedHighPriority.Publish
       
        [<CLIEvent>]
        member x.ChangedLowPriority: IEvent<EventHandler<TextContentChangedEventArgs>, _> = 
            changedLowPriority.Publish
        
        [<CLIEvent>]
        member x.Changing: IEvent<EventHandler<TextContentChangingEventArgs>, _> = 
            changing.Publish
        
        member x.CheckEditAccess(): bool = 
            failwith "Not implemented yet"
        
        member x.ContentType: IContentType = 
            failwith "Not implemented yet"
        
        [<CLIEvent>]
        member x.ContentTypeChanged: IEvent<EventHandler<ContentTypeChangedEventArgs>, _> = 
            contentTypeChanged.Publish
        
        member x.CreateEdit(options: EditOptions, reiteratedVersionNumber: Nullable<int>, editTag: obj): ITextEdit = 
            failwith "Not implemented yet"
        
        member x.CreateEdit(): ITextEdit = 
            failwith "Not implemented yet"
        
        member x.CreateReadOnlyRegionEdit(): IReadOnlyRegionEdit = 
            failwith "Not implemented yet"
        
        member x.CurrentSnapshot: ITextSnapshot = 
            snapshot :> ITextSnapshot
        
        member x.Delete(deleteSpan: Span): ITextSnapshot = 
            failwith "Not implemented yet"
        
        member x.EditInProgress: bool = 
            failwith "Not implemented yet"
        
        member x.GetReadOnlyExtents(span: Span): NormalizedSpanCollection = 
            failwith "Not implemented yet"
        
        member x.Insert(position: int, text: string): ITextSnapshot = 
            failwith "Not implemented yet"
        
        member x.IsReadOnly(position: int): bool = 
            failwith "Not implemented yet"
        
        member x.IsReadOnly(position: int, isEdit: bool): bool = 
            failwith "Not implemented yet"
        
        member x.IsReadOnly(span: Span): bool = 
            failwith "Not implemented yet"
        
        member x.IsReadOnly(span: Span, isEdit: bool): bool = 
            failwith "Not implemented yet"
        
        [<CLIEvent>]
        member x.PostChanged: IEvent<EventHandler, _> = 
            postChanged.Publish
        
        [<CLIEvent>]
        member x.ReadOnlyRegionsChanged: IEvent<EventHandler<SnapshotSpanEventArgs>, _> = 
            readOnlyRegionsChanged.Publish
        
        member x.Replace(replaceSpan: Span, replaceWith: string): ITextSnapshot = 
            failwith "Not implemented yet"
        
        member x.TakeThreadOwnership(): unit = 
            failwith "Not implemented yet"
