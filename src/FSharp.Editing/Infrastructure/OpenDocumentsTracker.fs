namespace FSharp.Editing.Infrastructure

open System.Collections.Generic


type IOpenDocument =
    abstract Text : Lazy<string>

type IOpenDocumentsTracker<'OpenDoc when 'OpenDoc :> IOpenDocument> =
    abstract MapOpenDocuments: (KeyValuePair<string, 'OpenDoc> -> 'a) -> seq<'a>
    abstract TryFindOpenDocument: string -> 'OpenDoc option
    abstract TryGetDocumentText: string -> string option
    abstract DocumentChanged: IEvent<string>
    abstract DocumentClosed: IEvent<string>

