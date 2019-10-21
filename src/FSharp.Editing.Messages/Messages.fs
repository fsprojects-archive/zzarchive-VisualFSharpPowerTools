namespace FSharp.Editing.Messages

type RequestId = Guid
type FilePath = string
type DocumentContent = string

type Project =
    { FileName: FilePath
      Files: FilePath list
      ReferencedProjects: FilePath list
      CompilerOptions: string list }

[<RequireQualifiedAccess>]
type ServerCapability =
    | SyntaxHighlighting
    | HighlighIdentifier
    | ImplementInterface

type ClientCallbackAddress = string

[<RequireQualifiedAccess>]
type ClientNotification =
      /// Reset all server state.
    | Reset
      /// A new project loaded or a known project changed in any way (name, file list, references, etc.)
    | ProjectLoaded of Project
      /// Previously loaded project was unloaded from IDE or deleted from disk so it should not be part of solution anymore.
    | ProjectUnloaded of FilePath
      /// Document opened in an IDE for the first time. No need to send its content since server can read it from disk.
    | DocumentOpened of FilePath
      /// Document closed. Server can stop tracking its content in a special way and just read it from disk.
    | DocumentClosed of FilePath
    | DocumentRenamed of oldFile: FilePath * newFile: FilePath
      /// Opened document changed in IDE without saving to disk. Full new content should be sent 
      /// (for now. Later we can add incremental changes support).
    | DocumentChanged of FilePath * DocumentContent
      /// Client subscribing on a set of server notifications, passing an address to which them should be sent.
    | Subscribe of Set<ServerCapability> * ClientCallbackAddress
    | CancelRequest of RequestId

type ClientRequest = 
    { RequestId: RequestId }

type ServerResponse = 
    { RequestId: RequestId }

[<RequireQualifiedAccess>]
type ClientMessage =
    | Request of ClientRequest
    | Notification of ClientNotification

type SyntaxHighlightingInfo = 
    { ClassificationRanges: unit list }

[<RequireQualifiedAccess>]
type ServerNotification =
    | SyntaxHighlightingChanged of file: FilePath * SyntaxHighlightingInfo

[<RequireQualifiedAccess>]
type ServerMessage =
    | Response of ServerResponse
    | Notification of ServerNotification