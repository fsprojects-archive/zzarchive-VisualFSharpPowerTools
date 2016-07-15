namespace FSharp.Editing.Messages

type [<Measure>] requestId
type RequestId = int64<requestId>
type FilePath = string
type DocumentContent = string

type Project =
    { FileName: FilePath
      Files: FilePath list
      ReferencedProjects: FilePath list
      CompilerOptions: string list }

type ServerCapability =
    | SyntaxHighlighting
    | HighlighIdentifier
    | ImplementInterface

type ClientNotification =
      /// New solution loaded or new folder opened. All server state should be discarded.
    | Cleanup
      /// A new project loaded or a known project changed in any way (name, file list, references, etc.)
    | ProjectLoaded of Project
      /// Previously loaded project was unloaded from IDE or deleted from disk so it should not be part of solution anymore.
    | ProjectUnloaded
      /// Document opened in an IDE for the first time. No need to send its content since server can read it from disk.
    | DocumentOpened of file: FilePath
      /// Document closed. Server can stop tracking its content in a special way and just read it from disk.
    | DocumentClosed of file: FilePath
    | DocumentRenamed of oldFile: FilePath * newFile: FilePath
      /// Opened document changed in IDE without saving to disk. Full new content should be sent 
      /// (for now. Later we can add incremental changes support).
    | DocumentChanged of file: FilePath * content: DocumentContent
    | Subscribe of ServerCapability Set

type SyntaxHighlightingInfo = 
    { ClassificationRanges: unit list }

type ServerNotification =
    | SyntaxHighlightingChanged of file: FilePath * SyntaxHighlightingInfo

type RequestMessage = 
    { RequestId: RequestId }

type ResponseMessage = 
    { RequestId: RequestId }