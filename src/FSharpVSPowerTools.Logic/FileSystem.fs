namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.IO
open FSharpVSPowerTools.ProjectSystem

type Version = int

type FileSystem (openDocumentsTracker: OpenDocumentsTracker) =
    static let defaultFileSystem = Shim.FileSystem

    let getOpenDocContent (fileName: string) =
        openDocumentsTracker.TryFindOpenDocument fileName
        |> Option.map (fun doc -> doc.Snapshot.GetText() |> doc.Encoding.GetBytes)

    interface IFileSystem with
        member x.FileStreamReadShim fileName = 
            getOpenDocContent fileName
            |> Option.map (fun bytes -> new MemoryStream (bytes) :> Stream)
            |> Option.getOrTry (fun () -> defaultFileSystem.FileStreamReadShim fileName)
        
        member x.ReadAllBytesShim fileName =
            getOpenDocContent fileName 
            |> Option.getOrTry (fun () -> defaultFileSystem.ReadAllBytesShim fileName)
        
        member x.GetLastWriteTimeShim fileName =
            openDocumentsTracker.TryFindOpenDocument fileName
            |> Option.bind (fun doc ->
                if doc.Document.IsDirty then
                    Some doc.LastChangeTime
                else None)
            |> Option.getOrTry (fun () -> defaultFileSystem.GetLastWriteTimeShim fileName)
        
        member x.GetTempPathShim() = defaultFileSystem.GetTempPathShim()
        member x.FileStreamCreateShim fileName = defaultFileSystem.FileStreamCreateShim fileName
        member x.FileStreamWriteExistingShim fileName = defaultFileSystem.FileStreamWriteExistingShim fileName
        member x.GetFullPathShim fileName = defaultFileSystem.GetFullPathShim fileName
        member x.IsInvalidPathShim fileName = defaultFileSystem.IsInvalidPathShim fileName
        member x.IsPathRootedShim fileName = defaultFileSystem.IsPathRootedShim fileName
        member x.SafeExists fileName = defaultFileSystem.SafeExists fileName
        member x.FileDelete fileName = defaultFileSystem.FileDelete fileName
        member x.AssemblyLoadFrom fileName = defaultFileSystem.AssemblyLoadFrom fileName
        member x.AssemblyLoad assemblyName = defaultFileSystem.AssemblyLoad assemblyName