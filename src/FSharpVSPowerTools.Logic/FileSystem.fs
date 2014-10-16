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
        member __.FileStreamReadShim fileName = 
            getOpenDocContent fileName
            |> Option.map (fun bytes -> new MemoryStream (bytes) :> Stream)
            |> Option.getOrTry (fun () -> defaultFileSystem.FileStreamReadShim fileName)
        
        member __.ReadAllBytesShim fileName =
            getOpenDocContent fileName 
            |> Option.getOrTry (fun () -> defaultFileSystem.ReadAllBytesShim fileName)
        
        member __.GetLastWriteTimeShim fileName =
            openDocumentsTracker.TryFindOpenDocument fileName
            |> Option.bind (fun doc ->
                if doc.Document.IsDirty then
                    Some doc.LastChangeTime
                else None)
            |> Option.getOrTry (fun () -> defaultFileSystem.GetLastWriteTimeShim fileName)
        
        member __.GetTempPathShim() = defaultFileSystem.GetTempPathShim()
        member __.FileStreamCreateShim fileName = defaultFileSystem.FileStreamCreateShim fileName
        member __.FileStreamWriteExistingShim fileName = defaultFileSystem.FileStreamWriteExistingShim fileName
        member __.GetFullPathShim fileName = defaultFileSystem.GetFullPathShim fileName
        member __.IsInvalidPathShim fileName = defaultFileSystem.IsInvalidPathShim fileName
        member __.IsPathRootedShim fileName = defaultFileSystem.IsPathRootedShim fileName
        member __.SafeExists fileName = defaultFileSystem.SafeExists fileName
        member __.FileDelete fileName = defaultFileSystem.FileDelete fileName
        member __.AssemblyLoadFrom fileName = defaultFileSystem.AssemblyLoadFrom fileName
        member __.AssemblyLoad assemblyName = defaultFileSystem.AssemblyLoad assemblyName