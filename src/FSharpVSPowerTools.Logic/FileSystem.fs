namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.IO
open FSharpVSPowerTools.ProjectSystem
open System.ComponentModel.Composition
open System.Collections.Generic
open System
open System.Collections.Concurrent

type Version = int

[<Export>]
type FileSystem [<ImportingConstructor>] (openDocumentsTracker: IOpenDocumentsTracker) =
    static let defaultFileSystem = Shim.FileSystem

    let fileExistsCache = ConcurrentDictionary<string, bool>()
    let fileLastWriteTimeCache = ConcurrentDictionary<string, DateTime>()

    let getOpenDocContent (fileName: string) =
        openDocumentsTracker.TryFindOpenDocument fileName
        |> Option.map (fun doc -> 
            let content = doc.Text.Value 
            content |> doc.Encoding.GetBytes)

    interface ICachingFileSystem with
        member __.ClearCache() =
            fileExistsCache.Clear()
            fileLastWriteTimeCache.Clear()

        member __.FileStreamReadShim fileName = 
            getOpenDocContent fileName
            |> Option.map (fun bytes -> new MemoryStream (bytes) :> Stream)
            |> Option.getOrTry (fun () -> defaultFileSystem.FileStreamReadShim fileName)
        
        member __.ReadAllBytesShim fileName =
            getOpenDocContent fileName 
            |> Option.getOrTry (fun () -> defaultFileSystem.ReadAllBytesShim fileName)
        
        member __.GetLastWriteTimeShim fileName =
            match fileLastWriteTimeCache.TryGetValue fileName with
            | true, x -> x
            | _ ->
                let res = 
                    openDocumentsTracker.TryFindOpenDocument fileName
                    |> Option.bind (fun doc ->
                        if doc.Document.IsDirty then
                            Some doc.LastChangeTime
                        else None)
                    |> Option.getOrTry (fun () -> defaultFileSystem.GetLastWriteTimeShim fileName)
                fileLastWriteTimeCache.[fileName] <- res
                res
        
        member __.GetTempPathShim() = defaultFileSystem.GetTempPathShim()
        member __.FileStreamCreateShim fileName = defaultFileSystem.FileStreamCreateShim fileName
        member __.FileStreamWriteExistingShim fileName = defaultFileSystem.FileStreamWriteExistingShim fileName
        member __.GetFullPathShim fileName = defaultFileSystem.GetFullPathShim fileName
        member __.IsInvalidPathShim fileName = defaultFileSystem.IsInvalidPathShim fileName
        member __.IsPathRootedShim fileName = defaultFileSystem.IsPathRootedShim fileName
        member __.SafeExists fileName = 
            match fileExistsCache.TryGetValue fileName with
            | true, x -> x
            | _ ->
                let res = defaultFileSystem.SafeExists fileName
                fileExistsCache.[fileName] <- res
                res
        member __.FileDelete fileName = defaultFileSystem.FileDelete fileName
        member __.AssemblyLoadFrom fileName = defaultFileSystem.AssemblyLoadFrom fileName
        member __.AssemblyLoad assemblyName = defaultFileSystem.AssemblyLoad assemblyName