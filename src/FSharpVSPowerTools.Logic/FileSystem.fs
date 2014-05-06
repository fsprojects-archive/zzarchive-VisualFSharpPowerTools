namespace FSharpVSPowerTools

open System
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.Text
open System.IO
open FSharpVSPowerTools.ProjectSystem

type Version = int

type FileSystem (openDocumentsTracker: OpenDocumentsTracker) =
    static let defaultFileSystem = Shim.FileSystem

    let getOpenDocVersion (fileName: string) =
        openDocumentsTracker.TryFindOpenDocument fileName
        |> Option.map (fun snapshot -> snapshot.Version.VersionNumber)
            
    let getOpenDocContent (fileName: string) =
        openDocumentsTracker.TryFindOpenDocument fileName
        |> Option.map (fun snapshot -> snapshot.GetText())
        |> Option.map Encoding.UTF8.GetBytes

    interface IFileSystem with
        // Implement the service to open files for reading and writing
        member x.FileStreamReadShim fileName = 
            getOpenDocContent fileName
            |> Option.map (fun bytes -> new MemoryStream (bytes) :> Stream)
            |> Option.getOrElse (defaultFileSystem.FileStreamReadShim fileName)

        member x.FileStreamCreateShim fileName = defaultFileSystem.FileStreamCreateShim fileName
        member x.FileStreamWriteExistingShim fileName = defaultFileSystem.FileStreamWriteExistingShim fileName
        
        member x.ReadAllBytesShim fileName =
            getOpenDocContent fileName |> Option.getOrElse (defaultFileSystem.ReadAllBytesShim fileName)
        
        // Implement the service related to temporary paths and file time stamps
        member x.GetTempPathShim() = defaultFileSystem.GetTempPathShim()
        
        member x.GetLastWriteTimeShim fileName =
            match getOpenDocVersion fileName with
            | Some ver ->
                let dt = DateTime (int64 ver)
                debug "[VFS] %s: %d" fileName dt.Ticks
                dt
            | None -> defaultFileSystem.GetLastWriteTimeShim fileName
        
        member x.GetFullPathShim fileName = defaultFileSystem.GetFullPathShim fileName
        member x.IsInvalidPathShim fileName = defaultFileSystem.IsInvalidPathShim fileName
        member x.IsPathRootedShim fileName = defaultFileSystem.IsPathRootedShim fileName
        // Implement the service related to file existence and deletion
        member x.SafeExists fileName = defaultFileSystem.SafeExists fileName
        member x.FileDelete fileName = defaultFileSystem.FileDelete fileName
        // Implement the service related to assembly loading, used to load type providers and for F# interactive.
        member x.AssemblyLoadFrom fileName = defaultFileSystem.AssemblyLoadFrom fileName
        member x.AssemblyLoad(assemblyName) = defaultFileSystem.AssemblyLoad assemblyName

