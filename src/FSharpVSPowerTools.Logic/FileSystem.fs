namespace FSharpVSPowerTools

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open EnvDTE
open System.Text
open System.IO

type FileSystem (dte: DTE) =
    static let defaultFileSystem = Shim.FileSystem

    let getDirtyDoc (fileName: string) =
        maybe {
            let openDocs = dte.Documents |> Seq.cast<Document> |> Seq.map (fun doc -> doc, doc.FullName, doc.Saved) |> Seq.toList
            let! doc, _, _ = openDocs |> List.tryFind (fun (_, fullName, saved) -> fullName = fileName && not saved)
            return doc
        }

    let getDirtyFileContent (fileName: string) =
        maybe {
            let! doc = getDirtyDoc fileName
            let textDoc = doc.Object("TextDocument") :?> TextDocument
            return textDoc.StartPoint.CreateEditPoint().GetText(textDoc.EndPoint) }

    let getDirtyFileBinaryContent fileName =
        debug "[VFS] Getting %s content..." fileName
        getDirtyFileContent fileName |> Option.map Encoding.UTF8.GetBytes

    interface IFileSystem with
        // Implement the service to open files for reading and writing
        member x.FileStreamReadShim fileName = 
            match getDirtyFileBinaryContent fileName with
            | Some bytes -> new MemoryStream (bytes) :> _
            | _ -> defaultFileSystem.FileStreamReadShim fileName

        member x.FileStreamCreateShim fileName = defaultFileSystem.FileStreamCreateShim fileName
        member x.FileStreamWriteExistingShim fileName = defaultFileSystem.FileStreamWriteExistingShim fileName
        
        member x.ReadAllBytesShim fileName =
            getDirtyFileBinaryContent fileName |> Option.getOrElse (defaultFileSystem.ReadAllBytesShim fileName)
        
        // Implement the service related to temporary paths and file time stamps
        member x.GetTempPathShim() = defaultFileSystem.GetTempPathShim()
        member x.GetLastWriteTimeShim fileName =
            match getDirtyFileContent fileName with
            | Some content -> 
                let h = hash content
                let dt = System.DateTime(abs (int64 h % 103231L))
                debug "[VFS] %s: %d" fileName dt.Ticks
                dt
            | _ -> defaultFileSystem.GetLastWriteTimeShim fileName
        member x.GetFullPathShim fileName = defaultFileSystem.GetFullPathShim fileName
        member x.IsInvalidPathShim fileName = defaultFileSystem.IsInvalidPathShim fileName
        member x.IsPathRootedShim fileName = defaultFileSystem.IsPathRootedShim fileName
        // Implement the service related to file existence and deletion
        member x.SafeExists fileName = defaultFileSystem.SafeExists fileName
        member x.FileDelete fileName = defaultFileSystem.FileDelete fileName
        // Implement the service related to assembly loading, used to load type providers and for F# interactive.
        member x.AssemblyLoadFrom fileName = defaultFileSystem.AssemblyLoadFrom fileName
        member x.AssemblyLoad(assemblyName) = defaultFileSystem.AssemblyLoad assemblyName

