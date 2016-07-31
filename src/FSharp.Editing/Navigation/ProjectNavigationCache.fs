namespace FSharp.Editing.Navigation

open System
open System.IO
open System.Text
open System.Threading
open System.Collections.Concurrent
open System.Security.Cryptography
open MBrace.FsPickler
open FSharp.Editing

type Source = string

type FileDescriptor =
    { Path: FilePath
      LastWriteTime: DateTime }

type FileNavigableItems =
    { Descriptor: FileDescriptor
      Items: NavigationItem[] }


// TODO - the cache doesn't have a way of checking and clearing out projects that are no longer on disk
// i.e. they were deleted or the project name was changed
type ProjectNavigationCache (projectPath:FilePath) =
    let cache = ConcurrentDictionary<FilePath, FileNavigableItems>(StringComparer.Ordinal)
    let dirty = ref false
    let pickler = FsPickler.CreateBinarySerializer()

    let projectPathHash (projectPath: FilePath): string =
        use digest = SHA1.Create()
        let bytes = Encoding.UTF8.GetBytes projectPath
        digest.ComputeHash bytes |> Array.toShortHexString

    let cacheFilePath projectPath =
        let projectHash = projectPathHash projectPath
        let root = Environment.ExpandEnvironmentVariables @"%LOCALAPPDATA%/FSharp.Editing/ProjectCaches"
        root </> projectHash </> "navigable_items.cache"

    let loadFromDisk (projectPath: FilePath) =
        protect <| fun _ ->
            cache.Clear()
            let pf = Profiler()
            let filePath = cacheFilePath projectPath
            let items = pf.Time "Read from file" <| fun _ ->
                if File.Exists filePath then
                    use file = File.OpenRead filePath
                    let items: FileNavigableItems[] = pickler.Deserialize file
                    items
                else [||]
                
            let items = pf.Time "Filter by LastWriteTime" <| fun _ ->
                for item in items do
                    match File.tryGetLastWriteTime item.Descriptor.Path with
                    | Some lastWriteTime when item.Descriptor.LastWriteTime = lastWriteTime ->
                        cache.[item.Descriptor.Path] <- item
                    | _ -> ()
                items
            items |> ignore // temp - just to stop the warning
            pf.Stop()
//            Logging.logInfo <| fun _ -> 
//                sprintf "[NavigableItemCache] Loaded: %d items for %d files. Sutable: %d items for %d files %s" 
//                        (items |> Array.sumBy (fun x -> x.Items.Length)) items.Length
//                        (cache.Values |> Seq.sumBy (fun x -> x.Items.Length)) cache.Count
//                        pf.Result

    let saveToDisk (projectPath: FilePath) =
        if !dirty then
            dirty := false
            protect <| fun _ ->
                let filePath = cacheFilePath projectPath
                let pf = Profiler()
                let items = pf.Time "Save to file" <| fun _ ->
                    let items = cache.Values |> Seq.toArray
                    Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore
                    use file = new FileStream (filePath, FileMode.Create, FileAccess.Write, FileShare.Read)
                    pickler.Serialize (file, items)
                    items
                pf.Stop()
                items |> ignore // temp - just to stop the warning
//                Logging.logInfo <| fun _ -> 
//                    sprintf "[NavigableItemCache] Saved %d items for %d files to %s %s" 
//                            (items |> Array.sumBy (fun x -> x.Items.Length)) items.Length filePath pf.Result
//

//    do tryLoadFromFile()
    do loadFromDisk projectPath

//    let saveTimer = new Timer((fun _ -> tryGetSolutionPath() |> Option.iter saveToDisk), null, 0, 5000)
    let saveTimer = new Timer((fun _ -> saveToDisk projectPath), null, 0, 5000)

    member __.TryGet (file: FileDescriptor): NavigationItem[] option =
        match cache.TryGetValue file.Path with
        | true, x when x.Descriptor.LastWriteTime = file.LastWriteTime -> 
            //Logging.logInfo (fun _ -> sprintf "[NavigableItemCache] Found for %s, %O" file.Path file.LastWriteTime)
            Some x.Items
        | true, _x ->
            //Logging.logInfo (fun _ -> sprintf "[NavigableItemCache] Found with different LastWriteTime for %s, %O <> %O" 
              //                                file.Path x.Descriptor.LastWriteTime file.LastWriteTime)
            None
        | _ -> 
            //Logging.logInfo (fun _ -> sprintf "[NavigableItemCache] Not found for %s" file.Path)
            None
    
    member __.Add (file: FileDescriptor, items: NavigationItem[]): unit = 
        cache.[file.Path] <- { Descriptor = file; Items = items }
        dirty := true
    
    member __.Remove (filePath: FilePath): unit = 
        cache.TryRemove filePath |> ignore

    interface IDisposable with
        member __.Dispose () =
            saveTimer.Dispose ()
