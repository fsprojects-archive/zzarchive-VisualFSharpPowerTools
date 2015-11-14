namespace FSharpVSPowerTools.Navigation

open System.ComponentModel.Composition
open System
open System.Collections.Concurrent
open Microsoft.VisualStudio.Shell
open EnvDTE80
open Microsoft.VisualStudio.Shell.Interop
open EnvDTE
open FSharpVSPowerTools
open System.Security.Cryptography
open System.Text
open Nessos.FsPickler
open System.IO
open System.Diagnostics
open System.Threading
open FSharpVSPowerTools.ProjectSystem

type Source = string
type FilePath = string

type FileDescriptor =
    { Path: FilePath
      LastWriteTime: DateTime }

type FileNavigableItems =
    { Descriptor: FileDescriptor
      Items: NavigableItem[] }

[<Export>]
type NavigableItemCache
    [<ImportingConstructor>]
    (
        [<Import(typeof<SVsServiceProvider>)>] serviceProvider: System.IServiceProvider
    ) =
    
    let cache = ConcurrentDictionary<FilePath, FileNavigableItems>()
    let dirty = ref false
    let pickler = FsPickler.CreateBinarySerializer()

    let solutionPathHash (solutionPath: FilePath): string =
        use digest = SHA1.Create()
        let bytes = Encoding.UTF8.GetBytes solutionPath
        digest.ComputeHash bytes |> Array.toHexString

    let cacheFilePath solutionPath =
        let solutionHash = solutionPathHash solutionPath
        let root = Environment.ExpandEnvironmentVariables @"%LOCALAPPDATA%\VisualFSharpPowerTools\SolutionCaches"
        root </> solutionHash </> "navigable_items.cache"

    let loadFromDisk (solutionPath: FilePath) =
        VSUtils.protect <| fun _ ->
            cache.Clear()
            let sw = Stopwatch.StartNew()
            let filePath = cacheFilePath solutionPath
            
            let items =
                if File.Exists filePath then
                    use file = File.OpenRead filePath
                    let items: FileNavigableItems[] = pickler.Deserialize file
                    items
                else [||]
            
            for item in items do
                match File.tryGetLastWriteTime item.Descriptor.Path with
                | Some lastWriteTime when item.Descriptor.LastWriteTime = lastWriteTime ->
                    cache.[item.Descriptor.Path] <- item
                | _ -> ()

            sw.Stop()
            Logging.logInfo <| fun _ -> 
                sprintf "[NavigableItemCache] Loaded: %d items for %d files. Sutable: %d items for %d files. Elapsed %O" 
                        (items |> Array.sumBy (fun x -> x.Items.Length)) items.Length
                        (cache.Values |> Seq.sumBy (fun x -> x.Items.Length)) cache.Count
                        sw.Elapsed

    let saveToDisk (solutionPath: FilePath) =
        if !dirty then
            dirty := false
            VSUtils.protect <| fun _ ->
                let sw = Stopwatch.StartNew()
                let items = cache.Values |> Seq.toArray
                let filePath = cacheFilePath solutionPath
                Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore
                use file = new FileStream (filePath, FileMode.Create, FileAccess.Write, FileShare.Read)
                pickler.Serialize (file, items)
                sw.Stop()
                Logging.logInfo <| fun _ -> 
                    sprintf "[NavigableItemCache] Saved %d items for %d files to %s in %O" 
                            (items |> Array.sumBy (fun x -> x.Items.Length)) items.Length filePath sw.Elapsed

    let mutable solutionEvents: SolutionEvents = null
    let mutable afterSolutionClosing: _dispSolutionEvents_AfterClosingEventHandler = null
    let mutable solutionOpened: _dispSolutionEvents_OpenedEventHandler = null
    let tryGetSolutionPath (dte: DTE) = Option.attempt (fun () -> dte.Solution.FullName)  
    
    let dte = lazy(
        let dte = serviceProvider.GetService<DTE, SDTE>()
        afterSolutionClosing <- _dispSolutionEvents_AfterClosingEventHandler (fun () -> cache.Clear())
        let tryLoadFromFile() = tryGetSolutionPath dte |> Option.iter loadFromDisk
        solutionOpened <- _dispSolutionEvents_OpenedEventHandler tryLoadFromFile
        
        match dte.Events with
            | :? Events2 as events ->
                solutionEvents <- events.SolutionEvents
                solutionEvents.add_Opened solutionOpened
                solutionEvents.add_AfterClosing afterSolutionClosing
            | _ -> ()
        tryLoadFromFile()
        dte)
    
    let saveTimer = new Timer((fun _ -> tryGetSolutionPath dte.Value |> Option.iter saveToDisk), null, 0, 5000)

    member __.TryGet (file: FileDescriptor): NavigableItem[] option =
        match cache.TryGetValue file.Path with
        | true, x when x.Descriptor.LastWriteTime = file.LastWriteTime -> Some x.Items
        | _ -> None
    
    member __.Add (file: FileDescriptor, items: NavigableItem[]): unit = 
        cache.[file.Path] <- { Descriptor = file; Items = items }
        dirty := true
    
    member __.Remove (filePath: FilePath): unit = cache.TryRemove filePath |> ignore

    interface IDisposable with
        member __.Dispose() =
            saveTimer.Dispose()
            solutionEvents.remove_Opened solutionOpened
            solutionEvents.remove_AfterClosing afterSolutionClosing

    