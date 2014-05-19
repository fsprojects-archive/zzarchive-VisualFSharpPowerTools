namespace FSharpVSPowerTools.Folders

open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools

type MoveToFolderDialog = FsXaml.XAML< "MoveToFolderDialog.xaml" >

type Folder = 
    { Name: string
      FullPath: string
      IsProject: bool
      SubFolders: Folder list }

type MoveToFolderDialogResources = 
    { FileNames: string list
      Root: Folder list }

type MoveToFolderDialogModel(resources: MoveToFolderDialogResources) = 
    let mutable folder = None
    
    let validate (folder: Folder option) = 
        match folder with
        | None -> Choice2Of2 Resource.validatingEmptyName
        | Some folder ->
            match folder.Name with
            | "" -> Choice2Of2 Resource.validatingEmptyName
            | _ -> 
                resources.FileNames 
                |> List.map (fun fn -> Path.Combine (folder.FullPath, fn))
                |> List.filter File.Exists
                |> function
                   | [] -> Choice1Of2()
                   | existentFiles -> 
                        Choice2Of2 (existentFiles 
                                    |> List.reduce (fun acc fn -> acc + (sprintf "File %s already exists.\n" fn)))
    
    let mutable validationResult = validate folder
    member x.Result = validationResult
    
    member x.WindowTitle = 
        let fileCount = List.length resources.FileNames
        sprintf "%s - Move %d item%s" Resource.vsPackageTitle fileCount (if fileCount = 1 then "" else "s")
    
    member x.Root = resources.Root
    
    member x.SelectedFolder 
        with get () = folder
        and set (v: Folder option) = 
            folder <- v
            validationResult <- validate folder
