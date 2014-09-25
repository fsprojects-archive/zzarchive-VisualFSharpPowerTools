﻿namespace FSharpVSPowerTools.Folders

open System.IO
open FSharpVSPowerTools
open FSharp.ViewModule
open FSharp.ViewModule.Validation

type MoveToFolderDialog = FsXaml.XAML< "MoveToFolderDialog.xaml" >

type Folder = 
    { Name: string
      FullPath: string
      IsProject: bool
      SubFolders: Folder list }

type MoveToFolderDialogResources = 
    { FileNames: string list
      Root: Folder list }

type MoveToFolderDialogModel(resources: MoveToFolderDialogResources) as self = 
    inherit ViewModelBase()

    let folderSelectedAndExists newFolder =
        match newFolder with
            | None -> Some Resource.validatingEmptyName
            | Some f ->
                match f.Name with
                    | null -> Some Resource.validatingEmptyName
                    | "" -> Some Resource.validatingEmptyName
                    | _ -> resources.FileNames
                            |> List.map(fun fn -> Path.Combine(f.FullPath, fn))
                            |> List.filter File.Exists
                            |> function
                               | [] -> None
                               | files -> Some (files |> List.reduce (fun acc fn -> acc + (sprintf "File %s already exists.\n" fn)))

    let validateFolder = 
        validate "SelectedFolder" 
            >> custom folderSelectedAndExists
            >> result
    let originalFolder : Folder option = None
    let folder = self.Factory.Backing(<@@ self.SelectedFolder @@>, originalFolder, validateFolder)
        
    member x.WindowTitle = 
        let fileCount = List.length resources.FileNames
        sprintf "%s - Move %d item%s" Resource.vsPackageTitle fileCount (if fileCount = 1 then "" else "s")
    
    member x.Root = resources.Root
    
    member x.SelectedFolder with get() = folder.Value and set (v) = folder.Value <- v
