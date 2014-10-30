namespace FSharpVSPowerTools.Folders

open System.IO
open FSharpVSPowerTools
open FSharp.ViewModule
open FSharp.ViewModule.Validation

type MoveToFolderDialog = FsXaml.XAML<"MoveToFolderDialog.xaml", ExposeNamedProperties=true>

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
        | Some folder -> 
            match folder.Name with
            | n when System.String.IsNullOrEmpty(n) -> Some Resource.validatingEmptyName
            | _ -> 
                resources.FileNames
                |> List.map (fun fn -> Path.Combine(folder.FullPath, fn))
                |> List.filter File.Exists
                |> function 
                   | [] -> None
                   | files -> Some (files |> List.reduce (fun acc fn -> acc + (sprintf "File %s already exists.\n" fn)))
    
    let originalFolder: Folder option = None
    let folder = self.Factory.Backing(<@@ self.SelectedFolder @@>, originalFolder, custom folderSelectedAndExists)
    
    member __.WindowTitle = 
        let fileCount = List.length resources.FileNames
        sprintf "%s - Move %d item%s" Resource.vsPackageTitle fileCount (if fileCount = 1 then "" else "s")
    
    member __.Root = resources.Root
    
    member __.SelectedFolder 
        with get () = folder.Value
        and set (v) = folder.Value <- v
