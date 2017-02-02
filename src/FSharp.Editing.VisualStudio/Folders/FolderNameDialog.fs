namespace FSharp.Editing.VisualStudio.Folders

open System.IO
open FSharp.Editing.VisualStudio
open ViewModule
open ViewModule.FSharp
open ViewModule.Validation.FSharp

type NewFolderNameDialog = FsXaml.XAML< @"Gui/FolderNameDialog.xaml">

[<NoEquality; NoComparison>]
type NewFolderNameDialogResources = 
    { WindowTitle: string
      FolderNames: Set<string>
      CheckUniqueFolderNames: bool
      OriginalName: string }

type NewFolderNameDialogModel(resources: NewFolderNameDialogResources) as self = 
    inherit ViewModelBase()
    
    let validateFolderName (folderName: string) =
        if (folderName.IndexOfAny <| Path.GetInvalidFileNameChars()) >= 0
        then Some Resource.validationInvalidFolderName
        else None

    let validateExists (folderName: string) = 
        match folderName.Trim() with
        | a when resources.FolderNames.Contains(a) -> Some Resource.validationFolderWithGivenNameAlreadyExists
        | _ -> None
    
    let validateName = 
        validate "Name"
        >> notNullOrWhitespace
        >> fixErrorsWithMessage Resource.validatingEmptyName
        >> (if resources.CheckUniqueFolderNames then (custom validateExists) else id)
        >> custom validateFolderName
        >> result
    
    let name = self.Factory.Backing(<@@ self.Name @@>, resources.OriginalName, validateName)
    member __.WindowTitle = resources.WindowTitle
    
    member __.Name 
        with get () = name.Value
        and set (v) = name.Value <- v
