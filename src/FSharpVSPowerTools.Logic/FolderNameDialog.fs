namespace FSharpVSPowerTools.Folders

open FSharpVSPowerTools
open FSharp.ViewModule
open FSharp.ViewModule.Validation

type NewFolderNameDialog = FsXaml.XAML<"FolderNameDialog.xaml">

[<NoEquality; NoComparison>]
type NewFolderNameDialogResources =
    { WindowTitle : string
      FolderNames : Set<string>
      OriginalName : string }

type NewFolderNameDialogModel(resources :NewFolderNameDialogResources) as self =
    inherit ViewModelBase()

    let validateExists (folderName : string) =
        match folderName.Trim() with
            | a when resources.FolderNames.Contains(a) -> Some Resource.validationFolderWithGivenNameAlreadyExists
            | _ -> None

    let validateName = 
        validate "Name" 
            >> notNullOrWhitespace 
            >> fixErrorsWithMessage Resource.validatingEmptyName
            >> custom validateExists
            >> result

    let name = self.Factory.Backing(<@@ self.Name @@>, resources.OriginalName, validateName)

    member x.WindowTitle = resources.WindowTitle
    member x.Name with get() = name.Value and set(v) = name.Value <- v
