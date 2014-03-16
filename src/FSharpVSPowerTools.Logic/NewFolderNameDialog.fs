namespace FSharpVSPowerTools.Folders

open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding

type NewFolderNameDialog = FsXaml.XAML<"NewFolderNameDialog.xaml">

[<NoEquality; NoComparison>]
type NewFolderNameDialogResources =
    { WindowTitle : string
      OriginalName : string }

type NewFolderNameDialogModel(_resources :NewFolderNameDialogResources) =
    let resources  = _resources

    let mutable name = resources.OriginalName

    let validate (name :string) =
        let name = name.Trim()
        match name with
        | "" -> Choice2Of2 Resource.validatingEmptyName
        | _ when name = resources.OriginalName -> Choice2Of2 Resource.validatingOriginalName
        | _ -> Choice1Of2()


    let mutable validationResult = validate name

    let errorsChanged = Event<_,_>() 

    member x.Result = validationResult

    member x.WindowTitle = resources.WindowTitle

    member x.Name
        with get() = name
        and set (v :string) =
            name <- v
            validationResult <- validate name
            errorsChanged.Trigger(x :> obj, DataErrorsChangedEventArgs("Name"))

    interface INotifyDataErrorInfo with
        member x.GetErrors _ = 
            match validationResult with
            | Choice2Of2 e -> [e]
            | _ -> []
            :> Collections.IEnumerable

        member x.HasErrors = match validationResult with Choice2Of2 _ -> true | _ -> false
        [<CLIEvent>]
        member x.ErrorsChanged = errorsChanged.Publish
        
[<RequireQualifiedAccess>]
module UI =
    let loadDialog (viewModel: NewFolderNameDialogModel) =
        let window = NewFolderNameDialog().CreateRoot()

        // Provides access to "code behind" style work
        let accessor = NewFolderNameDialog.Accessor(window)

        // Use this until we are able to do validation directly
        accessor.txtName.TextChanged.Add(fun _ ->
            accessor.btnOk.IsEnabled <- not (viewModel :> INotifyDataErrorInfo).HasErrors
             )
        accessor.btnOk.Click.Add(fun _ -> 
            match viewModel.Result with
            | Choice1Of2 _ ->
                window.DialogResult <- Nullable true
                window.Close()
            | Choice2Of2 errorMsg ->
                window.DialogResult <- Nullable false
                MessageBox.Show(errorMsg, Resource.vsPackageTitle, MessageBoxButton.OK, MessageBoxImage.Error) |> ignore)
        accessor.btnCancel.Click.Add(fun _ -> 
            window.DialogResult <- Nullable false
            window.Close())
        window.DataContext <- viewModel
        window.Loaded.Add (fun _ -> accessor.txtName.SelectAll())
        window
 