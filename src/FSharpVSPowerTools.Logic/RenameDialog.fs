namespace FSharpVSPowerTools.Refactoring
 
open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools

type RenameDialog = FSharpx.XAML<"RenameDialog.xaml">

type RenameDialogModel(originalName: string, symbol: FSharpSymbol) =
    let mutable name = originalName
    let errorsChanged = Event<_,_>()

    let validate name = 
        let cleanUpSymbol (s: string) = s.Replace(")", "").Replace("(", "").Trim()
        debug "[Rename Refactoring] Check the following name: %s" name
        let name = name.Trim()
        match name with
        | "" -> Choice2Of2 Resource.validatingEmptyName
        | _ when name = originalName -> Choice2Of2 Resource.validatingOriginalName
        | _ ->
            match symbol with
            | :? FSharpUnionCase ->
                // Union case shouldn't be lowercase
                if isIdentifier name && not (String.IsNullOrEmpty(name) || Char.IsLower(name.[0])) then
                    Choice1Of2()
                else
                    Choice2Of2 Resource.validatingUnionCase
            | :? FSharpMemberFunctionOrValue as v when isOperator (cleanUpSymbol v.DisplayName)  ->
                if isOperator name then Choice1Of2() 
                else Choice2Of2 Resource.validatingOperator
            | _ -> 
                if isIdentifier name then Choice1Of2()
                else Choice2Of2 Resource.validatingIdentifier

    let mutable validationResult = validate name
    member x.Result = validationResult

    member x.Name 
        with get() = name 
        and set (v: string) =
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
    let loadRenameDialog (viewModel: RenameDialogModel) =
        let window = RenameDialog()
        // Use this until we are able to do validation directly
        window.txtName.TextChanged.Add(fun _ ->
            window.btnOk.IsEnabled <- not (viewModel :> INotifyDataErrorInfo).HasErrors
             )
        window.btnOk.Click.Add(fun _ -> 
            match viewModel.Result with
            | Choice1Of2 _ ->
                window.Root.DialogResult <- Nullable true
                window.Root.Close()
            | Choice2Of2 errorMsg ->
                window.Root.DialogResult <- Nullable false
                MessageBox.Show(errorMsg, Resource.vsPackageTitle, MessageBoxButton.OK, MessageBoxImage.Error) |> ignore)
        window.btnCancel.Click.Add(fun _ -> 
            window.Root.DialogResult <- Nullable false
            window.Root.Close())
        window.Root.DataContext <- viewModel
        window.Root.Loaded.Add (fun _ -> window.txtName.SelectAll())
        window.Root
 