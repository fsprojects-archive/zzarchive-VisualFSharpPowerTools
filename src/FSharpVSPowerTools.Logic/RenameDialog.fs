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

type RenameDialogModel(originalName: string, symbol: FSharpSymbol, project: ProjectProvider) =
    let mutable name = originalName
    let errorsChanged = Event<_,_>()

    let isFileInCurrentProject ffileName =
        let filePath = Path.GetFullPath ffileName
        // NB: this isn't a foolproof way to match two paths
        project.SourceFiles |> Array.exists ((=) filePath)

    let validate name = 
        let cleanUpSymbol (s: string) = s.Replace(")", "").Replace("(", "").Trim()
        debug "[Rename Refactoring] Check the following name: %s" name
        let name = name.Trim()
        match name with
        | "" -> Choice2Of2 "Empty names are not allowed."
        | _ when name = originalName -> Choice2Of2 "New name is the same as the original."
        | _ ->
            match symbol.DeclarationLocation with
            // TODO: this should be determined before opening rename dialog
            | Some loc when not <| isFileInCurrentProject loc.FileName ->
                Choice2Of2 "Can't rename. The symbol isn't defined in current project."
            | _ ->
                match symbol with
                | :? FSharpUnionCase ->
                    // Union case shouldn't be lowercase
                    if isIdentifier name && not (String.IsNullOrEmpty(name) || Char.IsLower(name.[0])) then
                        Choice1Of2()
                    else
                        Choice2Of2 "Invalid name for union cases."
                | :? FSharpMemberFunctionOrValue as v when isOperator (cleanUpSymbol v.DisplayName)  ->
                    if isOperator name then Choice1Of2() 
                    else Choice2Of2 "Invalid name for operators."
                | _ -> if isIdentifier name then Choice1Of2()
                       else Choice2Of2 "Invalid name for identifiers."

    let mutable validationResult = validate name
    member x.Result = validationResult

    member x.Name with get() = name 
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
                MessageBox.Show(errorMsg, "F# Power Tools") |> ignore)
        window.btnCancel.Click.Add(fun _ -> 
            window.Root.DialogResult <- Nullable false
            window.Root.Close())
        window.Root.DataContext <- viewModel
        window.Root
 