namespace FSharpVSPowerTools.Refactoring
 
open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools
open FSharp.CompilerBinding

type RenameDialog = FsXaml.XAML<"RenameDialog.xaml">

type RenameDialogModel(originalName: string, symbol: Symbol, fSharpSymbol: FSharpSymbol) =
    let mutable name = originalName
    let errorsChanged = Event<_,_>() 
    
    let location = 
        let fullName = fSharpSymbol.FullName
        let displayName = fSharpSymbol.DisplayName
        if fullName.EndsWith displayName then
            let locationLength = max 0 (fullName.Length - (displayName.Length + 1))
            fullName.Remove locationLength
        else fullName

    let validate name = 
        debug "[Rename Refactoring] Check the following name: %s" name
        let name = name.Trim()
        match name with
        | "" -> Choice2Of2 Resource.validatingEmptyName
        | _ when name = originalName -> Choice2Of2 Resource.validatingOriginalName
        | _ ->
            match symbol.Kind, fSharpSymbol with
            | _, :? FSharpUnionCase ->
                // Union cases shouldn't be lowercase
                if isIdentifier name && not (String.IsNullOrEmpty(name) || Char.IsLower(name.[0])) then
                    Choice1Of2()
                else
                    Choice2Of2 Resource.validatingUnionCase
            | _, :? FSharpActivePatternCase ->
                    // Different from union cases, active patterns don't accept double-backtick identifiers
                    if isIdentifier name && not (String.IsNullOrEmpty name) && Char.IsUpper(name.[0]) then
                        Choice1Of2()
                    else
                        Choice2Of2 Resource.validatingActivePattern
            | Operator, _ ->
                if isOperator name then Choice1Of2() 
                else Choice2Of2 Resource.validatingOperator
            | GenericTypeParameter, _ ->
                if isGenericTypeParameter name then Choice1Of2()
                else Choice2Of2 Resource.validatingGenericTypeParameter
            | StaticallyResolvedTypeParameter, _ ->
                if isStaticallyResolvedTypeParameter name then Choice1Of2()
                else Choice2Of2 Resource.validatingStaticallyResolvedTypeParameter
            | (Ident | Other), _ ->
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

    member x.Location = location
    
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
        let window = RenameDialog().CreateRoot()

        // Provides access to "code behind" style work
        let accessor = RenameDialog.Accessor(window)

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
                messageBoxError errorMsg)
        accessor.btnCancel.Click.Add(fun _ -> 
            window.DialogResult <- Nullable false
            window.Close())
        window.DataContext <- viewModel
        window.Loaded.Add (fun _ -> accessor.txtName.SelectAll())
        window
 