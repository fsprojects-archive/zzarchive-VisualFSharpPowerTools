namespace FSharpVSPowerTools.Refactoring
 
open System
open System.IO
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools

open FSharp.ViewModule
open FSharp.ViewModule.Validation

type RenameDialog = FsXaml.XAML<"RenameDialog.xaml">

type RenameDialogModel(originalName: string, symbol: Symbol, fSharpSymbol: FSharpSymbol) as self =
    inherit ViewModelBase()

    let validateSymbols name =
        let check validationCheck error = if validationCheck then None else Some error

        debug "[Rename Refactoring] Check the following name: %s" name
        let name = name.Trim()
        match symbol.Kind, fSharpSymbol with
        | _, :? FSharpUnionCase ->
            // Union cases shouldn't be lowercase
            check (isIdentifier name && not (String.IsNullOrEmpty(name) || Char.IsLower(name.[0]))) Resource.validatingUnionCase 
        | _, :? FSharpActivePatternCase ->
                // Different from union cases, active patterns don't accept double-backtick identifiers
                check (isIdentifier name && not (String.IsNullOrEmpty name) && Char.IsUpper(name.[0])) Resource.validatingActivePattern
        | Operator, _ -> 
            check (isOperator name) Resource.validatingOperator
        | GenericTypeParameter, _ -> 
            check (isGenericTypeParameter name) Resource.validatingGenericTypeParameter
        | StaticallyResolvedTypeParameter, _ ->
            check (isStaticallyResolvedTypeParameter name) Resource.validatingStaticallyResolvedTypeParameter
        | (Ident | Other), _ ->
            check (isIdentifier name) Resource.validatingIdentifier

    // Using validation with custom name
    let validateName = 
        validate "Name" 
        >> notNullOrWhitespace 
        >> fixErrorsWithMessage Resource.validatingEmptyName
        >> notEqual originalName
        >> fixErrorsWithMessage Resource.validatingOriginalName
        >> custom validateSymbols
        >> result

    let name = self.Factory.Backing(<@@ self.Name @@>, originalName, validateName)
    
    let location = 
        let fullName = fSharpSymbol.FullName
        let displayName = fSharpSymbol.DisplayName
        if fullName.EndsWith displayName then
            let locationLength = max 0 (fullName.Length - (displayName.Length + 1))
            fullName.Remove locationLength
        else fullName

    member x.Name with get() = name.Value and set(v) = name.Value <- v
    member x.Location with get() = location
    
[<RequireQualifiedAccess>]
module UI =
    let loadRenameDialog (viewModel: RenameDialogModel) owner =
        let window = RenameDialog().CreateRoot()
        window.Owner <- owner
        window.DataContext <- viewModel        
        window
 