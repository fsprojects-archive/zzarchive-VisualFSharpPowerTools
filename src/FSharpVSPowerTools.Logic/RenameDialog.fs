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

// The ViewModel for our dialog
type RenameDialogModel(originalName: string, symbol: Symbol, initializationWorkflow : Async<(ParseAndCheckResults * SymbolDeclarationLocation * FSharpSymbol) option>, renameWorkflow : (ParseAndCheckResults -> SymbolDeclarationLocation -> string-> IProgress<(string * (int*int) option)> -> Async<unit>), cts : System.Threading.CancellationTokenSource) as self =
    inherit ViewModelBase()

    // This will hold the actual rename workflow arguments after the initialization async workflow completes
    let mutable workflowArguments : (FSharpSymbol * SymbolDeclarationLocation * ParseAndCheckResults) option = None

    // Our progress.  The tuple is (status string * (optional current/max progress))
    // When progress is None, the progress bar will be indeterminate
    let progress = Progress<(string * (int*int) option)>()
    let iprogress = progress :> IProgress<(string * (int*int) option)>

    // Custom validation for the name property
    let validateSymbols name =
        let check validationCheck error = if validationCheck then None else Some error

        debug "[Rename Refactoring] Check the following name: %s" name
        let name = name.Trim()
        match workflowArguments with
        | None -> Some Resource.renameErrorMessage
        | Some(fssym, _, _) ->
            match symbol.Kind, fssym with
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

    // Complete validation chain for the name property
    let validateName = 
        validate "Name" 
        >> notNullOrWhitespace 
        >> fixErrorsWithMessage Resource.validatingEmptyName
        >> notEqual originalName
        >> fixErrorsWithMessage Resource.validatingOriginalName
        >> custom validateSymbols
        >> result

    // Backing fields for all view-bound values
    let name = self.Factory.Backing(<@@ self.Name @@>, originalName, validateName)
    let location = self.Factory.Backing(<@@ self.Location @@>, String.Empty)
    let status = self.Factory.Backing(<@@ self.Status @@>, String.Empty)
    let hasProgress = self.Factory.Backing(<@@ self.HasProgress @@>, false)
    let progressCurrent = self.Factory.Backing(<@@ self.ProgressCurrent @@>, 0)
    let progressMax = self.Factory.Backing(<@@ self.ProgressMax @@>, 100)
    let completed = self.Factory.Backing(<@@ self.Completed @@>, false)
    let initialized = self.Factory.Backing(<@@ self.Initialized @@>, false)
    let executing = self.Factory.Backing(<@@ self.Executing @@>, false)

    // We wrap up the actual async workflow in order to close us when completed as well as mark us executing while we run
    let wrappedWorkflow _ str =
        async {
            executing.Value <- true
            iprogress.Report("Performing Rename...", None)
            match workflowArguments with
            | Some(_, location, results) ->
                do! renameWorkflow results location str progress
            | _ -> ()
            self.Completed <- true
        }
    // The async command which is executed when the user clicks OK
    let execute = self.Factory.CommandAsyncParamChecked(wrappedWorkflow, (fun _ -> self.IsValid && self.Initialized), [ <@@ self.IsValid @@> ; <@@ self.Initialized @@> ], cts.Token)
    
    // Cancelling the operation should cancel all async workflows and close us
    let cancel _ = 
        cts.Cancel()
        self.Completed <- true
    let cancelCommand = self.Factory.CommandSync(cancel)

    do  
        // Force initialization to re-evaluate Name's validation by making it a dependency      
        self.DependencyTracker.AddPropertyDependency(<@@ self.Name @@>, <@@ self.Initialized @@>)

        // Handle progress updates
        progress.ProgressChanged.Subscribe(fun newProgress -> 
            status.Value <- fst newProgress
            match snd newProgress with
            | None -> 
                hasProgress.Value <- false
            | Some(newCurrent, newMax) -> 
                hasProgress.Value <- true
                progressCurrent.Value <- newCurrent
                progressMax.Value <- newMax
            ) |> ignore

        // Perform our initialization routine while reporting progress/etc as necessary
        Async.StartImmediateSafe(
            async { 
                executing.Value <- true
                iprogress.Report ("Initializing...", None)
                let! b = initializationWorkflow 
                match b with
                | Some(results, location, symbol) ->
                    workflowArguments <- Some(symbol, location, results)                    
                    iprogress.Report (String.Empty, None)
                    self.Location <- 
                        let fullName = symbol.FullName
                        let displayName = symbol.DisplayName
                        if fullName.EndsWith displayName then
                            let locationLength = max 0 (fullName.Length - (displayName.Length + 1))
                            fullName.Remove locationLength
                        else fullName
                    initialized.Value <- true
                    executing.Value <- false
                | None -> 
                    cts.Cancel()
            }, cts.Token)

    // Our bound properties
    member x.Name with get() = name.Value and set(v) = name.Value <- v
    member x.Status with get() = status.Value
    member x.Initialized with get() = initialized.Value
    member x.Executing with get() = executing.Value
    member x.HasProgress with get() = hasProgress.Value
    member x.ProgressCurrent with get() = progressCurrent.Value
    member x.ProgressMax with get() = progressMax.Value
    member x.Location with get() = location.Value and set(v) = location.Value <- v
    member x.Completed with get() = completed.Value and set(v) = completed.Value <- v

    member x.ExecuteCommand with get() = execute               
    member x.CancelCommand with get() = cancelCommand
    

// This handles "code behind", ie: pure view logic for our dialog
type RenameDialogViewController() =
    interface FsXaml.IViewController with
        member this.Attach fe =
            // Use the TypeProvider's Accessor sub-type to gain access to named members
            let window = RenameDialog.Accessor fe

            let model = window.Root.DataContext :?> INotifyPropertyChanged
            // Once the model is initialized, focus and select txtName
            model.PropertyChanged.Add(fun e ->
                if e.PropertyName = "Initialized" then
                    window.Root.Activate() |> ignore
                    window.txtName.IsEnabled <- true
                    window.txtName.SelectAll()
                    window.txtName.Focus() |> ignore)
             
 [<RequireQualifiedAccess>]
module UI =
    let loadRenameDialog (viewModel: RenameDialogModel) owner =
        let window = RenameDialog().CreateRoot()
        window.Owner <- owner
        window.DataContext <- viewModel        
        window

