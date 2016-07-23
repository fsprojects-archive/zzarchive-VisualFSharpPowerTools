﻿namespace FSharp.Editing.VisualStudio.Symbol

open System
open System.ComponentModel
open System.Threading
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Editing
open FSharp.Editing.VisualStudio
open FSharp.Editing.VisualStudio.ProjectSystem
open FSharp.ViewModule
open FSharp.ViewModule.Progress
open FSharp.ViewModule.Validation
open FSharp.Editing.IdentifierUtils
open FSharp.Editing.Features
open FSharp.Editing.ProjectSystem

type RenameDialog = FsXaml.XAML< @"Gui/RenameDialog.xaml">

[<NoComparison>]
type RenameContext =
    { ParseAndCheckResults: ParseAndCheckResults
      SymbolDeclarationLocation: SymbolDeclarationLocation
      FSharpSymbol: FSharpSymbol 
      Symbol: Symbol }

type SymbolName = string

type RenameDialogViewModel(originalName: string, initialContext: Async<RenameContext option>, 
                           rename: RenameContext -> SymbolName -> ShowProgress -> Async<unit>, 
                           cts: CancellationTokenSource) as self = 
    inherit ViewModelBase()
    let originalName = originalName.Replace(DoubleBackTickDelimiter, "")
    // This will hold the actual rename workflow arguments after the initialization async workflow completes    
    let mutable context: RenameContext option = None
    // Grab our synchronization context for use in async workflows as necessary
    let syncCtx = SynchronizationContext.Current
     
    // Custom validation for the name property
    let validateSymbols newName = 
        let check validationCheck error = 
            if validationCheck then None
            else Some error
        debug "[Rename Refactoring] Check the following name: %s" newName
        match context with
        | None -> Some Resource.renameErrorMessage
        | Some { FSharpSymbol = fssym; Symbol = symbol } -> 
            match symbol.Kind, fssym with
            | _, UnionCase _ -> check (isUnionCaseIdent newName) Resource.validatingUnionCase
            | _, ActivePatternCase _ -> 
                // Different from union cases, active patterns don't accept double-backtick identifiers
                check (isFixableIdentifier newName && not (String.IsNullOrEmpty newName) && Char.IsUpper(newName.[0])) 
                    Resource.validatingActivePattern
            | Operator, _ -> check (isOperator newName) Resource.validatingOperator
            | GenericTypeParameter, _ -> check (isGenericTypeParameter newName) Resource.validatingGenericTypeParameter
            | StaticallyResolvedTypeParameter, _ -> 
                check (isStaticallyResolvedTypeParameter newName) Resource.validatingStaticallyResolvedTypeParameter
            | (Ident | Other), _ -> 
                if SourceCodeClassifier.getIdentifierCategory fssym <> Category.Other then 
                    check (isTypeNameIdent newName) Resource.validatingTypeName
                else check (isFixableIdentifier newName) Resource.validatingIdentifier
     
    // Complete validation chain for the name property
    let validateName = 
        validate "Name"
        >> notNullOrWhitespace
        >> fixErrorsWithMessage Resource.validatingEmptyName
        >> custom validateSymbols
        >> result
    
    // Backing fields for all view-bound values
    let name = self.Factory.Backing(<@@ self.Name @@>, originalName, validateName)
    let mutable symbolLocation = ""
    let fullName = self.Factory.Backing(<@@ self.FullName @@>, String.Empty)
    // RenameComplete is used to close our dialog from the View automatically - should be set when we want to "complete" this operation
    let renameComplete = self.Factory.Backing(<@@ self.RenameComplete @@>, false)
    // Initialized allows us to track when the initialization async workflow completes
    let initialized = self.Factory.Backing(<@@ self.Initialized @@>, false)
    // Create a progress manager to track the progress status
    let progress = ProgressManager()
    // This provides us a simple way to report progress to both the status bar and our dialog
    let report = Some (updateProgress progress)
    
    // We wrap up the actual async workflow in order to close us when renameComplete as well as mark us executing while we run
    let wrappedWorkflow _ newName = 
        async { 
            use waitCursor = Cursor.wait()
            reportProgress report (Reporting "Performing Rename...")
            // If the user just accepts with the original name, just make this a no-op instead of an error
            if not (newName = originalName) then 
                match context with
                | Some ctx -> do! rename ctx newName report.Value
                | _ -> ()
            renameComplete.Value <- true
            waitCursor.Restore()
        }
    
    // The async command which is executed when the user clicks OK
    let execute = 
        self.Factory.CommandAsyncParamChecked(wrappedWorkflow, (fun _ -> self.IsValid && self.Initialized), 
                                              [ <@@ self.IsValid @@>; <@@ self.Initialized @@> ], cts.Token)
    
    // Canceling the operation should cancel all async workflows and close us    
    let cancelCommand = self.Factory.CommandSync(fun _ -> cts.Cancel())
    
    // Generate the new name and show it on the textbox
    let updateFullName newName = 
        let encapsulated = 
            if validateName newName <> [] then newName
            else 
                match context with
                | Some { Symbol = symbol } -> encapsulateIdentifier symbol.Kind newName
                | None -> ""
        self.FullName <-
            if String.IsNullOrEmpty symbolLocation then encapsulated
            elif String.IsNullOrEmpty encapsulated then symbolLocation
            else symbolLocation + "." + encapsulated
    
    do 
        // Make us close if we're canceled
        cts.Token.Register(fun _ -> renameComplete.Value <- true) |> ignore
        // Force initialization to re-evaluate Name's validation by making it a dependency      
        self.DependencyTracker.AddPropertyDependency(<@@ self.Name @@>, <@@ self.Initialized @@>)
        // Perform our initialization routine while reporting progress/etc as necessary
        Async.StartImmediateSafe(
            async { 
                use waitCursor = Cursor.wait()
                reportProgress report (Reporting "Initializing...")
                let! ctx = initialContext
                match ctx with
                | Some { FSharpSymbol = fsSymbol } -> 
                    do! Async.SwitchToContext syncCtx
                    context <- ctx
                    reportProgress report Idle
                    let fullName = fsSymbol.FullName
                    let location =
                        match fsSymbol.DisplayName with
                        | "" -> ""
                        | displayName when fullName.EndsWith displayName ->
                            let locationLength = max 0 (fullName.Length - (displayName.Length + 1))
                            fullName.Remove locationLength
                        | _ -> fullName
                    symbolLocation <- location
                    updateFullName originalName
                    initialized.Value <- true
                | None -> cts.Cancel()
                waitCursor.Restore()
            }, cts.Token)
    
    // Our bound properties
    member __.Name 
        with get () = name.Value
        and set (v) = 
            name.Value <- v
            updateFullName v
    
    member __.FullName 
        with get () = fullName.Value
        and set (v) = fullName.Value <- v
    
    // Related to progress / status reporting
    member __.Progress = progress
    // Handles life-cycle tracking (when things should be enabled, closed, etc)
    member __.Initialized = initialized.Value
    member __.RenameComplete = renameComplete.Value
    // The actual commands for the buttons
    member __.ExecuteCommand = execute
    member __.CancelCommand = cancelCommand
                     
// Module for loading the rename dialog with a viewModel + owner
[<RequireQualifiedAccess>]
module UI = 
    open System.Windows.Threading

    let loadRenameDialog (viewModel: RenameDialogViewModel) owner = 
        let window = RenameDialog()

        // Once the view model is initialized, focus and select txtName so the user can just type "F2 / new_name / Enter"
        let inpc = viewModel :> INotifyPropertyChanged
        inpc.PropertyChanged.Add(fun e -> 
            if e.PropertyName = "Initialized" then 
                window.txtName.IsEnabled <- true
                window.txtName.SelectAll()
                window.Activate() |> ignore
                // Schedule activation to happen after bindings enable and make controls visible
                window.Dispatcher.BeginInvoke(DispatcherPriority.ApplicationIdle, 
                                                   ThreadStart(fun _ -> ignore(window.txtName.Focus()))) |> ignore)
        window.Owner <- owner
        window.DataContext <- viewModel
        window
