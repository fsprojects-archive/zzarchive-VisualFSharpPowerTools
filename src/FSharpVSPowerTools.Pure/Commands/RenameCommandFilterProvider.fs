module FSharpVSPowerTools.RenameCommandFilterProvider

open System.Diagnostics
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.Refactoring
open System.ComponentModel.Composition
open System
open Microsoft.VisualStudio.Text


[<Export (typeof<IVsTextViewCreationListener>)>]
[<ContentType "F#">]
[<TextViewRole (PredefinedTextViewRoles.Editable)>]
type RenameCommandFilterProvider [<ImportingConstructor>]
    ( [<Import (typeof<SVsServiceProvider>)>] 
        serviceProvider             :   IServiceProvider                ,
        textDocumentFactoryService  :   ITextDocumentFactoryService     ,
        editorFacotry               :   IVsEditorAdaptersFactoryService ,
        projectFactory              :   ProjectFactory                  ,
        vsLanguageService           :   VSLanguageService               ) =


    static member AddCommandFilter (viewAdapter:IVsTextView, commandFilter:RenameCommandFilter) =
        if not commandFilter.IsAdded then
            let mutable next = Unchecked.defaultof<IOleCommandTarget>
            let hr = viewAdapter.AddCommandFilter(commandFilter, &next)

            if hr = VSConstants.S_OK then
                commandFilter.IsAdded <- true
                // You'll need the next target for Exec and QueryStatus
                if isNotNull next then commandFilter.NextTarget <- next


    interface IVsTextViewCreationListener with

        member __.VsTextViewCreated textViewAdapter =
            maybe {
                let! textView = editorFacotry.TryGetWpfTextView textViewAdapter
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider

                if not generalOptions.RenameRefactoringEnabled then return! None else
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer textView.TextBuffer
                RenameCommandFilterProvider.AddCommandFilter ( textViewAdapter, 
                        new RenameCommandFilter( doc, textView, vsLanguageService, serviceProvider, projectFactory))
            } |> ignore

