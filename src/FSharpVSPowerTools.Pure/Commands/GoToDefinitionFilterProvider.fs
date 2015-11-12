module FSharpVSPowerTools.GoToDefinitionFilterProvider

open System.Diagnostics
open System.ComponentModel.Composition
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.Navigation
open Microsoft.VisualStudio.Text
open System.Collections.ObjectModel
open System
open Microsoft.VisualStudio.Shell.Interop
open EnvDTE80
open EnvDTE

[<Export(typeof<DotNetReferenceSourceProvider>)>]
type DotNetReferenceSourceProvider () =
    inherit ReferenceSourceProvider ("http://referencesource.microsoft.com")

[<Export (typeof<IVsTextViewCreationListener>)>]
[<Export (typeof<IWpfTextViewConnectionListener>)>]
[<ContentType "F#">]
[<TextViewRole (PredefinedTextViewRoles.Editable)>]
type  GoToDefinitionFilterProvider [<ImportingConstructor>]
    (   [<Import (typeof<SVsServiceProvider>)>] 
        serviceProvider                 :   IServiceProvider                ,
        editorFactory                   :   IVsEditorAdaptersFactoryService ,
        editorOptionsFactory            :   IEditorOptionsFactoryService    ,
        textDocumentFactoryService      :   ITextDocumentFactoryService     ,
        textEditorFactoryService        :   ITextEditorFactoryService       ,
        [<Import (typeof<DotNetReferenceSourceProvider>)>] 
        referenceSourceProvider         :   ReferenceSourceProvider         ,
        projectFactory                  :   ProjectFactory                  ,
        vsLanguageService               :   VSLanguageService               ) as self =
    
    let mutable solutionEvents = Unchecked.defaultof<SolutionEvents>

    let serviceType = typeof<GoToDefinitionFilterProvider>
    

    let dte = serviceProvider.GetService<EnvDTE.DTE,SDTE>()
    let events = dte.Events  :?> Events2
//    do 
//        if isNotNull events then
//            solutionEvents <- events.SolutionEvents
//            let afterClosing = solutionEvents.add_AfterClosing self.Cleanup()
//            afterClosing.
//          
    static member AddCommandFilter (viewAdapter:IVsTextView, commandFilter: GoToDefinitionFilter) : unit =
        if not commandFilter.IsAdded then
            let mutable next = Unchecked.defaultof<IOleCommandTarget>
            let hr = viewAdapter.AddCommandFilter(commandFilter, &next)

            if hr = VSConstants.S_OK then
                commandFilter.IsAdded <- true
                // You'll need the next target for Exec and QueryStatus
                if isNotNull next then commandFilter.NextTarget <- next
  

    member __.RegisterCommandFilter (textView:IWpfTextView, fireNavigationEvent:bool) : GoToDefinitionFilter option =
        maybe {
            let! textViewAdapter = editorFactory.TryGetViewAdapter textView 
            return! self.Register (textViewAdapter,textView, fireNavigationEvent)
        } 


    member __.Register (textViewAdapter:IVsTextView, textView:IWpfTextView, fireNavigationEvent:bool) : GoToDefinitionFilter option =
        maybe {
            let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
            if  not generalOptions.GoToMetadataEnabled 
                && not generalOptions.GoToSymbolSourceEnabled then return! None else
            let preference = 
                if generalOptions.GoToSymbolSourceEnabled then
                    if generalOptions.GoToMetadataEnabled   then NavigationPreference.SymbolSourceOrMetadata
                                                            else NavigationPreference.SymbolSource
                else NavigationPreference.Metadata 
            let! doc = textDocumentFactoryService.TryDocumentFromBuffer textView.TextBuffer
            let commandFilter = 
                new GoToDefinitionFilter (doc, textView, editorOptionsFactory, vsLanguageService,
                        serviceProvider, projectFactory, referenceSourceProvider, preference, fireNavigationEvent)
            if not referenceSourceProvider.IsActivated 
             && generalOptions.GoToSymbolSourceEnabled then
                referenceSourceProvider.Activate()

            textView.Properties.AddProperty (serviceType, commandFilter) 
            GoToDefinitionFilterProvider.AddCommandFilter(textViewAdapter, commandFilter)
            return
                commandFilter
        }        

    member __.Cleanup () = GoToDefinitionFilter.ClearXmlDocCache ()
    
    interface IWpfTextViewConnectionListener with    

        member __.SubjectBuffersConnected (textView:IWpfTextView, reason:ConnectionReason, subjectBuffers:Collection<ITextBuffer>) = ()
        
        member __.SubjectBuffersDisconnected (textView:IWpfTextView, reason:ConnectionReason, subjectBuffers:Collection<ITextBuffer>) =
                if reason = ConnectionReason.TextViewLifetime then () else 

                let mutable commandFilter = Unchecked.defaultof<GoToDefinitionFilter>

                if textView.Properties.TryGetProperty( serviceType, &commandFilter) then
                    let textViewAdapter = editorFactory.GetViewAdapter textView
                    let hr = textViewAdapter.RemoveCommandFilter commandFilter 
                    let success = textView.Properties.RemoveProperty serviceType
                    Debug.Assert ( (hr = VSConstants.S_OK) , "Should be able to remove adornment from the text view")
                    (commandFilter :> IDisposable).Dispose()


    interface IVsTextViewCreationListener with
        member __.VsTextViewCreated (textViewAdapter:IVsTextView) =
            maybe {
                let! textView = editorFactory.TryGetWpfTextView textViewAdapter
                self.Register( textViewAdapter, textView, false)
                |> ignore
            } |> ignore


    interface IDisposable with
        member __.Dispose () =
//            if isNotNull solutionEvents then
//            solutionEvents.remove_AfterClosing self.Cleanup
            (referenceSourceProvider :> IDisposable).Dispose ()