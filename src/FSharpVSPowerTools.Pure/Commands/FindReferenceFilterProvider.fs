module FSharpVSPowerTools.FindReferenceFilterProvider

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
open FSharpVSPowerTools.Navigation
open Microsoft.VisualStudio.Text
open System
open System.ComponentModel.Composition

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Editable)>]
type FindReferencesFilterProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
        serviceProvider             :   IServiceProvider                ,
        textDocumentFactoryService  :   ITextDocumentFactoryService     ,
        editorFactory               :   IVsEditorAdaptersFactoryService ,
        fileSystem                  :   FileSystem                      ,
        projectFactory              :   ProjectFactory                  ,
        vsLanguageService           :   VSLanguageService               ) as self = 

    static member AddCommandFilter (viewAdapter:IVsTextView, commandFilter:FindReferencesFilter) =
        if not commandFilter.IsAdded then
            let mutable next = Unchecked.defaultof<IOleCommandTarget>
            let hr = viewAdapter.AddCommandFilter(commandFilter, &next)

            if hr = VSConstants.S_OK then
                commandFilter.IsAdded <- true
                // You'll need the next target for Exec and QueryStatus
                if isNotNull next then commandFilter.NextTarget <- next

    member __.RegisterCommandFilter (textView:IWpfTextView, showProgress:bool ) =
        maybe {
            let! textViewAdapter = editorFactory.TryGetViewAdapter textView
            let! generalOptions = Setting.tryGetGeneralOptions serviceProvider

            if not generalOptions.FindAllReferencesEnabled then return! None else

            let! doc = textDocumentFactoryService.TryDocumentFromBuffer textView.TextBuffer
            let filter = new FindReferencesFilter ( doc, textView, vsLanguageService, serviceProvider,
                                                    projectFactory, showProgress, fileSystem)

            FindReferencesFilterProvider.AddCommandFilter (textViewAdapter, filter)
            return 
                filter        
        }
    member __.TextViewCreated textView =
        self.RegisterCommandFilter (textView, true) |> ignore

    interface IWpfTextViewConnectionListener with
        
        member x.SubjectBuffersConnected(textView, reason, subjectBuffers) = ()
           

        member x.SubjectBuffersDisconnected(textView, reason, subjectBuffers) =  ()
            
        

