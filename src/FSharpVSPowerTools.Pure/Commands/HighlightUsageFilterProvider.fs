module FSharpVSPowerTools.HighlightUsageFilterProvider

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
open FSharpVSPowerTools.HighlightUsage
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open System.ComponentModel.Composition
open System


[<Export(typeof<IVsTextViewCreationListener>)>]
[<ContentType "F#">]
[<TextViewRole(PredefinedTextViewRoles.Editable)>]
type HighlightUsageFilterProvider [<ImportingConstructor>]
    ( [<Import (typeof<SVsServiceProvider>)>] 
        serviceProvider :   IServiceProvider                    ,
        editorFactory   :   IVsEditorAdaptersFactoryService     ,
        tagAggregator   :   IViewTagAggregatorFactoryService    ) =
    

    static member AddCommandFilter (viewAdapter:IVsTextView, commandFilter:HighlightUsageFilter) =
        if not commandFilter.IsAdded then
            let mutable next = Unchecked.defaultof<IOleCommandTarget>
            let hr = viewAdapter.AddCommandFilter(commandFilter, &next)

            if hr = VSConstants.S_OK then
                commandFilter.IsAdded <- true
                // You'll need the next target for Exec and QueryStatus
                if isNotNull next then commandFilter.NextTarget <- next


    interface IVsTextViewCreationListener with

        member __.VsTextViewCreated textViewAdapter =
            unitMaybe {
                let! textView = editorFactory.TryGetWpfTextView textViewAdapter
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                if not generalOptions.HighlightUsageEnabled then return! None else

                HighlightUsageFilterProvider.AddCommandFilter ( textViewAdapter, 
                        new HighlightUsageFilter (textView, tagAggregator.CreateTagAggregator<TextMarkerTag> textView))
            } 

