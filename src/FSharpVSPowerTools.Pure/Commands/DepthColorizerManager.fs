module FSharpVSPowerTools.DepthColorizerManager

open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Formatting
open Microsoft.VisualStudio.Utilities
open System.ComponentModel.Composition
open System.Windows.Media
open System.Windows
open System.Windows.Controls
open Microsoft.Win32
open FSharpVSPowerTools.DepthColorizer
open Microsoft.VisualStudio.Shell
open System.Diagnostics
open FSharpVSPowerTools.ProjectSystem
open System.Collections.ObjectModel


[<Export (typeof<IWpfTextViewCreationListener>)>]
[<Export (typeof<IWpfTextViewConnectionListener>)>]
[<ContentType "F#">]
[<TextViewRole (PredefinedTextViewRoles.Structured)>]
type DepthColorizerAdornmentManager [<ImportingConstructor>]
    ( [<Import (typeof<SVsServiceProvider>)>] 
    serviceProvider                 :   IServiceProvider                ,
    shellEventListener              :   ShellEventListener              ,
    themeManager                    :   ThemeManager                    ,
    viewTagAggregatorFactoryService : IViewTagAggregatorFactoryService  ) =
    
    let mutable adornmentLayerDefinition = Unchecked.defaultof<AdornmentLayerDefinition>

    let serviceType = typeof<DepthColorizerAdornmentManager>

    [<Export>]
    [<Name (Constants.depthAdornmentLayerName)>]
    [<Order (Before = PredefinedAdornmentLayers.CurrentLineHighlighter)>]
    member __.AdornmentLayerDefinition 
        with get () = adornmentLayerDefinition 
        and  set v  = adornmentLayerDefinition <- v


    interface IWpfTextViewCreationListener with
        member __.TextViewCreated textView =
            unitMaybe {
                let generalOptions = Setting.getGeneralOptions serviceProvider
                if not generalOptions.DepthColorizerEnabled then () else

                let tagAggregator = viewTagAggregatorFactoryService.CreateTagAggregator<DepthRegionTag> textView
                let adornment = new DepthColorizerAdornment (textView, tagAggregator, themeManager, shellEventListener)
                textView.Properties.AddProperty (typeof<DepthColorizerAdornmentManager>, adornment )
            }

    interface IWpfTextViewConnectionListener with    

        member __.SubjectBuffersConnected (textView:IWpfTextView, reason:ConnectionReason, subjectBuffers:Collection<ITextBuffer>) = ()
        
        member __.SubjectBuffersDisconnected (textView:IWpfTextView, reason:ConnectionReason, subjectBuffers:Collection<ITextBuffer>) =
                if reason = ConnectionReason.TextViewLifetime then () else 
                let mutable adornment = Unchecked.defaultof<IDisposable>
                if textView.Properties.TryGetProperty( serviceType, &adornment) then
                    let success = textView.Properties.RemoveProperty serviceType
                    Debug.Assert (success, "Should be able to remove adornment from the text view")
                    adornment.Dispose()


//[<Export (typeof<ITaggerProvider>)>]
//[<ContentType "F#">]
//[<TagType (typeof<DepthRegionTag>)>]
//type DepthColorizerTaggerProvider [<ImportingConstructor>]
//    ( [<Import (typeof<SVsServiceProvider>)>] 
//    serviceProvider             :   IServiceProvider            ,
//    textDocumentFactoryService  :   ITextDocumentFactoryService ,
//    projectFactory              :   ProjectFactory              ,
//    vsLanguageService           :   VSLanguageService           ) =
//        
//    interface ITaggerProvider with
//        member __.CreateTagger buffer =
//            maybe {
//                let generalOptions = Setting.getGeneralOptions serviceProvider
//                if not generalOptions.DepthColorizerEnabled then return! None else
//                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
//                return 
//                    new DepthTagger (doc, buffer, serviceProvider, projectFactory, 
//                            vsLanguageService, OpenDocumentsTracker textDocumentFactoryService)
//                    :> obj :?> _
//            } |> Option.getOrElse null
//
