namespace FSharpVSPowerTools.QuickInfo

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open System
open System.ComponentModel.Composition


[<Export (typeof<IWpfTextViewMarginProvider>)>]
[<Name (Constants.QuickInfoMargin)>]
[<Order (After = PredefinedMarginNames.HorizontalScrollBar)>]
[<MarginContainer (PredefinedMarginNames.Bottom)>]
[<ContentType "F#">]
[<TextViewRole (PredefinedTextViewRoles.Document)>]
type QuickInfoMarginProvider [<ImportingConstructor>] 
    (   [<Import(typeof<SVsServiceProvider>)>] 
        serviceProvider                 :   IServiceProvider            ,
        textDocumentFactoryService      :   ITextDocumentFactoryService ,
        projectFactory                  :   ProjectFactory              ,
        vsLanguageService               :   VSLanguageService           ) =


    interface IWpfTextViewMarginProvider with
        member x.CreateMargin(wpfTextViewHost: IWpfTextViewHost, marginContainer: IWpfTextViewMargin): IWpfTextViewMargin = 
            let textView = wpfTextViewHost.TextView
            let buffer = textView.TextBuffer
            maybe {
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                if not generalOptions.QuickInfoPanelEnabled then return! None else 
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
                return
                    new QuickInfoMargin( doc, textView, vsLanguageService, serviceProvider, projectFactory)
                    :> IWpfTextViewMargin
            } |> Option.getOrElse marginContainer


