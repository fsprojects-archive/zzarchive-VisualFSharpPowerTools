module FSharpVSPowerTools.UnusedDeclarationMarginProvider

open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Linq
open System.Text
open System.Threading.Tasks
open FSharpVSPowerTools.SyntaxColoring

[<Export (typeof<IWpfTextViewMarginProvider>)>]
[<Name (Constants.fsharpUnusedDeclarationMargin)>]
[<ContentType "F#">]
[<Order (After = PredefinedMarginNames.VerticalScrollBar)>]
[<MarginContainer (PredefinedMarginNames.VerticalScrollBarContainer)>]
[<TextViewRole (PredefinedTextViewRoles.PrimaryDocument)>]
type UnusedDeclarationMarginProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
    serviceProvider : IServiceProvider,
    viewTagAggregatorFactoryService :   IViewTagAggregatorFactoryService ) =

    interface IWpfTextViewMarginProvider with
        member __.CreateMargin (wpfTextViewHost, marginContainer) =
            maybe {
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                if  not generalOptions.UnusedReferencesEnabled 
                    ||  generalOptions.UnusedOpensEnabled then return! None else 
                let textView = wpfTextViewHost.TextView
                let tagAggregator = viewTagAggregatorFactoryService.CreateTagAggregator<UnusedDeclarationTag> textView
                return 
                    new UnusedDeclarationMargin (textView, marginContainer, tagAggregator)
                    :> IWpfTextViewMargin
            } |>  Option.getOrElse null
