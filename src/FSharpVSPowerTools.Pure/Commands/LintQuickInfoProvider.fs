module FSharpVSPowerTools.LintQuickInfoProvider

open FSharpVSPowerTools.Linting
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System
open System.ComponentModel.Composition


[<Export (typeof<IQuickInfoSourceProvider>)>]
[<Name ("F# Lint Quick Info Provider")>]
[<Order (Before = "Default Quick Info Presenter")>]
[<ContentType "F#">]

type LintQuickInfoProvider [<ImportingConstructor>]
    //( [<Import(typeof<SVsServiceProvider>)>] 
     //   serviceProvider : IServiceProvider,
    (    viewTagAggregatorFactoryService : IViewTagAggregatorFactoryService ) =

    interface IQuickInfoSourceProvider with
        member __.TryCreateQuickInfoSource textBuffer =
            maybe {
                let! generalOptions = Setting.tryGetGeneralOptions()// serviceProvider
                if not generalOptions.LinterEnabled then return! None else
                return
                    new LintQuickInfoSource (textBuffer, viewTagAggregatorFactoryService)
                        :> IQuickInfoSource
            } |> Option.getOrElse null
