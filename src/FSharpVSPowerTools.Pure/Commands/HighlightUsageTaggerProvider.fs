module FSharpVSPowerTools.HighlightUsageTaggerProvider

open System.Diagnostics
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open EnvDTE
open FSharpVSPowerTools.HighlightUsage
open FSharpVSPowerTools.ProjectSystem
open System

//[<Export(typeof<IViewTaggerProvider>)>]
//[<ContentType("F#")>]
//[<TagType(typeof<HighlightUsageTag>)>]
//type HighlightUsageTaggerProvider [<ImportingConstructor>]
//    ( [<Import(typeof<SVsServiceProvider>)>] 
//        serviceProvider : IServiceProvider,
//        textDocumentFactoryService      :   ITextDocumentFactoryService           ,
//        projectFactory                  :   ProjectFactory                        ,
//        vsLanguageService               :   VSLanguageService                     ) =
//
//    interface IViewTaggerProvider with
//        member __.CreateTagger (textView, buffer) = 
//            maybe{
//          //      let generalOptions = Setting.getGeneralOptions serviceProvider
//                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
//            //    if not generalOptions.HighlightUsageEnabled then return! None else
//                return buffer.Properties.GetOrCreateSingletonProperty (fun () ->
//                    new HighlightUsageTagger (doc, textView, vsLanguageService, serviceProvider, projectFactory)
//                ) :> obj :?> _
//            } |> Option.getOrElse null
