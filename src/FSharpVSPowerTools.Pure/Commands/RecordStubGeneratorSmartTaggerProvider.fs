module FSharpVSPowerTools.RecordStubGeneratorSmartTaggerProvider

open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open FSharpVSPowerTools.Refactoring
open FSharpVSPowerTools.ProjectSystem
open System.Diagnostics
open Microsoft.VisualStudio.Shell.Interop

[<Export(typeof<IViewTaggerProvider>)>]
[<ContentType "F#">]
[<TagType(typeof<RecordStubGeneratorSmartTag>)>]
type RecordStubGeneratorSmartTaggerProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
        serviceProvider             :   IServiceProvider            ,
        textDocumentFactoryService  :   ITextDocumentFactoryService ,
        textEditorFactoryService    :   ITextEditorFactoryService   ,
        undoHistoryRegistry         :   ITextUndoHistoryRegistry    ,
        projectFactory              :   ProjectFactory              ,
        vsLanguageService           :   VSLanguageService           )  =

    interface IViewTaggerProvider with
        member __.CreateTagger (textView, buffer) =
            maybe {
                // Only provide the smart tagger on the top-level buffer
                if textView.TextBuffer <> buffer then return! None else
       //         let generalOptions = Setting.getGeneralOptions serviceProvider
         //       if not generalOptions.GenerateRecordStubEnabled then return! None else
                let codeGenOptions = Setting.getCodeGenerationOptions serviceProvider
                let dte = serviceProvider.GetService<EnvDTE.DTE,SDTE>()
                if dte.Version = string VisualStudioVersion.VS2015 then return! None else
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
                let generator = 
                    new RecordStubGenerator (doc, textView, undoHistoryRegistry.RegisterHistory buffer, 
                        vsLanguageService, serviceProvider, projectFactory, 
                        Setting.getDefaultMemberBody codeGenOptions, OpenDocumentsTracker textDocumentFactoryService)
                return
                    new RecordStubGeneratorSmartTagger (buffer, generator) :> obj :?> _
            } |> Option.getOrElse null

