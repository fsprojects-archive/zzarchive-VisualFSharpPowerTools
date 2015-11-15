module FSharpVSPowerTools.UnionPatternMatchCaseGeneratorSmartTaggerProvider

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

[<Export (typeof<IViewTaggerProvider>)>]
[<ContentType "F#">]
[<TagType (typeof<UnionPatternMatchCaseGeneratorSmartTag>)>]
type UnionPatternMatchCaseGeneratorSmartTaggerProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
        serviceProvider             :   IServiceProvider,
        textDocumentFactoryService  :   ITextDocumentFactoryService ,
        undoHistoryRegistry         :   ITextUndoHistoryRegistry    ,
        projectFactory              :   ProjectFactory              ,
        vsLanguageService           :   VSLanguageService           ) =

    interface IViewTaggerProvider with
        member __.CreateTagger (textView, buffer) =
            maybe {
                if textView.TextBuffer <> buffer then return! None else
       //         let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                let! codeGenOptions = Setting.tryGetCodeGenerationOptions serviceProvider
                let dte = serviceProvider.GetService<EnvDTE.DTE,SDTE>()
                if dte.Version = string VisualStudioVersion.VS2015 then return! None else
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
                let generator = 
                    new UnionPatternMatchCaseGenerator (doc, textView, undoHistoryRegistry.RegisterHistory buffer, 
                        vsLanguageService, serviceProvider, projectFactory, Setting.getDefaultMemberBody codeGenOptions, OpenDocumentsTracker textDocumentFactoryService )
                return
                    new UnionPatternMatchCaseGeneratorSmartTagger (buffer, generator) :> obj :?> _
            } |> Option.getOrElse null
