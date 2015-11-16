module FSharpVSPowerTools.ResolveUnopenedNamespaceSmartTaggerProvider

open System
open EnvDTE
open EnvDTE
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
[<TagType (typeof<ResolveUnopenedNamespaceSmartTag>)>]
[<TextViewRole (PredefinedTextViewRoles.Editable)>]
type ResolveUnopenedNamespaceSmartTaggerProvider [<ImportingConstructor>]
   ([<Import (typeof<SVsServiceProvider>)>] 
    serviceProvider             :   IServiceProvider            ,
    textDocumentFactoryService  :   ITextDocumentFactoryService ,
    undoHistoryRegistry         :   ITextUndoHistoryRegistry    ,
    projectFactory              :   ProjectFactory              ,
    vsLanguageService           :   VSLanguageService           ) =

    interface IViewTaggerProvider with
        member __.CreateTagger (textView, buffer) =
            maybe {
                if textView.TextBuffer <> buffer then return! None else
               // let generalOptions = Setting.getGeneralOptions serviceProvider
                let dte = serviceProvider.GetService<EnvDTE.DTE,SDTE>()
                if dte.Version = string VisualStudioVersion.VS2015 then return! None else
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
                let resolver = 
                    new UnopenedNamespaceResolver (doc, textView, undoHistoryRegistry.RegisterHistory buffer, 
                        vsLanguageService, serviceProvider, projectFactory)
                return
                    new ResolveUnopenedNamespaceSmartTagger (buffer, serviceProvider, resolver) :> obj :?> _
            } |> Option.getOrElse null
            
