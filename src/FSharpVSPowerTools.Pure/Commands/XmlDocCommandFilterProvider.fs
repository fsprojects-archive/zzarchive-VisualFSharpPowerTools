module FSharpVSPowerTools.XmlDocCommandFilterProvider

open System.Diagnostics                         
open Microsoft.VisualStudio                     
open Microsoft.VisualStudio.Shell               
open Microsoft.VisualStudio.Editor              
open Microsoft.VisualStudio.OLE.Interop         
open Microsoft.VisualStudio.Text.Editor         
open Microsoft.VisualStudio.TextManager.Interop 
open Microsoft.VisualStudio.Utilities           
open Microsoft.VisualStudio.Text                
open System                                     
open System.ComponentModel.Composition          
open System.Runtime.InteropServices             
open FSharpVSPowerTools.XmlDoc                  
open FSharpVSPowerTools.ProjectSystem           

// Useful reference: http://msdn.microsoft.com/en-us/library/dd885243.aspx

[<Export (typeof<IVsTextViewCreationListener>)>]
[<ContentType "F#" >]
[<TextViewRole (PredefinedTextViewRoles.Interactive)>]
type XmlDocCommandFilterProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
    serviceProvider                 :   IServiceProvider                ,
    textDocumentFactoryService      :   ITextDocumentFactoryService     ,
    editorFactory                   :   IVsEditorAdaptersFactoryService ,    
    projectFactory                  :   ProjectFactory                  ,
    vsLanguageService               :   VSLanguageService               ) =   

    interface IVsTextViewCreationListener with
        member __.VsTextViewCreated textViewAdapter = 
            unitMaybe {
                let! generalOptions = Setting.tryGetGeneralOptions serviceProvider
                let! wpfTextView = editorFactory.TryGetWpfTextView textViewAdapter
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer wpfTextView.TextBuffer  
                if generalOptions.XmlDocEnabled then 
                    XmlDocFilter( textViewAdapter, wpfTextView, doc.FilePath, projectFactory, 
                        vsLanguageService,OpenDocumentsTracker textDocumentFactoryService, serviceProvider)
                    |> ignore
            } 


         

