module FSharpVSPowerTools.CodeFormattingHelper

open System                                 
open System.Collections.Generic             
open System.ComponentModel.Composition      
open System.Linq                            
open System.Text                            
open System.Threading.Tasks                 
open Microsoft.VisualStudio.Editor          
open Microsoft.VisualStudio.Text.Editor     
open Microsoft.VisualStudio.Text.Operations 
open Microsoft.VisualStudio.Utilities       
open Microsoft.VisualStudio.Text            
open FSharpVSPowerTools.CodeFormatting      
open Microsoft.VisualStudio.Shell      
open FSharpVSPowerTools.ProjectSystem     

[<Export (typeof<IWpfTextViewCreationListener>)>]
[<Name "F# Formatting Command Hook">]
[<ContentType "F#">]
[<TextViewRole (PredefinedTextViewRoles.Interactive)>]
type CodeFormattingHookHelper [<ImportingConstructor>] 
    (   [<Import(typeof<SVsServiceProvider>)>]  
        serviceProvider                 :   IServiceProvider,
        adaptersFactory                 :   IVsEditorAdaptersFactoryService,  
        editorOptionsFactory            :   IEditorOptionsFactoryService,    
        editorOperationsFactoryService  :   IEditorOperationsFactoryService, 
        textBufferUndoManagerProvider   :   ITextBufferUndoManagerProvider,  
        textDocumentFactoryService      :   ITextDocumentFactoryService     ) as self =

    member private __.GetServices () =
        CodeFormattingServices
            (   editorOptionsFactory, editorOperationsFactoryService, textBufferUndoManagerProvider,
                textDocumentFactoryService, serviceProvider)

    member internal __.RegisterCommandDispatcher (wpfTextView:IWpfTextView) =
        maybe{
            let! view = adaptersFactory.TryGetViewAdapter wpfTextView
            return!
                StandardCommandDispatcher.Register (view, wpfTextView, self.GetServices ())
        }

    interface IWpfTextViewCreationListener with
        member __.TextViewCreated (textView:IWpfTextView) : unit = 
            System.Windows.Threading.Dispatcher.CurrentDispatcher.BeginInvoke
                (Action (fun () -> (self.RegisterCommandDispatcher textView)|>ignore))
            |> ignore
        
        