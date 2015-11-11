module FSharpVsPowerTools.Pure.OutliningTaggerProvider


open System.ComponentModel.Composition      
open FSharpVSPowerTools.ProjectSystem       
open Microsoft.VisualStudio.Shell           
open Microsoft.VisualStudio.Text            
open Microsoft.VisualStudio.Text.Tagging    
open Microsoft.VisualStudio.Utilities       
open System                                 
open Microsoft.VisualStudio.Text.Projection 
open Microsoft.VisualStudio.Text.Editor     
open Microsoft.VisualStudio.Text.Outlining  
open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem.VSUtils
open FSharpVSPowerTools.Outlining

[<Export(typeof<ITaggerProvider>)>]
[<Export(typeof<IWpfTextViewCreationListener>)>]
[<TagType(typeof<IOutliningRegionTag>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Structured)>]
type OutliningTaggerProvider [<ImportingConstructor>]
    ( [<Import(typeof<SVsServiceProvider>)>] 
        serviceProvider : IServiceProvider,
        textDocumentFactoryService      :   ITextDocumentFactoryService           ,
        textEditorFactoryService        :   ITextEditorFactoryService             ,
        projectionBufferFactoryService  :   IProjectionBufferFactoryService       ,
        outliningManagerService         :   IOutliningManagerService              ,
        projectFactory                  :   ProjectFactory                        ,
        vsLanguageService               :   VSLanguageService                     ) as self =

    member __.CreateTagger<'T when 'T :> ITag> (buffer: ITextBuffer) = 
//        let generalOptions = Setting.getGeneralOptions serviceProvider
//        let doc = serviceProvider.GetDocumentFromBuffer buffer
//        if doc.IsNone || not generalOptions.OutliningEnabled then null else 
        //let generalOptions = Setting.getGeneralOptions serviceProvider
//        let mutable doc = Unchecked.defaultof<ITextDocument>
//        if not(textDocumentFactoryService.TryGetTextDocument (buffer,&doc)) then None else
//            let d = doc
        maybe{
            let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
            return buffer.Properties.GetOrCreateSingletonProperty (fun () ->
                new OutliningTagger
                    (   doc
                    ,   serviceProvider
                    ,   textEditorFactoryService
                    ,   projectionBufferFactoryService
                    ,   projectFactory
                    ,   vsLanguageService               
                )) :?> _ 
        } // |> Some

    interface ITaggerProvider with
        member __.CreateTagger buffer = 
            match self.CreateTagger buffer with
            | None -> null
            | Some tg ->  tg  :> obj :?> _

    member __.TextViewCreated (textView: IWpfTextView) : unit = 
//        let generalOptions = Setting.getGeneralOptions serviceProvider
//        if not generalOptions.OutliningEnabled then () else
        let textBuffer = textView.TextBuffer
        match (self :> ITaggerProvider).CreateTagger<IOutliningRegionTag> textBuffer with
        | null -> ()
        | outliningTagger ->
            let isFirstOutlining = ref true
            outliningTagger.TagsChanged.Add ( fun _ ->
                if !isFirstOutlining then
                    let fullspan = SnapshotSpan(textView.TextSnapshot, 0, textView.TextSnapshot.Length)
                    // ensure that first tags have been computed
                    let tags = outliningTagger.GetTags( NormalizedSnapshotSpanCollection fullspan )
                    let outliningManager = outliningManagerService.GetOutliningManager textView
                    outliningManager.CollapseAll
                        ( fullspan, fun (c:ICollapsible) -> c.Tag.IsDefaultCollapsed ) |> ignore
                    isFirstOutlining := false
            ) 



    interface IWpfTextViewCreationListener with
        member __.TextViewCreated textView  =  self.TextViewCreated textView


        
            
            
            
            






