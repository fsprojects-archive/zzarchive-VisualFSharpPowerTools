module FSharpVSPowerTools.OutliningTaggerProvider


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

    interface ITaggerProvider with
        member __.CreateTagger buffer = 
            maybe{
             //   let generalOptions = Setting.getGeneralOptions serviceProvider
                let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
            //    if not generalOptions.OutliningEnabled then return! None else
                return buffer.Properties.GetOrCreateSingletonProperty (fun () ->
                    new OutliningTagger (doc, serviceProvider, textEditorFactoryService, 
                            projectionBufferFactoryService, projectFactory, vsLanguageService)
                ) :> obj :?> _
            } |> Option.getOrElse null


    interface IWpfTextViewCreationListener with
        member __.TextViewCreated textView  =  
            maybe{
                let generalOptions = Setting.getGeneralOptions serviceProvider  
                if not generalOptions.OutliningEnabled then return () else
                let textBuffer = textView.TextBuffer
                match (self :> ITaggerProvider).CreateTagger<IOutliningRegionTag> textBuffer with
                | null -> ()
                | outliningTagger ->
                    let isFirstOutlining = ref true
                    outliningTagger.TagsChanged.Add ( fun _ ->
                        if !isFirstOutlining then
                            let fullspan = SnapshotSpan (textView.TextSnapshot, 0, textView.TextSnapshot.Length)
                            // ensure that first tags have been computed
                            outliningTagger.GetTags( NormalizedSnapshotSpanCollection fullspan ) |> ignore
                            let outliningManager = outliningManagerService.GetOutliningManager textView
                            outliningManager.CollapseAll
                                ( fullspan, fun (c:ICollapsible) -> c.Tag.IsDefaultCollapsed ) |> ignore
                            isFirstOutlining := false
                    )         
            } |> ignore
        


        
            
            
            
            






