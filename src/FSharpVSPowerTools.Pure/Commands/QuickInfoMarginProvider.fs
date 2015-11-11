namespace FSharpVSPowerTools.QuickInfo

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open System
open System.ComponentModel.Composition

//namespace 
////{
//[<Export(typeof<IWpfTextViewMarginProvider>)>]
//[<Name(Constants.QuickInfoMargin)>]
//[<Order(After = PredefinedMarginNames.HorizontalScrollBar)>]
//[<MarginContainer(PredefinedMarginNames.Bottom)>]
//[<ContentType("F#")>]
//[<TextViewRole(PredefinedTextViewRoles.Document)>]
//type QuickInfoMarginProvider [<ImportingConstructor>] 
//    (   [<Import(typeof<SVsServiceProvider>)>] 
//        serviceProvider                 :   IServiceProvider            ,
//        textDocumentFactoryService      :   ITextDocumentFactoryService ,
//        projectFactory                  :   ProjectFactory              ,
//        vsLanguageService               :   VSLanguageService           ) as self =
//
//    member x.CreateMargin(wpfTextViewHost: IWpfTextViewHost, marginContainer: IWpfTextViewMargin) = 
//        //let generalOptions = Setting.getGeneralOptions serviceProvider
//        //if not generalOptions.QuickInfoPanelEnabled then null else 
//        let textView = wpfTextViewHost.TextView
//        let buffer = textView.TextBuffer
//        maybe {
//            let! doc = textDocumentFactoryService.TryDocumentFromBuffer buffer
//            return
//                new QuickInfoMargin( doc, textView, vsLanguageService, serviceProvider, projectFactory)
//                :> IWpfTextViewMargin
//        } 
//
//
//    interface IWpfTextViewMarginProvider with
//        member x.CreateMargin(wpfTextViewHost: IWpfTextViewHost, marginContainer: IWpfTextViewMargin): IWpfTextViewMargin = 
//            self.CreateMargin (wpfTextViewHost,marginContainer) 
//            |> Option.getOrElse marginContainer


    
//        private readonly IServiceProvider _serviceProvider
//        private readonly ITextDocumentFactoryService _textDocumentFactoryService
//        private readonly ProjectFactory _projectFactory
//        private readonly VSLanguageService _vsLanguageService
//
//        [ImportingConstructor]
//        public QuickInfoMarginProvider(
//            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
//            ITextDocumentFactoryService textDocumentFactoryService,
//            ProjectFactory projectFactory,
//            VSLanguageService vsLanguageService)
//        {
//            _serviceProvider = serviceProvider
//            _textDocumentFactoryService = textDocumentFactoryService
//            _projectFactory = projectFactory
//            _vsLanguageService = vsLanguageService
//        }
//
//        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost textViewHost, IWpfTextViewMargin marginContainer)
//        {
//            var generalOptions = Setting.getGeneralOptions(_serviceProvider)
//            if (generalOptions == null || !generalOptions.QuickInfoPanelEnabled) return null
//
//            var textView = textViewHost.TextView
//            var buffer = textView.TextBuffer
//
//            ITextDocument doc
//            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
//                return new QuickInfoMargin(doc, textView, _vsLanguageService, _serviceProvider, _projectFactory)
//            else
//                return null
//        }
//    }
//
//}
