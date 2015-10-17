using System.ComponentModel.Composition;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace FSharpVSPowerTools
{
    [Export(typeof(ITaggerProvider))]
    [TagType(typeof(IOutliningRegionTag))]
    [ContentType("F#")]
    public class OutliningTaggerProvider : ITaggerProvider
    {
        private readonly SVsServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _vsLanguageService;

        [ImportingConstructor]
        public OutliningTaggerProvider(
            SVsServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService vsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _vsLanguageService = vsLanguageService;
        }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || generalOptions.OutliningEnabled == false)
                return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return (ITagger<T>)buffer.Properties.GetOrCreateSingletonProperty(() =>
                   new Outlining.OutliningTagger(                       
                       doc,
                       _serviceProvider,
                       _projectFactory,
                       _vsLanguageService));
            }

            return null;
        }
    }
}