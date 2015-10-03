using System.ComponentModel.Composition;
using FSharpVSPowerTools.Outlining;
using FSharpVSPowerTools.ProjectSystem;
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
        private readonly VSLanguageService _vsLanguageService;

        [ImportingConstructor]
        public OutliningTaggerProvider(VSLanguageService vsLanguageService)
        {
            _vsLanguageService = vsLanguageService;
        }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            return (ITagger<T>) buffer.Properties.GetOrCreateSingletonProperty(() =>
                new OutliningTagger(buffer, _vsLanguageService));
        }
    }
}