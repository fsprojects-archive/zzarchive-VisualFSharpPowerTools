using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using FSharpVSPowerTools.HighlightUsage;
using Microsoft.VisualStudio.Shell;
using EnvDTE;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [FileExtension(".fs")]
    [TagType(typeof(HighlightUsageTag))]
    public class HighlightUsageTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal ITextSearchService TextSearchService { get; set; }

        [Import]
        internal ITextStructureNavigatorSelectorService TextStructureNavigatorSelector { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var textStructureNavigator = TextStructureNavigatorSelector.GetTextStructureNavigator(buffer);
            var tagger = new HighlightUsageTagger(textView, buffer, textStructureNavigator);
            tagger.Initialize();
            return tagger as ITagger<T>;
        }
    }
}