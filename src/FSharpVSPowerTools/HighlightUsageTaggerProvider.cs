using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using FSharpVSPowerTools.HighlightUsage;
using FSharpVSPowerTools.ProjectSystem;
using System;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(HighlightUsageTag))]
    public class HighlightUsageTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal ITextSearchService TextSearchService { get; set; }

        [Import]
        private VSLanguageService fsharpVsLanguageService;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider;

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            GeneralOptionsPage generalOptions = (GeneralOptionsPage)(Package.GetGlobalService(typeof(GeneralOptionsPage)));
            if (!generalOptions.HighlightUsageEnabled)
            {
                Debug.WriteLine("[Highlight Usage] The feature is disabled in General option page.");
                return null;
            }

            return new HighlightUsageTagger(textView, buffer, TextSearchService, fsharpVsLanguageService, serviceProvider) as ITagger<T>;
        }
    }
}