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
        private ITextSearchService textSearchService = null;

        [Import]
        private VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider = null;

        [Import(typeof(ProjectFactory))]
        private ProjectFactory projectFactory = null;

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.HighlightUsageEnabled) return null;

            return new HighlightUsageTagger(textView, buffer, textSearchService, fsharpVsLanguageService, serviceProvider,
                                            projectFactory) as ITagger<T>;
        }
    }
}