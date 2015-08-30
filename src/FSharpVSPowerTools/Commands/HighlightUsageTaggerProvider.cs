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
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        private static readonly Type serviceType = typeof(HighlightUsageTagger);

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.HighlightUsageEnabled) return null;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return buffer.Properties.GetOrCreateSingletonProperty(serviceType, 
                    () => new HighlightUsageTagger(doc, textView, fsharpVsLanguageService, serviceProvider, projectFactory) as ITagger<T>);
            }

            return null;
        }
    }
}