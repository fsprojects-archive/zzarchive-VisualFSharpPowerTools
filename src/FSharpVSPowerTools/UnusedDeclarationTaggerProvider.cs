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
using Microsoft.VisualStudio.Text.Classification;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(UnusedDeclarationTag))]
    public class UnusedDeclarationTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal IClassifierAggregatorService aggregatorService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide tagging on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.UnusedDeclarationsEnabled) return null;

            return new UnusedDeclarationTagger(buffer, aggregatorService.GetClassifier(buffer)) as ITagger<T>;
        }
    }
}