using FSharpVSPowerTools.Linting;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;

namespace FSharpVSPowerTools
{
    [Export(typeof(IQuickInfoSourceProvider))]
    [Name("F# Lint Quick Info Provider")]
    [Order(Before = "Default Quick Info Presenter")]
    [ContentType("F#")]
    internal class LintQuickInfoProvider : IQuickInfoSourceProvider
    {
        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        [Import]
        internal IViewTagAggregatorFactoryService viewTagAggregatorFactoryService = null;

        public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
        {
            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.LinterEnabled) return null;

            return new LintQuickInfoSource(textBuffer, viewTagAggregatorFactoryService);
        }
    }
}