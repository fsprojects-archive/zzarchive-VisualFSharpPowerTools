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
using FSharpVSPowerTools.Linting;
using Microsoft.VisualStudio.Text.Classification;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Language.Intellisense;

namespace FSharpVSPowerTools
{
    [Export(typeof(IQuickInfoSourceProvider))]
    [Name("F# Lint Quick Info Provider")]
    [Order(Before = "Default Quick Info Presenter")]
    [ContentType("F#")]
    internal class LintQuickInfoProvider : IQuickInfoSourceProvider
    {
        [Import]
        internal IViewTagAggregatorFactoryService viewTagAggregatorFactoryService = null;

        public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
        {
            return new LintQuickInfoSource(textBuffer, viewTagAggregatorFactoryService);
        }
    }
}