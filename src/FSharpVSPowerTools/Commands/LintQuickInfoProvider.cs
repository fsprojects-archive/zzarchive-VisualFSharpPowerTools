using FSharpVSPowerTools.Linting;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
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
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    internal class LintQuickInfoProvider : IQuickInfoSourceProvider
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly IViewTagAggregatorFactoryService _viewTagAggregatorFactoryService;
        private readonly IGeneralOptions _generalOptions;


        [ImportingConstructor]
        public LintQuickInfoProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            IGeneralOptions generalOptions,
            IViewTagAggregatorFactoryService viewTagAggregatorFactoryService)
        {
            _serviceProvider = serviceProvider;
            _viewTagAggregatorFactoryService = viewTagAggregatorFactoryService;
            _generalOptions = generalOptions;
    }

        public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
        {
            //var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.LinterEnabled) return null;

            return new LintQuickInfoSource(textBuffer, _viewTagAggregatorFactoryService);
        }
    }
}