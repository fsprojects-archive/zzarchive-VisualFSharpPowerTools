using FSharp.Editing.VisualStudio;
using FSharp.Editing.VisualStudio.ProjectSystem;
using FSharp.Editing.VisualStudio.Symbol;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;

namespace FSharpVSPowerTools.QuickInfo
{
    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(Constants.QuickInfoMargin)]
    [Order(After = PredefinedMarginNames.HorizontalScrollBar)]
    [MarginContainer(PredefinedMarginNames.Bottom)]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class QuickInfoMarginProvider : IWpfTextViewMarginProvider
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _vsLanguageService;

        [ImportingConstructor]
        public QuickInfoMarginProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService vsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _vsLanguageService = vsLanguageService;
        }

        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost textViewHost, IWpfTextViewMargin marginContainer)
        {
            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.QuickInfoPanelEnabled) return null;

            var textView = textViewHost.TextView;
            var buffer = textView.TextBuffer;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
                return new QuickInfoMargin(doc, textView, _vsLanguageService, _projectFactory);
            else
                return null;
        }
    }

}
