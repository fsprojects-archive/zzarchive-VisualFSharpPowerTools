using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.SymbolInfo;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;

namespace Winterdom.Viasfora.Margins
{
    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(FSharpVSPowerTools.Constants.symbolInfoMargin)]
    [Order(After = PredefinedMarginNames.HorizontalScrollBar)]
    [MarginContainer(PredefinedMarginNames.Bottom)]
    [ContentType("text")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    public class DevMarginProvider : IWpfTextViewMarginProvider
    {
        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        VSLanguageService languageService = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost textViewHost, IWpfTextViewMargin marginContainer)
        {
//            var generalOptions = Setting.getGeneralOptions(serviceProvider);
//            if (generalOptions == null || !generalOptions.HighlightUsageEnabled) return null;

            var textView = textViewHost.TextView;
            var buffer = textView.TextBuffer;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
                return new SymbolInfoMargin(doc, textView, languageService, serviceProvider, projectFactory);
            else
                return null;
        }
    }

}
