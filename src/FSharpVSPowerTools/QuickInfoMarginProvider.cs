using FSharpVSPowerTools;
using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.QuickInfo;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;

namespace Winterdom.Viasfora.Margins
{
    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(FSharpVSPowerTools.Constants.QuickInfoMargin)]
    [Order(After = PredefinedMarginNames.HorizontalScrollBar)]
    [MarginContainer(PredefinedMarginNames.Bottom)]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    public class QuickInfoMarginProvider : IWpfTextViewMarginProvider
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
            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.QuickInfoPanelEnabled) return null;

            var textView = textViewHost.TextView;
            var buffer = textView.TextBuffer;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
                return new QuickInfoMargin(doc, textView, languageService, serviceProvider, projectFactory);
            else
                return null;
        }
    }

}
