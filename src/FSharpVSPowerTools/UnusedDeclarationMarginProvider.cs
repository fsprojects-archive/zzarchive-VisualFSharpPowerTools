﻿using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(Constants.fsharpUnusedDeclarationMargin)]
    [ContentType("F#")]
    [Order(After = PredefinedMarginNames.VerticalScrollBar)]
    [MarginContainer(PredefinedMarginNames.VerticalScrollBarContainer)]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    public class UnusedDeclarationMarginProvider : IWpfTextViewMarginProvider
    {
        [Import]
        internal IClassifierAggregatorService classifierAggregatorService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost wpfTextViewHost, IWpfTextViewMargin marginContainer)
        {
            var generalOptions = Utils.GetGeneralOptionsPage(serviceProvider);
            if (!generalOptions.UnusedDeclarationsEnabled) return null;

 	        var textView = wpfTextViewHost.TextView;
            return new UnusedDeclarationMargin(textView, marginContainer,
                           classifierAggregatorService.GetClassifier(textView.TextBuffer));
        }
    }
}
