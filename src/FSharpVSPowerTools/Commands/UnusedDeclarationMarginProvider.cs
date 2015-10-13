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
using FSharpVSPowerTools.SyntaxColoring;

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(Constants.fsharpUnusedDeclarationMargin)]
    [ContentType("F#")]
    [Order(After = PredefinedMarginNames.VerticalScrollBar)]
    [MarginContainer(PredefinedMarginNames.VerticalScrollBarContainer)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class UnusedDeclarationMarginProvider : IWpfTextViewMarginProvider
    {
        [Import]
        internal IViewTagAggregatorFactoryService viewTagAggregatorFactoryService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost wpfTextViewHost, IWpfTextViewMargin marginContainer)
        {
            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !(generalOptions.UnusedReferencesEnabled || generalOptions.UnusedOpensEnabled)) return null;

 	        var textView = wpfTextViewHost.TextView;
            var tagAggregator = viewTagAggregatorFactoryService.CreateTagAggregator<UnusedDeclarationTag>(textView);
            return new UnusedDeclarationMargin(textView, marginContainer, tagAggregator);
        }
    }
}
