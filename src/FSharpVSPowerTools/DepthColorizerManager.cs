using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using System.Windows.Media;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Win32;
using FSharpVSPowerTools.DepthColorizer;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;
using FSharpVSPowerTools.ProjectSystem;

namespace FSharpVSPowerTools
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(DepthRegionTag))]
    public class DepthColorizerTaggerProvider : ITaggerProvider
    {
        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            ITextDocument doc;

            var generalOptions = Utils.GetGeneralOptionsPage(serviceProvider);
            if (generalOptions == null || !generalOptions.DepthColorizerEnabled) return null;

            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return new DepthTagger(buffer, doc.FilePath, fsharpVsLanguageService) as ITagger<T>;
            }

            return null;
        }
    }

    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Structured)]
    public class DepthColorizerAdornment : IWpfTextViewCreationListener
    {
        [Export]
        [Name(Constants.depthAdornmentLayerName)]
        [Order(Before = PredefinedAdornmentLayers.CurrentLineHighlighter)]
        private AdornmentLayerDefinition AdornmentLayerDefinition { get; set; }

        [Import]
        private IViewTagAggregatorFactoryService viewTagAggregatorFactoryService = null;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider = null;

        public void TextViewCreated(IWpfTextView textView)
        {
            if (textView == null) return;

            var generalOptions = Utils.GetGeneralOptionsPage(serviceProvider);
            if (!generalOptions.DepthColorizerEnabled) return;

            var tagAggregator = viewTagAggregatorFactoryService.CreateTagAggregator<DepthRegionTag>(textView);
            new FullLineAdornmentManager(textView, tagAggregator, serviceProvider);
        }
    }
}
