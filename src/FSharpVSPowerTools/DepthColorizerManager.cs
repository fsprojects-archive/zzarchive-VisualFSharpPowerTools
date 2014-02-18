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

using FSharpDepthColorizer;

namespace FSharpVSPowerTools
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(FSharpRegionTag))]
    public class DepthColorizerTaggerProvider : ITaggerProvider
    {
        [Import]
        internal ITextDocumentFactoryService TextDocumentFactoryService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            ITextDocument doc;

            if (TextDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return new FSharpTagger(buffer, doc.FilePath) as ITagger<T>;
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
        [Name("FSharpDepthFullLineAdornment")]
        [Order(Before=PredefinedAdornmentLayers.Selection)]
        internal AdornmentLayerDefinition AdornmentLayerDefinition { get; set; }

        [Import]
        internal IViewTagAggregatorFactoryService ViewTagAggregatorFactoryService { get; set; }

        public void TextViewCreated(IWpfTextView textView)
        {
            if (textView == null) return;

            var tagAggregator = ViewTagAggregatorFactoryService.CreateTagAggregator<FSharpRegionTag>(textView);
            new FullLineAdornmentManager(textView, tagAggregator);
        }
    }
}
