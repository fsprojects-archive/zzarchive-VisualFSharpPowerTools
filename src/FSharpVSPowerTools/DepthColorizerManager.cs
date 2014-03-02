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
        internal ITextDocumentFactoryService TextDocumentFactoryService { get; set; }

        [Import]
        internal VSLanguageService FsharpLanguageService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            ITextDocument doc;

            GeneralOptionsPage generalOptions = (GeneralOptionsPage)(Package.GetGlobalService(typeof(GeneralOptionsPage)));
            if (!generalOptions.DepthColorizerEnabled)
            {
                Debug.WriteLine("[Depth Colorizer] The feature is disabled in General option page.");
                return null;
            }

            if (TextDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return new DepthTagger(buffer, doc.FilePath, FsharpLanguageService) as ITagger<T>;
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
        [Order(Before = PredefinedAdornmentLayers.TextMarker)]
        internal AdornmentLayerDefinition AdornmentLayerDefinition { get; set; }

        [Import]
        internal IViewTagAggregatorFactoryService ViewTagAggregatorFactoryService { get; set; }

        public void TextViewCreated(IWpfTextView textView)
        {
            if (textView == null) return;

            var tagAggregator = ViewTagAggregatorFactoryService.CreateTagAggregator<DepthRegionTag>(textView);
            new FullLineAdornmentManager(textView, tagAggregator);
        }
    }
}
