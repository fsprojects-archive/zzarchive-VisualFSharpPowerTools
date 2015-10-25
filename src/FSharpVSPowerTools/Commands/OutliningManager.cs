using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Outlining;
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
    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType("F#")]
    internal class OutliningManager : IWpfTextViewCreationListener
    {
        [Import]
        internal IOutliningManagerService outliningManagerService = null;

        [Import(typeof(SVsServiceProvider))]
        internal System.IServiceProvider serviceProvider = null;

        [Import]
        internal OutliningTaggerProvider taggerProvider = null;

        public void TextViewCreated(IWpfTextView textView)
        {
            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.OutliningEnabled) return;
            var textBuffer = textView.TextBuffer;
            var outliningTagger = taggerProvider.CreateTagger<IOutliningRegionTag>(textBuffer);
            outliningTagger.TagsChanged += (sender, e) =>
                {
                    var fullSpan = new SnapshotSpan(textView.TextSnapshot, 0, textView.TextSnapshot.Length);
                    // Ensure that first tags have been computed.
                    var tags = outliningTagger.GetTags(new NormalizedSnapshotSpanCollection(fullSpan));
                    var outliningManager = outliningManagerService.GetOutliningManager(textView);
                    if (outliningManager != null)
                    {
                        var results = outliningManager.GetAllRegions(fullSpan);
                        foreach (ICollapsible region in results)
                        {
                            outliningManager.TryCollapse(region);
                        }
                        return;
                    }
                };
        }
    }
}
