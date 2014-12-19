using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.HighlightUsage;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;

namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    internal class HighlightUsageFilterProvider : IVsTextViewCreationListener
    {
        [Import]
        internal IVsEditorAdaptersFactoryService editorFactory = null;

        [Import(typeof(SVsServiceProvider))]
        internal System.IServiceProvider serviceProvider = null;

        [Import]
        internal IViewTagAggregatorFactoryService tagAggregator = null;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var textView = editorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.HighlightUsageEnabled) return;
            
            AddCommandFilter(textViewAdapter,
                new HighlightUsageFilter(textView, tagAggregator.CreateTagAggregator<TextMarkerTag>(textView)));            
        }

        private static void AddCommandFilter(IVsTextView viewAdapter, HighlightUsageFilter commandFilter)
        {
            if (!commandFilter.IsAdded)
            {
                // Get the view adapter from the editor factory
                IOleCommandTarget next;
                int hr = viewAdapter.AddCommandFilter(commandFilter, out next);

                if (hr == VSConstants.S_OK)
                {
                    commandFilter.IsAdded = true;
                    // You'll need the next target for Exec and QueryStatus
                    if (next != null) commandFilter.NextTarget = next;
                }
            }
        }
    }
}