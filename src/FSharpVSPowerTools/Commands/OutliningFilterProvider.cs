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
using FSharpVSPowerTools.Navigation;
using Microsoft.VisualStudio.Text;

namespace FSharpVSPowerTools.Outlining
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    internal class OutliningFilterProvider : IVsTextViewCreationListener
    {
        private readonly System.IServiceProvider _serviceProvider;
        private readonly IVsEditorAdaptersFactoryService _editorFactory;
        private readonly IGeneralOptions _generalOptions;

        [ImportingConstructor]
        public OutliningFilterProvider(
            [Import(typeof(SVsServiceProvider))] System.IServiceProvider serviceProvider,
                    IGeneralOptions generalOptions,

        IVsEditorAdaptersFactoryService editorFactory)
        {
            _serviceProvider = serviceProvider;
            _editorFactory = editorFactory;
            _generalOptions = generalOptions;

        }

        private static void AddCommandFilter(IVsTextView viewAdapter, OutliningFilter commandFilter)
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

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var textView = _editorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

            //var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.OutliningEnabled) return;

            AddCommandFilter(textViewAdapter, new OutliningFilter());
        }
    }
}