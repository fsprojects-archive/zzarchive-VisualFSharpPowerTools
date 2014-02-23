using System.ComponentModel.Composition;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;

namespace FSharpVSPowerTools.Refactoring
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    internal class RenameCommandFilterProvider : IVsTextViewCreationListener
    {
        [Import(typeof(IVsEditorAdaptersFactoryService))]
        public IVsEditorAdaptersFactoryService EditorFactory;

        [Import]
        internal ITextUndoHistoryRegistry UndoHistoryRegistry { get; set; }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var textView = EditorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

            IVsTextLines buffer;
            if (textViewAdapter.GetBuffer(out buffer) != VSConstants.S_OK) return;

            AddCommandFilter(textViewAdapter, 
                new RenameCommandFilter(
                    textView, 
                    UndoHistoryRegistry.RegisterHistory(EditorFactory.GetDocumentBuffer(buffer)), 
                    PowerToolsCommandsPackage.Instance));
        }

        private static void AddCommandFilter(IVsTextView viewAdapter, RenameCommandFilter commandFilter)
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