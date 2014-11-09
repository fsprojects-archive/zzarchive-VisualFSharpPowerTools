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

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    internal class FindReferencesFilterProvider : IWpfTextViewCreationListener
    {
        [Import]
        internal IVsEditorAdaptersFactoryService editorFactory = null;

        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        internal System.IServiceProvider serviceProvider = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        [Import]
        internal FileSystem fileSystem = null; 

        internal FindReferencesFilter RegisterCommandFilter(IWpfTextView textView, bool showProgress)
        {
            var textViewAdapter = editorFactory.GetViewAdapter(textView);
            if (textViewAdapter == null) return null;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.FindAllReferencesEnabled) return null;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(textView.TextBuffer, out doc))
            {
                Debug.Assert(doc != null, "Text document shouldn't be null.");
                var filter = new FindReferencesFilter(doc, textView, fsharpVsLanguageService,
                                                serviceProvider, projectFactory, showProgress, fileSystem);
                AddCommandFilter(textViewAdapter, filter);
                return filter;
            }
            return null;
        }

        public void TextViewCreated(IWpfTextView textView)
        {
            RegisterCommandFilter(textView, showProgress: true);
        }

        private static void AddCommandFilter(IVsTextView viewAdapter, FindReferencesFilter commandFilter)
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