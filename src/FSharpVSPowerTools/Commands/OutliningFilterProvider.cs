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
    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    internal class OutliningFilterProvider : IWpfTextViewCreationListener
    {
        private readonly System.IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly IVsEditorAdaptersFactoryService _editorFactory;
        private readonly FileSystem _fileSystem;

        [ImportingConstructor]
        public OutliningFilterProvider(
            [Import(typeof(SVsServiceProvider))] System.IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            IVsEditorAdaptersFactoryService editorFactory,
            FileSystem fileSystem,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _editorFactory = editorFactory;
            _fileSystem = fileSystem;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
        }

        public void TextViewCreated(IWpfTextView textView)
        {
            var textViewAdapter = _editorFactory.GetViewAdapter(textView);
            if (textViewAdapter == null) return;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.OutliningEnabled) return;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(textView.TextBuffer, out doc))
            {
                var filter = new OutliningFilter(textView, null);
                AddCommandFilter(textViewAdapter, filter);
            }
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
    }
}