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
using FSharpVSPowerTools.Refactoring;
using Microsoft.VisualStudio.Text;

namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    internal class RenameCommandFilterProvider : IVsTextViewCreationListener
    {
        private readonly System.IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly IVsEditorAdaptersFactoryService _editorFactory;
        private readonly IGeneralOptions _generalOptions;
        

        [ImportingConstructor]
        public RenameCommandFilterProvider(
            [Import(typeof(SVsServiceProvider))] System.IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            IVsEditorAdaptersFactoryService editorFactory,
            ProjectFactory projectFactory,
            IGeneralOptions generalOptions,
        
        VSLanguageService fsharpVsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _editorFactory = editorFactory;
            _projectFactory = projectFactory;
            _generalOptions = generalOptions;
            _fsharpVsLanguageService = fsharpVsLanguageService;
        }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var textView = _editorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

           // var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.RenameRefactoringEnabled) return;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(textView.TextBuffer, out doc))
            {
                AddCommandFilter(textViewAdapter,
                    new RenameCommandFilter(doc, textView, _fsharpVsLanguageService,
                                            _serviceProvider, _projectFactory));
            }
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