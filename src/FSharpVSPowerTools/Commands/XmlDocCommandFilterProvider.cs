using System;
using System.Diagnostics;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text;
using FSharpVSPowerTools.XmlDoc;
using FSharpVSPowerTools.ProjectSystem;

// Useful reference: http://msdn.microsoft.com/en-us/library/dd885243.aspx
namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class XmlDocCommandFilterProvider : IVsTextViewCreationListener
    {
        private readonly System.IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly IVsEditorAdaptersFactoryService _editorFactory;
        private readonly IOpenDocumentsTracker _openDocumentTracker;
       
        [ImportingConstructor]
        public XmlDocCommandFilterProvider(
            [Import(typeof(SVsServiceProvider))] System.IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            IVsEditorAdaptersFactoryService editorFactory,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService,
            IOpenDocumentsTracker openDocumentTracker)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _editorFactory = editorFactory;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
            _openDocumentTracker = openDocumentTracker;
        }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var wpfTextView = _editorFactory.GetWpfTextView(textViewAdapter);
            if (wpfTextView == null) return;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.XmlDocEnabled) return;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(wpfTextView.TextBuffer, out doc))
                new XmlDocFilter(textViewAdapter, wpfTextView, doc.FilePath, _projectFactory, _fsharpVsLanguageService, _openDocumentTracker);
        }
    }
}
