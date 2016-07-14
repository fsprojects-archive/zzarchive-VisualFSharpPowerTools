using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.CodeFormatting;
using FSharpVSPowerTools.ProjectSystem;

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [Name("F# Formatting Command Hook")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class CodeFormattingHookHelper : IWpfTextViewCreationListener
    {
        private readonly IVsEditorAdaptersFactoryService _adaptersFactory;
        private readonly IEditorOptionsFactoryService _editorOptionsFactory;
        private readonly IEditorOperationsFactoryService _editorOperationsFactoryService;
        private readonly ITextBufferUndoManagerProvider _textBufferUndoManagerProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly IServiceProvider _serviceProvider;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _vsLanguageService;
        private readonly IOpenDocumentsTracker _openDocumentTracker;

        [ImportingConstructor]
        public CodeFormattingHookHelper(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider, 
            IVsEditorAdaptersFactoryService adaptersFactory, 
            IEditorOptionsFactoryService editorOptionsFactory,
            IEditorOperationsFactoryService editorOperationsFactoryService,
            ITextBufferUndoManagerProvider textBufferUndoManagerProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService vsLanguageService,
            IOpenDocumentsTracker openDocumentTracker)
        {
            _serviceProvider = serviceProvider;
            _adaptersFactory = adaptersFactory;
            _editorOptionsFactory = editorOptionsFactory;
            _editorOperationsFactoryService = editorOperationsFactoryService;
            _textBufferUndoManagerProvider = textBufferUndoManagerProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _vsLanguageService = vsLanguageService;
            _openDocumentTracker = openDocumentTracker;
        }

        internal StandardCommandDispatcher RegisterCommandDispatcher(IWpfTextView wpfTextView)
        {
            var view = _adaptersFactory.GetViewAdapter(wpfTextView);
            if (view != null)
            {
                return StandardCommandDispatcher.Register(view, wpfTextView, GetServices());
            }
            return null;
        }

        public void TextViewCreated(IWpfTextView wpfTextView)
        {
            System.Windows.Threading.Dispatcher.CurrentDispatcher.BeginInvoke(new Action(() =>
            {
                RegisterCommandDispatcher(wpfTextView);
            }));
        }

        private CodeFormattingServices GetServices()
        {
            return new CodeFormattingServices(_editorOptionsFactory, _editorOperationsFactoryService,
                            _textBufferUndoManagerProvider, _textDocumentFactoryService,
                            _projectFactory, _vsLanguageService, _openDocumentTracker, _serviceProvider);
        }
    }
}
