using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.TaskList;
using Microsoft.VisualStudio.Editor;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.TextManager.Interop;

namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    internal class TaskListCommentFilterProvider : IVsTextViewCreationListener
    {
        internal IWpfTextView _textView;
        internal TaskListCommentFilter _taskCommentFilter;

        readonly System.IServiceProvider _serviceProvider;
        readonly IVsEditorAdaptersFactoryService _editorFactory;
        readonly TaskListManager _taskListManager;
        readonly IOpenDocumentsTracker _openDocumentTracker;

        private readonly IGeneralOptions _generalOptions;

        [ImportingConstructor]
        public TaskListCommentFilterProvider(
            [Import(typeof(SVsServiceProvider))] System.IServiceProvider serviceProvider,
            IVsEditorAdaptersFactoryService editorFactory,
            TaskListManager taskListManager,
            IGeneralOptions generalOptions,

        IOpenDocumentsTracker openDocumentTracker)
        {
            _serviceProvider = serviceProvider;
            _editorFactory = editorFactory;
            _taskListManager = taskListManager;
            _generalOptions = generalOptions;
            _openDocumentTracker = openDocumentTracker;
        }
        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            _textView = _editorFactory.GetWpfTextView(textViewAdapter);
            if (_textView == null) return;

            //var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.TaskListCommentsEnabled) return;

            _taskCommentFilter = new TaskListCommentFilter(_textView, _serviceProvider, _taskListManager, _openDocumentTracker);
        }
    }
}