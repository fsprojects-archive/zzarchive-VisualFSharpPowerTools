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
    internal class TaskListCommentFilterProvider : IVsTextViewCreationListener
    {
        [Import]
        private IVsEditorAdaptersFactoryService editorFactory = null;

        [Import(typeof(SVsServiceProvider))]
        private System.IServiceProvider serviceProvider = null;

        [Import(typeof(OpenDocumentsTracker))]
        private OpenDocumentsTracker openDocsTracker = null;

        private IWpfTextView textView;
        private TaskListCommentFilter taskCommentFilter;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            textView = editorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

            var generalOptions = Utils.GetGeneralOptionsPage(serviceProvider);
            if (!generalOptions.TaskListCommentsEnabled) return;

            taskCommentFilter = new TaskListCommentFilter(textView, serviceProvider, openDocsTracker);
        }
    }
}