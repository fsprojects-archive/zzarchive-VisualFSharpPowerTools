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
        internal IVsEditorAdaptersFactoryService editorFactory = null;

        [Import(typeof(SVsServiceProvider))]
        internal System.IServiceProvider serviceProvider = null;

        [Import]
        internal OpenDocumentsTracker openDocsTracker = null;

        internal IWpfTextView textView;
        internal TaskListCommentFilter taskCommentFilter;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            textView = editorFactory.GetWpfTextView(textViewAdapter);
            if (textView == null) return;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.TaskListCommentsEnabled) return;

            taskCommentFilter = new TaskListCommentFilter(textView, serviceProvider, openDocsTracker);
        }
    }
}