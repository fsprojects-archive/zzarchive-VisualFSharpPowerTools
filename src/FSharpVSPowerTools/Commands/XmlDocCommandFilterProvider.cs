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
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class XmlDocCommandFilterProvider : IVsTextViewCreationListener
    {
        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        internal IVsEditorAdaptersFactoryService editorFactory = null;

        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        [Import(typeof(SVsServiceProvider))]
        internal System.IServiceProvider serviceProvider = null;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var wpfTextView = editorFactory.GetWpfTextView(textViewAdapter);
            if (wpfTextView == null) return;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.XmlDocEnabled) return;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(wpfTextView.TextBuffer, out doc))
            {
                new XmlDocFilter(textViewAdapter, wpfTextView, doc.FilePath, projectFactory, fsharpVsLanguageService, serviceProvider);
            }
        }
    }
}
