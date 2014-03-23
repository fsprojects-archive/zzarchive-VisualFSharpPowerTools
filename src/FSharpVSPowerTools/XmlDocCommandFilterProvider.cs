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
        private ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        private IVsEditorAdaptersFactoryService editorFactory = null;

        [Import]
        private VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        private System.IServiceProvider serviceProvider = null;

        [Import]
        private Logger logger = null;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var wpfTextView = editorFactory.GetWpfTextView(textViewAdapter);
            if (wpfTextView == null) return;

            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.XmlDocEnabled)
            {
                logger.Log(LogType.Information, "XMLDoc feature is disabled in General option page.");
                return;
            }

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(wpfTextView.TextBuffer, out doc))
            {
                new XmlDocFilter(textViewAdapter, wpfTextView, doc.FilePath, fsharpVsLanguageService);
            }
        }
    }
}
