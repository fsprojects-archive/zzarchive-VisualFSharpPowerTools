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

// Useful reference: http://msdn.microsoft.com/en-us/library/dd885243.aspx
namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class VsTextViewCommandFilter : IVsTextViewCreationListener
    {
        [Import(typeof(ITextDocumentFactoryService))]
        public ITextDocumentFactoryService TextDocumentFactoryService { get; set; }

        [Import(typeof(IVsEditorAdaptersFactoryService))]
        public IVsEditorAdaptersFactoryService EditorFactory { get; set; }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var wpfTextView = EditorFactory.GetWpfTextView(textViewAdapter);
            if (wpfTextView == null) return;

            GeneralOptionsPage generalOptions = (GeneralOptionsPage)(Package.GetGlobalService(typeof(GeneralOptionsPage)));
            if (!generalOptions.XMLDocEnabled)
            {
                Debug.WriteLine("[XMLDoc] The feature is disabled in General option page.");
                return;
            }

            ITextDocument doc;
            if (TextDocumentFactoryService.TryGetTextDocument(wpfTextView.TextBuffer, out doc))
            {
                new FSharpXmlDoc.XmlDocFilter(textViewAdapter, wpfTextView, doc.FilePath);
            }
        }
    }
}
