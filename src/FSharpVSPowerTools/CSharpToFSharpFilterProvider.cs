﻿using System;
using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text;
using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.CSharpToFSharp;

// Useful reference: http://msdn.microsoft.com/en-us/library/dd885243.aspx
namespace FSharpVSPowerTools
{
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class CSharpToFSharpFilterProvider : IVsTextViewCreationListener
    {
        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import]
        internal IVsEditorAdaptersFactoryService editorFactory = null;

        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        internal System.IServiceProvider serviceProvider = null;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            var wpfTextView = editorFactory.GetWpfTextView(textViewAdapter);
            if (wpfTextView == null) return;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.CSharpToFSharpEnabled) return;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(wpfTextView.TextBuffer, out doc))
            {
                Debug.Assert(doc != null, "Text document shouldn't be null.");
                new CSharpToFSharpFilter(textViewAdapter, wpfTextView, doc.FilePath, fsharpVsLanguageService);
            }
        }
    }
}
