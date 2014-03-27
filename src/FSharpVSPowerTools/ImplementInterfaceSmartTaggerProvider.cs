using System;
using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using FSharpVSPowerTools.Refactoring;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Editor;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(ImplementInterfaceSmartTag))]
    public class ImplementInterfaceSmartTaggerProvider : IViewTaggerProvider
    {
        [Import]
        private VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider = null;

        [Import]
        private IEditorOptionsFactoryService editorOptionsFactory = null;

        [Import]
        private ITextUndoHistoryRegistry undoHistoryRegistry = null;

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            return new ImplementInterfaceSmartTagger(textView, buffer, editorOptionsFactory,
                        undoHistoryRegistry.RegisterHistory(buffer), 
                        fsharpVsLanguageService, serviceProvider) as ITagger<T>;
        }
    }
}