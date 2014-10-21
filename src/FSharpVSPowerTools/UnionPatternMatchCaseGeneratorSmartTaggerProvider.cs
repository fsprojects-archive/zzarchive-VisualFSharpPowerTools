using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.Refactoring;
using FSharpVSPowerTools.ProjectSystem;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(UnionPatternMatchCaseGeneratorSmartTag))]
    public class UnionPatternMatchCaseGeneratorSmartTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        [Import]
        internal ITextUndoHistoryRegistry undoHistoryRegistry = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide the smart tagger on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Utils.GetGeneralOptionsPage(serviceProvider);
            if (generalOptions == null || !generalOptions.UnionPatternMatchCaseGenerationEnabled) return null;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                if (doc != null)
                    return new UnionPatternMatchCaseGeneratorSmartTagger(doc, textView,
                                undoHistoryRegistry.RegisterHistory(buffer),
                                fsharpVsLanguageService, serviceProvider,
                                projectFactory, Utils.GetDefaultMemberBody(serviceProvider)) as ITagger<T>;
            }
            
            return null;
        }
    }
}
