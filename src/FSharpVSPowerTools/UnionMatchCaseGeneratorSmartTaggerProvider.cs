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
    [TagType(typeof(UnionMatchCaseGeneratorSmartTag))]
    public class UnionMatchCaseGeneratorSmartTaggerProvider : IViewTaggerProvider
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
            // Only provide the smart tagger on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            // TODO reactivate check
            if (generalOptions != null)// && generalOptions.RecordStubGenerationEnabled)
            {
                return new UnionMatchCaseGeneratorSmartTagger(textView, buffer, editorOptionsFactory,
                    undoHistoryRegistry.RegisterHistory(buffer),
                    fsharpVsLanguageService, serviceProvider) as ITagger<T>;
            }
            else
                return null;
        }
    }
}
