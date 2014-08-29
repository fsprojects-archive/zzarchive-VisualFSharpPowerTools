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
    [TagType(typeof(RecordStubGeneratorSmartTag))]
    public class RecordStubGeneratorSmartTaggerProvider : IViewTaggerProvider
    {
        [Import]
        private VSLanguageService fsharpVsLanguageService = null;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider = null;

        [Import]
        private ITextUndoHistoryRegistry undoHistoryRegistry = null;

        [Import(typeof(ProjectFactory))]
        private ProjectFactory projectFactory = null;

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide the smart tagger on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Utils.GetGeneralOptionsPage(serviceProvider);
            if (generalOptions != null && generalOptions.GenerateRecordStubEnabled)
            {
                return new RecordStubGeneratorSmartTagger(textView, buffer,
                    undoHistoryRegistry.RegisterHistory(buffer),
                    fsharpVsLanguageService, serviceProvider,
                    projectFactory, Utils.GetDefaultMemberBody(serviceProvider)) as ITagger<T>;
            }
            else
                return null;
        }
    }
}
