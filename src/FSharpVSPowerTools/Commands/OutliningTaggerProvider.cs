using System.ComponentModel.Composition;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using Microsoft.VisualStudio.Text.Projection;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Outlining;

namespace FSharpVSPowerTools
{
    [Export(typeof(ITaggerProvider))]
    [Export(typeof(IWpfTextViewCreationListener))]
    [TagType(typeof(IOutliningRegionTag))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Structured)]
    public class OutliningTaggerProvider : ITaggerProvider, IWpfTextViewCreationListener
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ITextEditorFactoryService _textEditorFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _vsLanguageService;
        private readonly IProjectionBufferFactoryService _projectionBufferFactoryService;
        private readonly IOutliningManagerService _outliningManagerService;

        [ImportingConstructor]
        public OutliningTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ITextEditorFactoryService textEditorFactoryService,
            IProjectionBufferFactoryService projectionBufferFactoryService,
            IOutliningManagerService outliningManagerService,
            ProjectFactory projectFactory,
            VSLanguageService vsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _textEditorFactoryService = textEditorFactoryService;
            _projectionBufferFactoryService = projectionBufferFactoryService;
            _outliningManagerService = outliningManagerService;
            _projectFactory = projectFactory;
            _vsLanguageService = vsLanguageService;
        }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.OutliningEnabled)
                return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return (ITagger<T>)buffer.Properties.GetOrCreateSingletonProperty(() =>
                   new Outlining.OutliningTagger(                       
                       doc,
                       _serviceProvider,
                       _textEditorFactoryService,
                       _projectionBufferFactoryService,
                       _projectFactory,
                       _vsLanguageService));
            }

            return null;
        }

        public void TextViewCreated(IWpfTextView textView)
        {
            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.OutliningEnabled) return;
            var textBuffer = textView.TextBuffer;
            var outliningTagger = CreateTagger<IOutliningRegionTag>(textBuffer);
            outliningTagger.TagsChanged += (sender, e) =>
            {
                var fullSpan = new SnapshotSpan(textView.TextSnapshot, 0, textView.TextSnapshot.Length);
                // Ensure that first tags have been computed.
                var tags = outliningTagger.GetTags(new NormalizedSnapshotSpanCollection(fullSpan));
                var outliningManager = _outliningManagerService.GetOutliningManager(textView);
                if (outliningManager != null)
                {
                    outliningManager.CollapseAll(fullSpan, match: c => c.Tag.IsDefaultCollapsed);
                }
            };
        }
    }
}