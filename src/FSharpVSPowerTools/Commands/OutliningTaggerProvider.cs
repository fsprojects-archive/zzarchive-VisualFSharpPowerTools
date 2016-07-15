using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using Microsoft.VisualStudio.Text.Projection;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Outlining;
using FSharp.Editing.VisualStudio.ProjectSystem;
using FSharp.Editing.VisualStudio;
using FSharp.Editing.VisualStudio.Outlining;

namespace FSharpVSPowerTools.Outlining
{
    [Export(typeof(ITaggerProvider))]
    [Export(typeof(IWpfTextViewCreationListener))]
    [TagType(typeof(IOutliningRegionTag))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Structured)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class OutliningTaggerProvider : ITaggerProvider, IWpfTextViewCreationListener
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ITextEditorFactoryService _textEditorFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _vsLanguageService;
        private readonly IProjectionBufferFactoryService _projectionBufferFactoryService;
        private readonly IOutliningManagerService _outliningManagerService;
        private readonly IVSOpenDocumentsTracker _openDocumentsTracker;

        [ImportingConstructor]
        public OutliningTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ITextEditorFactoryService textEditorFactoryService,
            IProjectionBufferFactoryService projectionBufferFactoryService,
            IOutliningManagerService outliningManagerService,
            ProjectFactory projectFactory,
            VSLanguageService vsLanguageService,
            IVSOpenDocumentsTracker openDocumentsTracker)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _textEditorFactoryService = textEditorFactoryService;
            _projectionBufferFactoryService = projectionBufferFactoryService;
            _outliningManagerService = outliningManagerService;
            _projectFactory = projectFactory;
            _vsLanguageService = vsLanguageService;
            _openDocumentsTracker = openDocumentsTracker;
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
                   new OutliningTagger(                       
                       doc,
                       _serviceProvider,
                       _textEditorFactoryService,
                       _projectionBufferFactoryService,
                       _projectFactory,
                       _vsLanguageService,
                       _openDocumentsTracker));
            }

            return null;
        }

        public void TextViewCreated(IWpfTextView textView)
        {
            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.OutliningEnabled) return;
            
            var textBuffer = textView.TextBuffer;
            var outliningTagger = CreateTagger<IOutliningRegionTag>(textBuffer);
            bool isFirstOutlining = true;

            outliningTagger.TagsChanged += (sender, e) =>
            {
                if (isFirstOutlining)
                {
                    // Try to collapse tags once at view opening
                    isFirstOutlining = false;

                    var outliningManager = textView.Properties.GetOrCreateSingletonProperty(
                        () => _outliningManagerService.GetOutliningManager(textView));
                    if (outliningManager != null)
                    {
                        var fullSpan = new SnapshotSpan(textBuffer.CurrentSnapshot, 0, textBuffer.CurrentSnapshot.Length);
                        outliningManager.CollapseAll(fullSpan, match: c => c.Tag.IsDefaultCollapsed);
                    }
                }
            };
        }
    }
}