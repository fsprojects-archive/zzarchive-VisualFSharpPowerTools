using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell.Interop;
using FSharp.Editing.VisualStudio.CodeGeneration;
using FSharp.Editing.VisualStudio.ProjectSystem;
using FSharp.Editing.VisualStudio;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(UnionPatternMatchCaseGeneratorSmartTag))]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class UnionPatternMatchCaseGeneratorSmartTaggerProvider : IViewTaggerProvider
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly IVSOpenDocumentsTracker _openDocumentsTracker;
        private readonly ITextUndoHistoryRegistry _undoHistoryRegistry;
        
        [ImportingConstructor]
        public UnionPatternMatchCaseGeneratorSmartTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ITextUndoHistoryRegistry undoHistoryRegistry,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService,
            IVSOpenDocumentsTracker openDocumentsTracker)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _undoHistoryRegistry = undoHistoryRegistry;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
            _openDocumentsTracker = openDocumentsTracker;
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide the smart tagger on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.UnionPatternMatchCaseGenerationEnabled) return null;
            var codeGenOptions = Setting.getCodeGenerationOptions(_serviceProvider);
            if (codeGenOptions == null) return null;

            var dte = _serviceProvider.GetService(typeof(SDTE)) as EnvDTE.DTE;
            var vsVersion = VisualStudioVersionModule.fromDTEVersion(dte.Version);
            if (vsVersion >= VisualStudioVersion.VS2015) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                var generator = 
                    new UnionPatternMatchCaseGenerator(
                        doc, 
                        textView,
                        _undoHistoryRegistry.RegisterHistory(buffer),
                        _fsharpVsLanguageService,
                        _projectFactory, 
                        Setting.getDefaultMemberBody(codeGenOptions),
                        _openDocumentsTracker);

                return new UnionPatternMatchCaseGeneratorSmartTagger(buffer, generator) as ITagger<T>;
            }
            
            return null;
        }
    }
}
