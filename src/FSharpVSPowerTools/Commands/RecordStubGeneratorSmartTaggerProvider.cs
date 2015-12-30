﻿using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.Refactoring;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Shell.Interop;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(RecordStubGeneratorSmartTag))]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class RecordStubGeneratorSmartTaggerProvider : IViewTaggerProvider
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly ProjectFactory _projectFactory;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly IOpenDocumentsTracker _openDocumentsTracker;
        private readonly ITextUndoHistoryRegistry _undoHistoryRegistry;
        private readonly IGeneralOptions _generalOptions;
        private readonly ICodeGenerationOptions _codeGenOptions;


        [ImportingConstructor]
        public RecordStubGeneratorSmartTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ITextUndoHistoryRegistry undoHistoryRegistry,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService,
            IOpenDocumentsTracker openDocumentsTracker,
            IGeneralOptions generalOptions  ,
            ICodeGenerationOptions codeGenOptions)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _undoHistoryRegistry = undoHistoryRegistry;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
            _openDocumentsTracker = openDocumentsTracker;
            _generalOptions = generalOptions;
            _codeGenOptions = codeGenOptions;
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide the smart tagger on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            //var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.GenerateRecordStubEnabled) return null;
            //var codeGenOptions = Setting.getCodeGenerationOptions(_serviceProvider);
            if (_codeGenOptions == null) return null;

            var dte = _serviceProvider.GetService(typeof(SDTE)) as EnvDTE.DTE;
            var vsVersion = VisualStudioVersionModule.fromDTEVersion(dte.Version);
            if (vsVersion == VisualStudioVersion.VS2015) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                var generator = new RecordStubGenerator(doc, textView,
                                _undoHistoryRegistry.RegisterHistory(buffer),
                                _fsharpVsLanguageService, _serviceProvider,
                                _projectFactory, _codeGenOptions.DefaultBody,
                                _openDocumentsTracker);
                return new RecordStubGeneratorSmartTagger(buffer, generator) as ITagger<T>;
            }
            
            return null;
        }
    }
}
