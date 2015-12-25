﻿using FSharpVSPowerTools.Linting;
using FSharpVSPowerTools.ProjectSystem;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;
using System.Windows.Media;

namespace FSharpVSPowerTools
{
    static class FormatDefinition
    {
        [Export(typeof(EditorFormatDefinition))]
        [Name(Constants.LintTagErrorType)]
        [Order(After = Priority.High)]
        [UserVisible(true)]
        internal class LintFormatDefinition : EditorFormatDefinition
        {
            public LintFormatDefinition()
            {
                this.ForegroundColor = Colors.Orange;
                this.BackgroundCustomizable = false;
                this.DisplayName = "F# Lint";
            }
        }

        [Export(typeof(ErrorTypeDefinition))]
        [Name(Constants.LintTagErrorType)]
        [DisplayName(Constants.LintTagErrorType)]
        internal static ErrorTypeDefinition LintErrorTypeDefinition = null;
    }

    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(LintTag))]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class LintTaggerProvider : IViewTaggerProvider
    {
        readonly IServiceProvider _serviceProvider;
        readonly ITextDocumentFactoryService _textDocumentFactoryService;
        readonly ProjectFactory _projectFactory;
        readonly VSLanguageService _fsharpVsLanguageService;
        readonly IOpenDocumentsTracker _openDocumentTracker;
        private readonly IGeneralOptions _generalOptions;
        private readonly ILintOptions _lintOptions;

        [ImportingConstructor]
        public LintTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService,
            IOpenDocumentsTracker openDocumentTracker,
        IGeneralOptions generalOptions,
        ILintOptions lintOptions
  
  
  
  
        )
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
            _openDocumentTracker = openDocumentTracker;
            _generalOptions = generalOptions;
            _lintOptions = lintOptions;
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (textView.TextBuffer != buffer) return null;

            //var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.LinterEnabled) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return buffer.Properties.GetOrCreateSingletonProperty(
                    () => new LintTagger(doc, _fsharpVsLanguageService, _serviceProvider, _projectFactory, _openDocumentTracker,_lintOptions) as ITagger<T>);
            }

            return null;
        }
    }
}