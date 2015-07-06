using FSharpVSPowerTools.Linting;
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
                this.ForegroundColor = Color.FromRgb(155, 155, 255);
                this.BackgroundCustomizable = false;
                this.DisplayName = "F# Lint";
            }
        }

        [Export(typeof(ErrorTypeDefinition))]
        [Name(Constants.LintTagErrorType)]
        [DisplayName(Constants.LintTagErrorType)]
        static ErrorTypeDefinition LintErrorTypeDefinition = null;
    }

    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(LintTag))]
    public class LintTaggerProvider : IViewTaggerProvider
    {
        [Import]
        internal VSLanguageService fsharpVsLanguageService = null;

        [Import]
        internal ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import(typeof(SVsServiceProvider))]
        internal IServiceProvider serviceProvider = null;

        [Import]
        internal ProjectFactory projectFactory = null;

        private static readonly Type serviceType = typeof(LintTagger);

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(serviceProvider);
            if (generalOptions == null || !generalOptions.LinterEnabled) return null;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return buffer.Properties.GetOrCreateSingletonProperty(serviceType, 
                    () => new LintTagger(doc, textView, fsharpVsLanguageService, serviceProvider, projectFactory) as ITagger<T>);
            }

            return null;
        }
    }
}