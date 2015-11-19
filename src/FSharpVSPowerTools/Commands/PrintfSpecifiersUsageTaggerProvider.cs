using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.ProjectSystem;
using System;
using FSharpVSPowerTools.PrintfSpecifiersHighlightUsage;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Formatting;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(PrintfSpecifiersUsageTag))]
    public class PrintfSpecifiersUsageTaggerProvider : IViewTaggerProvider
    {
        readonly IServiceProvider _serviceProvider;
        readonly ITextDocumentFactoryService _textDocumentFactoryService;
        readonly ProjectFactory _projectFactory;
        readonly VSLanguageService _fsharpVsLanguageService;

        [ImportingConstructor]
        public PrintfSpecifiersUsageTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.HighlightUsageEnabled) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return buffer.Properties.GetOrCreateSingletonProperty(
                    () => new PrintfSpecifiersUsageTagger(doc, textView, _fsharpVsLanguageService, _serviceProvider, _projectFactory)) as ITagger<T>;
            }

            return null;
        }
    }

    [Export]
    public class PrintfColorManager
    {
        static readonly FontColor LightThemeColor = new FontColor(null, Color.FromRgb(245, 222, 179));
        static readonly FontColor DarkThemeColor = new FontColor(null, Color.FromRgb(145, 122, 110));
        VisualStudioTheme lastTheme = VisualStudioTheme.Unknown;

        [Import]
        private ThemeManager themeManager = null;

        [Import]
        private IClassificationFormatMapService classificationFormatMapService = null;

        [Import]
        private IClassificationTypeRegistryService classificationTypeRegistry = null;

        public FontColor GetDefaultColor()
        {
            return themeManager.GetCurrentTheme() == VisualStudioTheme.Dark ? DarkThemeColor : LightThemeColor;
        }

        public void UpdateColors()
        {
            var currentTheme = themeManager.GetCurrentTheme();

            if (currentTheme != VisualStudioTheme.Unknown && currentTheme != lastTheme)
            {
                lastTheme = currentTheme;
                var color = GetDefaultColor();
                var formatMap = classificationFormatMapService.GetClassificationFormatMap(category: "text");

                try
                {
                    formatMap.BeginBatchUpdate();
                    var classificationType = classificationTypeRegistry.GetClassificationType(Constants.fsharpPrintfTagType);
                    var oldProp = formatMap.GetTextProperties(classificationType);

                    var foregroundBrush =
                        color.Foreground == null
                            ? null
                            : new SolidColorBrush(color.Foreground.Value);

                    var backgroundBrush =
                        color.Background == null
                            ? null
                            : new SolidColorBrush(color.Background.Value);

                    var newProp = TextFormattingRunProperties.CreateTextFormattingRunProperties(
                        foregroundBrush, backgroundBrush, oldProp.Typeface, null, null, oldProp.TextDecorations,
                        oldProp.TextEffects, oldProp.CultureInfo);

                    formatMap.SetTextProperties(classificationType, newProp);
                }
                finally
                {
                    formatMap.EndBatchUpdate();
                }
            }
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [Name(Constants.fsharpPrintfTagType)]
    [UserVisible(true)]
    public class HighlightIdentifierFormatDefinition: MarkerFormatDefinition
    {
        [ImportingConstructor]
        public HighlightIdentifierFormatDefinition(PrintfColorManager colorManager) 
        {
            var color = colorManager.GetDefaultColor();
            BackgroundColor = color.Background;
            ForegroundColor = color.Foreground;
            DisplayName = "F# Highlight Printf";
            ZOrder = 5;
        }
    }
}