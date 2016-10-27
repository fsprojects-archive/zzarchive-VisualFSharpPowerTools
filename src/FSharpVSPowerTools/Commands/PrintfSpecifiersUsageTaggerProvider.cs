using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using System;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.PlatformUI;
using FSharp.Editing.VisualStudio.Symbol;
using FSharp.Editing.VisualStudio.ProjectSystem;
using FSharp.Editing.VisualStudio;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(PrintfSpecifiersUsageTag))]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class PrintfSpecifiersUsageTaggerProvider : IViewTaggerProvider, IDisposable
    {
        readonly IServiceProvider _serviceProvider;
        readonly ITextDocumentFactoryService _textDocumentFactoryService;
        readonly ProjectFactory _projectFactory;
        readonly VSLanguageService _fsharpVsLanguageService;
        readonly PrintfColorManager _printfColorManager;

        [ImportingConstructor]
        public PrintfSpecifiersUsageTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService,
            PrintfColorManager printfColorManager)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
            _printfColorManager = printfColorManager;

            VSColorTheme.ThemeChanged += UpdateTheme;
            _printfColorManager.UpdateColors(force: true);
        }

        void UpdateTheme(EventArgs e)
        {
            _printfColorManager.UpdateColors(force: false);
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.HighlightPrintfUsageEnabled) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return textView.Properties.GetOrCreateSingletonProperty(
                    () => new PrintfSpecifiersUsageTagger(doc, textView, _fsharpVsLanguageService, _projectFactory)) as ITagger<T>;
            }

            return null;
        }

        public void Dispose()
        {
            VSColorTheme.ThemeChanged -= UpdateTheme;
        }
    }

    [Export]
    public class PrintfColorManager
    {
        static readonly Color LightThemeColor = Color.FromRgb(245, 222, 179);
        static readonly Color DarkThemeColor = Color.FromRgb(0, 77, 77);
        
        VisualStudioTheme _currentTheme;
        DateTime _lastThemeChange;
        ThemeManager _themeManager;
        IEditorFormatMapService _editorFormatMapService;

        [ImportingConstructor]
        public PrintfColorManager(ThemeManager themeManager, IEditorFormatMapService editorFormatMapService) 
        {
            _themeManager = themeManager;
            _editorFormatMapService = editorFormatMapService;

            _currentTheme = _themeManager.GetCurrentTheme();
            _lastThemeChange = DateTime.MinValue;
        }

        public Color GetDefaultColor()
        {
            return _currentTheme == VisualStudioTheme.Dark ? DarkThemeColor : LightThemeColor;
        }

        public virtual void UpdateColors(bool force)
        {
            var newTheme = _themeManager.GetCurrentTheme();

            // Multiple theme change events are fired in rapid succession after the theme was changed.
            // All of them must be processed to properly update the color scheme.
            if (newTheme != VisualStudioTheme.Unknown &&
                (newTheme != _currentTheme || (DateTime.Now - _lastThemeChange).TotalSeconds < 10 || force))
            {
                _currentTheme = newTheme;
                _lastThemeChange = DateTime.Now;

                var formatMap = _editorFormatMapService.GetEditorFormatMap(category: "text");
                try
                {
                    formatMap.BeginBatchUpdate();
                    var dict = formatMap.GetProperties(Constants.fsharpPrintfTagType);
                    dict["BackgroundColor"] = GetDefaultColor();
                    formatMap.SetProperties(Constants.fsharpPrintfTagType, dict);
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
            BackgroundColor = colorManager.GetDefaultColor();
            DisplayName = "F# Highlight Printf";
            ZOrder = 5;
        }
    }
}