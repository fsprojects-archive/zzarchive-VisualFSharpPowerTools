using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.PlatformUI;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Windows.Media;

namespace FSharpVSPowerTools
{
    static class ClassificationTypes
    {
        public const string FSharpReferenceType = Constants.fsharpReferenceType;
        public const string FSharpValueType = Constants.fsharpValueType;
        public const string FSharpPatternCase = Constants.fsharpPatternCase;
        public const string FSharpFunction = Constants.fsharpFunction;
        public const string FSharpMutableVar = Constants.fsharpMutableVar;
        public const string FSharpQuotation = Constants.fsharpQuotation;
        public const string FSharpModule = Constants.fsharpModule;
        public const string FSharpUnused = Constants.fsharpUnused;
        public const string FSharpPrintf = Constants.fsharpPrintf;
        public const string FSharpEscaped = Constants.fsharpEscaped;
        public const string FSharpOperator = Constants.fsharpOperator;

        [Export]
        [Name(FSharpReferenceType)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpReferenceClassificationType = null;

        [Export]
        [Name(FSharpValueType)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpValueClassificationType = null;

        [Export]
        [Name(FSharpPatternCase)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpPatternCaseClassificationType = null;

        [Export]
        [Name(FSharpFunction)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpFunctionClassificationType = null;

        [Export]
        [Name(FSharpMutableVar)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpMutableVarClassificationType = null;
        
        [Export]
        [Name(FSharpQuotation)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpQuotationClassificationType = null;

        [Export]
        [Name(FSharpModule)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpModuleClassificationType = null;

        [Export]
        [Name(FSharpUnused)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpUnusedClassificationType = null;

        [Export]
        [Name(FSharpPrintf)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpPrintfClassificationType = null;

        [Export]
        [Name(FSharpEscaped)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpEscapedClassificationType = null;

        [Export]
        [Name(FSharpOperator)]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpOperatorClassificationType = null;
    }

    [Export]
    public class ClassificationColorManager 
    {
        static readonly Dictionary<string, FontColor> LightAndBlueColors = new Dictionary<string, FontColor>
            {
                { ClassificationTypes.FSharpReferenceType, new FontColor(Color.FromRgb(43, 145, 175)) },
                { ClassificationTypes.FSharpValueType, new FontColor(Color.FromRgb(43, 145, 175)) },
                { ClassificationTypes.FSharpPatternCase, new FontColor(Colors.Black) },
                { ClassificationTypes.FSharpFunction, new FontColor(Colors.Black) },
                { ClassificationTypes.FSharpMutableVar, new FontColor(Colors.Black) },
                { ClassificationTypes.FSharpQuotation, new FontColor(background: Color.FromRgb(255, 242, 223)) },
                { ClassificationTypes.FSharpModule, new FontColor(Color.FromRgb(43, 145, 175)) },
                { ClassificationTypes.FSharpUnused, new FontColor(Color.FromRgb(157, 157, 157)) },
                { ClassificationTypes.FSharpPrintf, new FontColor(Color.FromRgb(43, 145, 175)) },
                { ClassificationTypes.FSharpEscaped, new FontColor(Color.FromRgb(255, 0, 128)) },
                { ClassificationTypes.FSharpOperator, new FontColor(Colors.Black) }
            };

        static readonly Dictionary<string, FontColor> DarkColors = new Dictionary<string, FontColor>
            {
                { ClassificationTypes.FSharpReferenceType, new FontColor(Color.FromRgb(78, 201, 176)) },
                { ClassificationTypes.FSharpValueType, new FontColor(Color.FromRgb(78, 201, 176)) },
                { ClassificationTypes.FSharpPatternCase, new FontColor(Color.FromRgb(220, 220, 220)) },
                { ClassificationTypes.FSharpFunction, new FontColor(Color.FromRgb(220, 220, 220)) },
                { ClassificationTypes.FSharpMutableVar, new FontColor(Color.FromRgb(220, 220, 220)) },
                { ClassificationTypes.FSharpQuotation, new FontColor(background: Color.FromRgb(98, 58, 0)) },
                { ClassificationTypes.FSharpModule, new FontColor(Color.FromRgb(78, 201, 176)) },
                { ClassificationTypes.FSharpUnused, new FontColor(Color.FromRgb(155, 155, 155)) },
                { ClassificationTypes.FSharpPrintf, new FontColor(Color.FromRgb(78, 220, 176)) },
                { ClassificationTypes.FSharpEscaped, new FontColor(Color.FromRgb(190, 0, 94)) },
                { ClassificationTypes.FSharpOperator, new FontColor(Color.FromRgb(220, 220, 220)) }
            };

        private ThemeManager _themeManager;
        private IClassificationFormatMapService _classificationFormatMapService;
        private IClassificationTypeRegistryService _classificationTypeRegistry;

        private VisualStudioTheme _currentTheme;

        [ImportingConstructor]
        public ClassificationColorManager(
            ThemeManager themeManager,
            IClassificationFormatMapService classificationFormatMapService,
            IClassificationTypeRegistryService classificationTypeRegistry)
        {
            _themeManager = themeManager;
            _classificationFormatMapService = classificationFormatMapService;
            _classificationTypeRegistry = classificationTypeRegistry;

            // Theme changed event may fire even though the same theme is still in use.
            // We save a current theme and skip color updates in these cases. 
            _currentTheme = _themeManager.GetCurrentTheme();
        }

        public FontColor GetDefaultColors(string category) 
        {
            bool success;
            FontColor color;
            switch (_currentTheme)
            {
                case VisualStudioTheme.Dark:
                    color = new FontColor(Color.FromRgb(220, 220, 220), Color.FromRgb(30, 30, 30));
                    success = DarkColors.TryGetValue(category, out color);
                    if (!success)
                        LoggingModule.logWarningMessage(() => String.Format("Theme manager can't read colors correctly from {0} theme.", _currentTheme));
                    return color;

                case VisualStudioTheme.Light:
                case VisualStudioTheme.Blue:
                default:
                    color = new FontColor(Colors.Black, Colors.White);
                    success = LightAndBlueColors.TryGetValue(category, out color);
                    if (!success)
                        LoggingModule.logWarningMessage(() => String.Format("Theme manager can't read colors correctly from {0} theme.", _currentTheme));
                    return color;
            }
        }

        public void UpdateColors()
        {
            var newTheme = _themeManager.GetCurrentTheme();

            if (newTheme != VisualStudioTheme.Unknown && newTheme != _currentTheme)
            {
                _currentTheme = newTheme;

                var colors = newTheme == VisualStudioTheme.Dark ? DarkColors : LightAndBlueColors;
                var formatMap = _classificationFormatMapService.GetClassificationFormatMap(category: "text");
                    
                try
                {
                    formatMap.BeginBatchUpdate();
                    foreach (var pair in colors)
                    {
                        string type = pair.Key;
                        FontColor color = pair.Value;

                        var classificationType = _classificationTypeRegistry.GetClassificationType(type);
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
                }
                finally
                {
                    formatMap.EndBatchUpdate();
                }
            }
        }
    }

    static class ClassificationFormats
    {
        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpReferenceType)]
        [Name(ClassificationTypes.FSharpReferenceType)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)] 
        internal sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpReferenceTypeFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Types";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpReferenceType);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpValueType)]
        [Name(ClassificationTypes.FSharpValueType)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)] 
        internal sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpValueTypeFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Value Types";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpValueType);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPatternCase)]
        [Name(ClassificationTypes.FSharpPatternCase)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)] 
        internal sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpPatternCaseFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Patterns";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpPatternCase);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpFunction)]
        [Name(ClassificationTypes.FSharpFunction)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)] 
        internal sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpFunctionFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Functions / Methods";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpFunction);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpMutableVar)]
        [Name(ClassificationTypes.FSharpMutableVar)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)] 
        internal sealed class FSharpMutableVarFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpMutableVarFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Mutable Variables / Reference Cells";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpMutableVar);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpQuotation)]
        [Name(ClassificationTypes.FSharpQuotation)]
        [UserVisible(true)]
        [Order(Before = PredefinedClassificationTypeNames.String)]
        internal sealed class FSharpQuotationFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpQuotationFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Quotations";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpQuotation);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
                ForegroundCustomizable = false;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpModule)]
        [Name(ClassificationTypes.FSharpModule)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)]
        internal sealed class FSharpModuleFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpModuleFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Modules";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpModule);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpUnused)]
        [Name(ClassificationTypes.FSharpUnused)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)]
        internal sealed class FSharpUnusedFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpUnusedFormat(ClassificationColorManager colorManager)
            {
                DisplayName = "F# Unused Declarations";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpUnused);
                ForegroundColor = colors.Foreground;
                BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPrintf)]
        [Name(ClassificationTypes.FSharpPrintf)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)]
        internal sealed class FSharpPrintfFormat : ClassificationFormatDefinition
        {
             [ImportingConstructor]
             public FSharpPrintfFormat(ClassificationColorManager colorManager)
             {
                 DisplayName = "F# Printf Format";
                 var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpPrintf);
                 ForegroundColor = colors.Foreground;
                 BackgroundColor = colors.Background;
             }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpEscaped)]
        [Name(ClassificationTypes.FSharpEscaped)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)]
        internal sealed class FSharpEscapedFormat : ClassificationFormatDefinition
        {
             [ImportingConstructor]
             public FSharpEscapedFormat(ClassificationColorManager colorManager)
             {
                 DisplayName = "F# Escaped Characters";
                 var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpEscaped);
                 ForegroundColor = colors.Foreground;
                 BackgroundColor = colors.Background;
             }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpOperator)]
        [Name(ClassificationTypes.FSharpOperator)]
        [UserVisible(true)]
        [Order(After = PredefinedClassificationTypeNames.String)]
        internal sealed class FSharpOperatorFormat : ClassificationFormatDefinition
        {
             [ImportingConstructor]
             public FSharpOperatorFormat(ClassificationColorManager colorManager)
             {
                 DisplayName = "F# Operators";
                 var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpOperator);
                 ForegroundColor = colors.Foreground;
                 BackgroundColor = colors.Background;
             }
        }
    }

    [Export(typeof(ITaggerProvider))]
    [TagType(typeof(UnusedDeclarationTag))]
    [Export(typeof(IClassifierProvider))]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class SyntaxConstructClassifierProvider : ITaggerProvider, IClassifierProvider, IDisposable
    { 
        private readonly ClassificationColorManager _classificationColorManager;
        private readonly IClassificationTypeRegistryService _classificationRegistry;
        private readonly ITextDocumentFactoryService _textDocumentFactoryService;
        private readonly VSLanguageService _fsharpVsLanguageService;
        private readonly ProjectFactory _projectFactory;
        private readonly IServiceProvider _serviceProvider;
        private readonly IGeneralOptions _generalOptions;
        

        [ImportingConstructor]
        public SyntaxConstructClassifierProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ClassificationColorManager classificationColorManager,
            IClassificationTypeRegistryService classificationRegistry,
            ITextDocumentFactoryService textDocumentFactoryService,
            VSLanguageService fsharpVsLanguageService,
            ProjectFactory projectFactory,
            IGeneralOptions generalOptions
            
            )
        {
            _serviceProvider = serviceProvider;
            _classificationColorManager = classificationColorManager;
            _classificationRegistry = classificationRegistry;
            _textDocumentFactoryService = textDocumentFactoryService;
            _fsharpVsLanguageService = fsharpVsLanguageService;
            _projectFactory = projectFactory;
            _generalOptions = generalOptions;

            // Receive notification for Visual Studio theme change
            VSColorTheme.ThemeChanged += UpdateTheme;
        }

        private void UpdateTheme(EventArgs e)
        {
            _classificationColorManager.UpdateColors();
        }

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            //var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (_generalOptions == null || !_generalOptions.SyntaxColoringEnabled) return null;

            bool includeUnusedReferences = _generalOptions.UnusedReferencesEnabled;
            bool includeUnusedOpens = _generalOptions.UnusedOpensEnabled;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return buffer.Properties.GetOrCreateSingletonProperty(
                    () => new SyntaxConstructClassifier(doc, buffer, _classificationRegistry, _fsharpVsLanguageService,
                                    _serviceProvider, _projectFactory, includeUnusedReferences, includeUnusedOpens));
            }

            return null;
        }

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            return GetClassifier(buffer) as ITagger<T>;
        }

        public void Dispose()
        {
            VSColorTheme.ThemeChanged -= UpdateTheme;
        }
    }
}
