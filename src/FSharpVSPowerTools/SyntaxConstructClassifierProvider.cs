using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Utilities;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows.Media;

namespace FSharpVSPowerTools
{
    static class ClassificationTypes
    {
        public const string FSharpReferenceType = "FSharp.ReferenceType";
        public const string FSharpValueType = "FSharp.ValueType";
        public const string FSharpPatternCase = "FSharp.PatternCase";
        public const string FSharpFunction = "FSharp.Function";
        public const string FSharpMutableVar = "FSharp.MutableVar";
        public const string FSharpQuotation = "FSharp.Quotation";
        public const string FSharpModule = "FSharp.Module";

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
    }

    public class FontColor
    {
        public readonly Color? Foreground;
        public readonly Color? Background;

        public FontColor(Color? foreground = null, Color? background = null)
        {
            Foreground = foreground;
            Background = background;
        }
    }

    [Export]
    public class ClassificationColorManager 
    {
        private readonly IDictionary<VisualStudioTheme, IDictionary<string, FontColor>> themeColors =
            new Dictionary<VisualStudioTheme, IDictionary<string, FontColor>>();

        private VisualStudioTheme lastTheme = VisualStudioTheme.Unknown;

        public ClassificationColorManager()
        {
            // Light/Blue theme colors
            var lightAndBlueColors = new Dictionary<string, FontColor>
            {
                { ClassificationTypes.FSharpReferenceType, new FontColor(Color.FromRgb(43, 145, 175)) },
                { ClassificationTypes.FSharpValueType, new FontColor(Color.FromRgb(43, 145, 175)) },
                { ClassificationTypes.FSharpPatternCase, new FontColor(Colors.Black) },
                { ClassificationTypes.FSharpFunction, new FontColor(Colors.Black) },
                { ClassificationTypes.FSharpMutableVar, new FontColor(Colors.Black) },
                { ClassificationTypes.FSharpQuotation, new FontColor(background: Color.FromRgb(255, 242, 223)) },
                { ClassificationTypes.FSharpModule, new FontColor(Color.FromRgb(43, 145, 175)) }
            };

            themeColors.Add(VisualStudioTheme.Blue, lightAndBlueColors);
            themeColors.Add(VisualStudioTheme.Light, lightAndBlueColors);
            themeColors.Add(VisualStudioTheme.Unknown, lightAndBlueColors);

            // Dark theme colors
            var darkColors = new Dictionary<string, FontColor>
            {
                { ClassificationTypes.FSharpReferenceType, new FontColor(Color.FromRgb(78, 201, 176)) },
                { ClassificationTypes.FSharpValueType, new FontColor(Color.FromRgb(78, 201, 176)) },
                { ClassificationTypes.FSharpPatternCase, new FontColor(Color.FromRgb(220, 220, 220)) },
                { ClassificationTypes.FSharpFunction, new FontColor(Color.FromRgb(220, 220, 220)) },
                { ClassificationTypes.FSharpMutableVar, new FontColor(Color.FromRgb(220, 220, 220)) },
                { ClassificationTypes.FSharpQuotation, new FontColor(background: Color.FromRgb(98, 58, 0)) },
                { ClassificationTypes.FSharpModule, new FontColor(Color.FromRgb(78, 201, 176)) },
            };

            themeColors.Add(VisualStudioTheme.Dark, darkColors);
        }

        [Import]
        private ThemeManager themeManager = null;

        [Import]
        private IClassificationFormatMapService classificationFormatMapService = null;

        [Import]
        private IClassificationTypeRegistryService classificationTypeRegistry = null;

        public FontColor GetDefaultColors(string category) 
        {
            var currentTheme = themeManager.GetCurrentTheme();

            bool success;
            FontColor color;
            switch (currentTheme)
            {
                case VisualStudioTheme.Dark:
                    color = new FontColor(Color.FromRgb(220, 220, 220), Color.FromRgb(30, 30, 30));
                    success = themeColors[currentTheme].TryGetValue(category, out color);
                    if (!success) Debug.WriteLine("Classification theme manager can't read colors correctly.");
                    return color;

                case VisualStudioTheme.Light:
                case VisualStudioTheme.Blue:
                default:
                    color = new FontColor(Colors.Black, Colors.White);
                    success = themeColors[currentTheme].TryGetValue(category, out color);
                    if (!success) Debug.WriteLine("Classification theme manager can't read colors correctly.");
                    return color;
            }
        }

        public void UpdateColors()
        {
            var currentTheme = themeManager.GetCurrentTheme();

            if (currentTheme != VisualStudioTheme.Unknown && currentTheme != lastTheme)
            {
                lastTheme = currentTheme;

                var colors = themeColors[currentTheme];
                var formatMap = classificationFormatMapService.GetClassificationFormatMap(category: "text");
                    
                try
                {
                    formatMap.BeginBatchUpdate();
                    foreach (var pair in colors)
                    {
                        string type = pair.Key;
                        FontColor color = pair.Value;

                        var classificationType = classificationTypeRegistry.GetClassificationType(type);
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
        internal sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpReferenceTypeFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Types";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpReferenceType);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpValueType)]
        [Name(ClassificationTypes.FSharpValueType)]
        [UserVisible(true)]
        internal sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpValueTypeFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Value Types";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpValueType);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPatternCase)]
        [Name(ClassificationTypes.FSharpPatternCase)]
        [UserVisible(true)]
        internal sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpPatternCaseFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Patterns";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpPatternCase);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpFunction)]
        [Name(ClassificationTypes.FSharpFunction)]
        [UserVisible(true)]
        internal sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpFunctionFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Functions / Methods";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpFunction);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpMutableVar)]
        [Name(ClassificationTypes.FSharpMutableVar)]
        [UserVisible(true)]
        internal sealed class FSharpMutableVarFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpMutableVarFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Mutable Variables / Reference Cells";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpMutableVar);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpQuotation)]
        [Name(ClassificationTypes.FSharpQuotation)]
        [UserVisible(true)]
        internal sealed class FSharpQuotationFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpQuotationFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Quotations";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpQuotation);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
                this.ForegroundCustomizable = false;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpModule)]
        [Name(ClassificationTypes.FSharpModule)]
        [UserVisible(true)]
        internal sealed class FSharpModuleFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpModuleFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Modules";
                var colors = colorManager.GetDefaultColors(ClassificationTypes.FSharpModule);
                this.ForegroundColor = colors.Foreground;
                this.BackgroundColor = colors.Background;
            }
        }
    }

    [Export(typeof(IClassifierProvider))]
    [ContentType("F#")]
    public class SyntaxConstructClassifierProvider : IClassifierProvider
    { 
        [Import]
        private IClassificationTypeRegistryService classificationRegistry = null;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider = null;

        [Import]
        private VSLanguageService fsharpVsLanguageService = null;

        [Import]
        private ITextDocumentFactoryService textDocumentFactoryService = null;

        [Import(typeof(ProjectFactory))]
        private ProjectFactory projectFactory = null;

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.SyntaxColoringEnabled) return null;

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
                return buffer.Properties.GetOrCreateSingletonProperty(() =>
                    new SyntaxConstructClassifier(doc, classificationRegistry, fsharpVsLanguageService, serviceProvider,
                                                  projectFactory));

            return null;
        }
    }
}
