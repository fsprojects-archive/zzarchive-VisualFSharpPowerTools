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
    }

    [Export]
    public class ClassificationColorManager 
    {
        private readonly IDictionary<VisualStudioTheme, IDictionary<string, Color>> themeColors =
            new Dictionary<VisualStudioTheme, IDictionary<string, Color>>();

        private VisualStudioTheme lastTheme = VisualStudioTheme.Unknown;

        public ClassificationColorManager()
        {
            // Light/Blue theme colors
            var lightAndBlueColors = new Dictionary<string, Color>
            {
                { ClassificationTypes.FSharpReferenceType, Color.FromRgb(43, 145, 175) },
                { ClassificationTypes.FSharpValueType, Color.FromRgb(43, 145, 175) },
                { ClassificationTypes.FSharpPatternCase, Colors.Black },
                { ClassificationTypes.FSharpFunction, Colors.Black },
                { ClassificationTypes.FSharpMutableVar, Colors.Black },
                { ClassificationTypes.FSharpQuotation, Color.FromRgb(255, 242, 223) }
            };

            themeColors.Add(VisualStudioTheme.Blue, lightAndBlueColors);
            themeColors.Add(VisualStudioTheme.Light, lightAndBlueColors);
            themeColors.Add(VisualStudioTheme.Unknown, lightAndBlueColors);

            // Dark theme colors
            var darkColors = new Dictionary<string, Color>
            {
                { ClassificationTypes.FSharpReferenceType, Color.FromRgb(78, 201, 176) },
                { ClassificationTypes.FSharpValueType, Color.FromRgb(78, 201, 176) },
                { ClassificationTypes.FSharpPatternCase, Color.FromRgb(220, 220, 220) },
                { ClassificationTypes.FSharpFunction, Color.FromRgb(220, 220, 220) },
                { ClassificationTypes.FSharpMutableVar, Color.FromRgb(220, 220, 220) },
                { ClassificationTypes.FSharpQuotation, Color.FromRgb(255, 242, 223) }
            };

            themeColors.Add(VisualStudioTheme.Dark, darkColors);
        }

        [Import]
        private ThemeManager themeManager = null;

        [Import]
        private IClassificationFormatMapService classificationFormatMapService = null;

        [Import]
        private IClassificationTypeRegistryService classificationTypeRegistry = null;

        public Color GetDefaultColor(string category) 
        {
            var currentTheme = themeManager.GetCurrentTheme();

            bool success;
            Color color;
            switch (currentTheme)
            {
                case VisualStudioTheme.Dark:
                    color = Color.FromRgb(220, 220, 220);
                    success = themeColors[currentTheme].TryGetValue(category, out color);
                    if (!success) Debug.WriteLine("Classification theme manager can't read colors correctly.");
                    return color;

                case VisualStudioTheme.Light:
                case VisualStudioTheme.Blue:
                default:
                    color = Colors.Black;
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

                IDictionary<string, Color> colors = themeColors[currentTheme];
                IClassificationFormatMap formatMap = classificationFormatMapService.GetClassificationFormatMap(category: "text");
                    
                try
                {
                    formatMap.BeginBatchUpdate();
                    foreach (var pair in colors)
                    {
                        string type = pair.Key;
                        Color color = pair.Value;

                        var classificationType = classificationTypeRegistry.GetClassificationType(type);
                        var oldProp = formatMap.GetTextProperties(classificationType);
                        var newProp = TextFormattingRunProperties.CreateTextFormattingRunProperties(oldProp.Typeface,
                            oldProp.FontRenderingEmSize, color);

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
        sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpReferenceTypeFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Types";
                this.ForegroundColor = colorManager.GetDefaultColor(ClassificationTypes.FSharpReferenceType);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpValueType)]
        [Name(ClassificationTypes.FSharpValueType)]
        [UserVisible(true)]
        sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpValueTypeFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Value Types";
                this.ForegroundColor = colorManager.GetDefaultColor(ClassificationTypes.FSharpValueType);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPatternCase)]
        [Name(ClassificationTypes.FSharpPatternCase)]
        [UserVisible(true)]
        sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpPatternCaseFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Patterns";
                this.ForegroundColor = colorManager.GetDefaultColor(ClassificationTypes.FSharpPatternCase);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpFunction)]
        [Name(ClassificationTypes.FSharpFunction)]
        [UserVisible(true)]
        sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpFunctionFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Functions / Methods";
                this.ForegroundColor = colorManager.GetDefaultColor(ClassificationTypes.FSharpFunction);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpMutableVar)]
        [Name(ClassificationTypes.FSharpMutableVar)]
        [UserVisible(true)]
        sealed class FSharpMutableVarFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpMutableVarFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Mutable Variables / Reference Cells";
                this.ForegroundColor = colorManager.GetDefaultColor(ClassificationTypes.FSharpMutableVar);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpQuotation)]
        [Name(ClassificationTypes.FSharpQuotation)]
        [UserVisible(true)]
        sealed class FSharpQuotationFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpQuotationFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Quotations";
                this.BackgroundColor = colorManager.GetDefaultColor(ClassificationTypes.FSharpQuotation);
                this.ForegroundCustomizable = false;
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
        private Logger logger = null;
        
        [Import]
        private ITextDocumentFactoryService textDocumentFactoryService = null;

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.SyntaxColoringEnabled)
            {
                logger.Log(LogType.Information, "Syntax Coloring feature is disabled in General option page.");
                return null;
            }

            ITextDocument doc;
            if (textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
                return buffer.Properties.GetOrCreateSingletonProperty(() =>
                    new SyntaxConstructClassifier(doc, classificationRegistry, fsharpVsLanguageService, serviceProvider));

            return null;
        }
    }
}
