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
        [Export]
        [Name("FSharp.ReferenceType")]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpReferenceClassfierType = null;

        [Export]
        [Name("FSharp.ValueType")]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpValueClassfierType = null;

        [Export]
        [Name("FSharp.TypeParameter")]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpTypeParameterClassfierType = null;

        [Export]
        [Name("FSharp.PatternCase")]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpPatternCaseClassfierType = null;

        [Export]
        [Name("FSharp.Function")]
        [BaseDefinition("identifier")]
        internal static ClassificationTypeDefinition FSharpFunctionClassfierType = null;

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
            var lightAndBlueColors = new Dictionary<string, Color>()
            {
                {"FSharp.ReferenceType", Color.FromRgb(43, 145, 175)},
                {"FSharp.ValueType", Color.FromRgb(43, 145, 175)},
                {"FSharp.PatternCase", Colors.Black},
                {"FSharp.Function", Colors.Olive}
            };

            themeColors.Add(VisualStudioTheme.Blue, lightAndBlueColors);
            themeColors.Add(VisualStudioTheme.Light, lightAndBlueColors);
            themeColors.Add(VisualStudioTheme.Unknown, lightAndBlueColors);

            // Dark theme colors
            var darkColors = new Dictionary<string, Color>()
            {
                {"FSharp.ReferenceType", Color.FromRgb(78, 201, 176)},
                {"FSharp.ValueType", Color.FromRgb(78, 201, 176)},
                {"FSharp.PatternCase", Color.FromRgb(220, 220, 220)},
                {"FSharp.Function", Color.FromRgb(255, 128, 64)}
            };

            themeColors.Add(VisualStudioTheme.Dark, darkColors);
        }

        [Import]
        private IClassificationFormatMapService classificationFormatMapService = null;

        [Import]
        private IClassificationTypeRegistryService classificationTypeRegistry = null;

        public Color GetDefaultColor(VisualStudioTheme currentTheme, string category) 
        {   
            bool result = false;
            var color = Colors.Black;
            switch (currentTheme)
            {
                case VisualStudioTheme.Dark:
                    color = Colors.White;
                    result = themeColors[currentTheme].TryGetValue(category, out color);
                    if (!result) Debug.WriteLine("Classification theme manager can't read colors correctly.");
                    return color;
                case VisualStudioTheme.Light:
                case VisualStudioTheme.Blue:
                default:
                    color = Colors.Black;
                    result = themeColors[currentTheme].TryGetValue(category, out color);
                    if (!result) Debug.WriteLine("Classification theme manager can't read colors correctly.");
                    return color;
            }
        }

        public void UpdateColors(VisualStudioTheme currentTheme)
        {
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
        [ClassificationType(ClassificationTypeNames = "FSharp.ReferenceType")]
        [Name("FSharp.ReferenceType")]
        [UserVisible(true)]
        sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpReferenceTypeFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# User Types";
                this.ForegroundColor = colorManager.GetDefaultColor(VisualStudioTheme.Blue, "FSharp.ReferenceType");
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.ValueType")]
        [Name("FSharp.ValueType")]
        [UserVisible(true)]
        sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpValueTypeFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# User Types (Value types)";
                // TODO: extract string constant
                this.ForegroundColor = colorManager.GetDefaultColor(VisualStudioTheme.Blue, "FSharp.ValueType");
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.PatternCase")]
        [Name("FSharp.PatternCase")]
        [UserVisible(true)]
        sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpPatternCaseFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Patterns";
                // TODO: extract string constant
                this.ForegroundColor = colorManager.GetDefaultColor(VisualStudioTheme.Blue, "FSharp.PatternCase");
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.Function")]
        [Name("FSharp.Function")]
        [UserVisible(true)]
        sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpFunctionFormat(ClassificationColorManager colorManager)
            {
                this.DisplayName = "F# Functions";
                // TODO: extract string constant
                this.ForegroundColor = colorManager.GetDefaultColor(VisualStudioTheme.Blue, "FSharp.Function");
            }
        }
    }

    [Export(typeof(IClassifierProvider))]
    [ContentType("F#")]
    public class SyntaxConstructClassifierProvider : IClassifierProvider
    { 
        [Import]
        private IClassificationTypeRegistryService classificationRegistry = null;

        [Import]
        private ClassificationColorManager colorManager = null;

        [Import]
        private ThemeManager themeManager = null;

        [Import(typeof(SVsServiceProvider))]
        private IServiceProvider serviceProvider = null;

        [Import]
        private VSLanguageService fsharpVsLanguageService = null;

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.SyntaxColoringEnabled)
            {
                Debug.WriteLine("[Syntax Coloring] The feature is disabled in General option page.");
                return null;
            }

            colorManager.UpdateColors(themeManager.GetCurrentTheme());

            return buffer.Properties.GetOrCreateSingletonProperty(() =>
            {
                return new SyntaxConstructClassifier(buffer, classificationRegistry, fsharpVsLanguageService,
                    serviceProvider);
            });
        }
    }
}
