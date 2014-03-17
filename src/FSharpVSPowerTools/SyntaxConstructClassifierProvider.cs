using System;
using System.Diagnostics;
using System.Windows.Media;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Language.StandardClassification;
using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.SyntaxColoring;

namespace FSharpVSPowerTools
{
    static class ClassificationTypes
    {
        public const string FSharpReferenceType = "FSharp.ReferenceType";
        public const string FSharpValueType = "FSharp.ValueType";
        public const string FSharpPatternCase = "FSharp.PatternCase";
        public const string FSharpFunction = "FSharp.Function";

        public const string FSharpReferenceDefinition = PredefinedClassificationTypeNames.SymbolDefinition;
        public const string FSharpValueDefinition = PredefinedClassificationTypeNames.SymbolDefinition;
        public const string FSharpPatternCaseDefinition = "HtmlClientTemplateValue";
        public const string FSharpFunctionDefinition = "HtmlClientTemplateValue";

        [Export]
        [Name(FSharpReferenceType)]
        [BaseDefinition(FSharpReferenceDefinition)]
        internal static ClassificationTypeDefinition FSharpReferenceClassfierType = null;

        [Export]
        [Name(FSharpValueType)]
        [BaseDefinition(FSharpValueDefinition)]
        internal static ClassificationTypeDefinition FSharpValueClassfierType = null;

        [Export]
        [Name(FSharpPatternCase)]
        [BaseDefinition(FSharpPatternCaseDefinition)]
        internal static ClassificationTypeDefinition FSharpPatternCaseClassfierType = null;

        [Export]
        [Name(FSharpFunction)]
        [BaseDefinition(FSharpFunctionDefinition)]
        internal static ClassificationTypeDefinition FSharpFunctionClassfierType = null;
    }

    static class ClassificationFormats
    {
        internal static Color? GetColor(IClassificationTypeRegistryService classificationRegistry,
                IClassificationFormatMapService formatMap, string tag)
        {
            var map = formatMap.GetClassificationFormatMap(FontsAndColorsCategory.TextEditor);
            var functionType = classificationRegistry.GetClassificationType(tag);
            if (functionType == null) return null;
            var functionProp = map.GetExplicitTextProperties(functionType);
            Color? color = (functionProp.ForegroundBrush as SolidColorBrush).Color;
            return color;
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpReferenceType)]
        [Name("F# User Types")]
        [DisplayName("F# User Types")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        internal sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpReferenceTypeFormat(
                IClassificationTypeRegistryService classificationRegistry,
                IClassificationFormatMapService formatMap) 
            {
                this.ForegroundColor = GetColor(classificationRegistry, formatMap, ClassificationTypes.FSharpReferenceDefinition);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpValueType)]
        [Name("F# User Types (Value types)")]
        [DisplayName("F# User Types (Value types)")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        internal sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpValueTypeFormat(
                IClassificationTypeRegistryService classificationRegistry,
                IClassificationFormatMapService formatMap) 
            {
                this.ForegroundColor = GetColor(classificationRegistry, formatMap, ClassificationTypes.FSharpValueDefinition);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPatternCase)]
        [Name("F# Patterns")]
        [DisplayName("F# Patterns")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        internal sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpPatternCaseFormat(
                IClassificationTypeRegistryService classificationRegistry,
                IClassificationFormatMapService formatMap) 
            {
                this.ForegroundColor = GetColor(classificationRegistry, formatMap, ClassificationTypes.FSharpPatternCaseDefinition);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpFunction)]
        [Name("F# Functions")]
        [DisplayName("F# Functions")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        internal sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
            [ImportingConstructor]
            public FSharpFunctionFormat(
                IClassificationTypeRegistryService classificationRegistry,
                IClassificationFormatMapService formatMap) 
            {
                this.ForegroundColor = GetColor(classificationRegistry, formatMap, ClassificationTypes.FSharpFunctionDefinition);
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

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            return buffer.Properties.GetOrCreateSingletonProperty(() => 
                new SyntaxConstructClassifier(buffer, classificationRegistry, fsharpVsLanguageService, serviceProvider));
        }
    }
}
