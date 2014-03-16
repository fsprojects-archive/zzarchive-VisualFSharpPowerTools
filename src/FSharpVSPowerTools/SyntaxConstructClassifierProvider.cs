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
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Language.StandardClassification;
using FSharpVSPowerTools.ProjectSystem;

namespace FSharpVSPowerTools
{
    static class ClassificationTypes
    {
        public const string FSharpReferenceType = "FSharp.ReferenceType";
        public const string FSharpValueType = "FSharp.ValueType";
        public const string FSharpPatternCase = "FSharp.PatternCase";
        public const string FSharpFunction = "FSharp.Function";

        [Export]
        [Name(FSharpReferenceType)]
        [BaseDefinition(PredefinedClassificationTypeNames.SymbolDefinition)]
        internal static ClassificationTypeDefinition FSharpReferenceClassfierType = null;

        [Export]
        [Name(FSharpValueType)]
        [BaseDefinition(PredefinedClassificationTypeNames.SymbolDefinition)]
        internal static ClassificationTypeDefinition FSharpValueClassfierType = null;

        [Export]
        [Name(FSharpPatternCase)]
        [BaseDefinition(PredefinedClassificationTypeNames.SymbolReference)]
        internal static ClassificationTypeDefinition FSharpPatternCaseClassfierType = null;

        [Export]
        [Name(FSharpFunction)]
        [BaseDefinition(PredefinedClassificationTypeNames.Identifier)]
        internal static ClassificationTypeDefinition FSharpFunctionClassfierType = null;
    }

    static class ClassificationFormats
    {
        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpReferenceType)]
        [Name("F# User Types")]
        [DisplayName("F# User Types")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpValueType)]
        [Name("F# User Types (Value types)")]
        [DisplayName("F# User Types (Value types)")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpPatternCase)]
        [Name("F# Patterns")]
        [DisplayName("F# Patterns")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = ClassificationTypes.FSharpFunction)]
        [Name("F# Functions")]
        [DisplayName("F# Functions")]
        [UserVisible(true)]
        [Order(After = Priority.Default, Before = Priority.High)]
        sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
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
