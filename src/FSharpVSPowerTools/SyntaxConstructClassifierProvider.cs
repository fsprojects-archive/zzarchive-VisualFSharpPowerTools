using FSharpVSPowerTools.ProjectSystem;
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System;
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

    // These colors are defined differently for debugging purpose
    // They are taken from https://github.com/leddt/visualstudio-colors-solarized/blob/master/vs2013/solarized-light.vssettings
    // In the final version, we should probably set a default theme with less number of colors.

    static class ClassificationFormats
    {
        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.ReferenceType")]
        [Name("FSharp.ReferenceType")]
        [UserVisible(true)]
        sealed class FSharpReferenceTypeFormat : ClassificationFormatDefinition
        {
            public FSharpReferenceTypeFormat()
            {
                this.DisplayName = "F# User Types";
                this.ForegroundColor = Color.FromRgb(43, 145, 175);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.ValueType")]
        [Name("FSharp.ValueType")]
        [UserVisible(true)]
        sealed class FSharpValueTypeFormat : ClassificationFormatDefinition
        {
            public FSharpValueTypeFormat()
            {
                this.DisplayName = "F# User Types (Value types)";
                this.ForegroundColor = Color.FromRgb(49, 190, 239);
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.PatternCase")]
        [Name("FSharp.PatternCase")]
        [UserVisible(true)]
        sealed class FSharpPatternCaseFormat : ClassificationFormatDefinition
        {
            public FSharpPatternCaseFormat()
            {
                this.DisplayName = "F# Patterns";
                this.ForegroundColor = Colors.Purple;
            }
        }

        [Export(typeof(EditorFormatDefinition))]
        [ClassificationType(ClassificationTypeNames = "FSharp.Function")]
        [Name("FSharp.Function")]
        [UserVisible(true)]
        sealed class FSharpFunctionFormat : ClassificationFormatDefinition
        {
            public FSharpFunctionFormat()
            {
                this.DisplayName = "F# Functions";
                this.ForegroundColor = Color.FromRgb(0, 0, 160);
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
            var generalOptions = serviceProvider.GetService(typeof(GeneralOptionsPage)) as GeneralOptionsPage;
            if (!generalOptions.SyntaxColoringEnabled)
            {
                Debug.WriteLine("[Syntax Coloring] The feature is disabled in General option page.");
                return null;
            }

            return buffer.Properties.GetOrCreateSingletonProperty(() => 
                new SyntaxConstructClassifier(buffer, classificationRegistry, fsharpVsLanguageService, serviceProvider));
        }
    }
}
