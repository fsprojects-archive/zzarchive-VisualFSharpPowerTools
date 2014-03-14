using System;
using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using FSharpVSPowerTools.SyntaxColoring;
using Microsoft.VisualStudio.Text.Classification;
using System.Windows.Media;
using FSharpVSPowerTools.ProjectSystem;

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
                this.ForegroundColor = Colors.Teal;
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
